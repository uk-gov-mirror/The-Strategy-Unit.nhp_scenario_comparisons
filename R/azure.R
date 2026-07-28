#' List All NHP Results Data Files and their Metadata
#'
#' @returns A data.frame. As many rows as there are files in `container`. As
#'   many columns as there are metadata elements, plus the file path.
#' @examples \dontrun{get_container() |> get_nhp_result_sets()}
#' @export
get_nhp_result_sets <- function() {
  allowed_datasets <- get_user_allowed_datasets()
  container_results <- Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS")
  cont <- get_container(container_results)
  get_metadata <- purrr::partial(AzureStor::get_storage_metadata, object = cont)

  metadata_cache <- cachem::cache_disk(".cache")
  metadata <- memoise::memoise(get_metadata, cache = metadata_cache)

  cat("loading result sets filenames\n")
  files <- cont |>
    AzureStor::list_blobs("prod", info = "all", recursive = TRUE) |>
    dplyr::filter(!.data[["isdir"]]) |>
    dplyr::filter(!stringr::str_detect(name, "prod/dev")) |> #remove dev runs
    dplyr::mutate(
      version_number = as.numeric(
        stringr::str_replace_all(name, ".*prod/v([0-9]+\\.[0-9]+)/.*", "\\1")
      )
    ) |>
    dplyr::filter(version_number >= 3.1) |> #keep models on v3.1 or later only
    purrr::pluck("name") |>
    purrr::set_names()

  cat("getting metadata\n")
  files <- files |>
    purrr::map(metadata, .progress = "Initialising..") |>
    dplyr::bind_rows(.id = "file") |>
    # filter to available datasets for this user
    dplyr::semi_join(ds, by = dplyr::join_by("dataset")) |>
    dplyr::mutate(
      dplyr::across("viewable", as.logical)
    )
  cat("returning result sets\n")

  files
}


get_result_sets <- function(
  allowed_datasets = get_user_allowed_datasets(NULL)
) {
  token <- azkit::get_auth_token()
  if (!token$validate()) {
    token$refresh()
  }

  app_url <- config::get("app_url")
  build_app_url <- function(
    app_version,
    dataset,
    model_run_id,
    outputs_app_uri
  ) {
    glue::glue(app_url)
  }

  azkit::read_azure_table(
    table_name = Sys.getenv("AZ_TABLE_NAME"),
    table_endpoint = Sys.getenv("AZ_TABLE_EP"),
    token = token,
    filter = "status eq 'complete'"
  ) |>
    dplyr::filter(.data[["dataset"]] %in% allowed_datasets) |>
    dplyr::mutate(
      dplyr::across(
        "app_version",
        \(.x) {
          ifelse(
            stringr::str_starts(.x, "v"),
            stringr::str_replace(.x, "\\.", "-"),
            "dev"
          )
        }
      ),
      dplyr::across(
        "outputs_app_uri",
        \(.x) {
          build_app_url(
            .data[["app_version"]],
            .data[["dataset"]],
            .data[["RowKey"]],
            .x
          )
        }
      )
    )
}


get_user_allowed_datasets <- function(groups = NULL) {
  codes <- names(yyjsonr::read_json_file("supporting_data/datasets.json"))
  nhp_stub <- "^nhp_(national|icb|provider)_"
  if (is.null(groups) || any(c("nhp_devs", "nhp_power_users") %in% groups)) {
    c("synthetic", codes)
  } else {
    allowed <- sub(nhp_stub, "", grepv(nhp_stub, groups))
    c("synthetic", intersect(codes, allowed))
  }
}


filter_result_sets <- function(result_sets, ds, sc, cd) {
  result_sets |>
    shiny::req() |>
    dplyr::filter(
      .data[["dataset"]] == ds,
      .data[["scenario"]] == sc,
      .data[["create_datetime"]] == cd
    ) |>
    require_rows()
}


#' Unzip, Read and Parse an NHP Results File
#'
#' @param container_results Name of a blob_container/storage_container object
#'     that stores results files.
#' @param file Character. The path to a file in the named `container`.
#'
#' @details Assumes you've connected to the container that holds NHP results.
#'
#' @return A nested list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' container <- azkit::get_container()
#' result_sets <- container |> get_nhp_result_sets()
#' file <- result_sets |> dplyr::slice(1) |> dplyr::pull(file)
#' r <- container |> get_nhp_results(file)
#' }
get_nhp_results <- function(
  container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"),
  blob_url = Sys.getenv("AZ_STORAGE_EP"),
  file
) {
  container <- get_container(container = container_results, endpoint = blob_url)

  AzureStor::download_blob(container, file, NULL) |>
    memDecompress(type = "gzip") |>
    yyjsonr::read_json_raw(
      yyjsonr::opts_read_json(
        obj_of_arrs_to_df = FALSE
      )
    ) |>
    parse_results() # applies patch logic dependent on app_version in params
}

get_baseline_and_projections <- function(r_trust) {
  r_trust[["results"]][["default"]] |>
    dplyr::group_by(measure, pod, sitetret) |>
    dplyr::summarise(
      baseline = sum(baseline),
      principal = sum(principal),
      lwr_ci = sum(lwr_ci),
      upr_ci = sum(upr_ci)
    )
}

get_stepcounts <- function(r_trust) {
  r_trust[["results"]][["step_counts"]]
}

get_losgroup <- function(r_trust) {
  los_group_is_null <- is.null(r_trust[["results"]][["los_group"]])

  if (los_group_is_null) {
    # tretspef+los_group renamed from tretspef_raw+los_group in v4.0
    r_trust <- r_trust[["results"]][["tretspef+los_group"]]
  } else {
    r_trust <- r_trust[["results"]][["los_group"]]
  }

  r_trust
}
