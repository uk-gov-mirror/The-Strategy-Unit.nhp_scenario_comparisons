#' Concatenate Scheme Name and Code
#' @param scheme_code Character. A focus scheme's three-character ODS code.
#' @param lookup_path Character. The file path to the CSV lookup of scheme names
#'     and codes.
#' @param as_filestring Logical. Express as a string with punctuation removed,
#'    hyphen-delimited and in lowercase? Used to build filepath.
#' @return Character string.
#' @export
#' @examples \dontrun{construct_scheme_name("XYZ")}
make_scheme_name <- function(
  scheme_code,
  lookup_path = "supporting_data/scheme-lookup.csv",
  as_filestring = FALSE
) {
  scheme_string <- readr::read_csv(lookup_path, show_col_types = FALSE) |>
    dplyr::filter(.data$scheme == scheme_code) |>
    dplyr::mutate(
      hosp_site_scheme = glue::glue("{hosp_site} ({scheme})"),
      .keep = "none"
    ) |>
    dplyr::pull()

  if (as_filestring) {
    scheme_string <- scheme_string |>
      stringr::str_remove_all("[:punct:]") |>
      stringr::str_to_lower() |>
      stringr::str_replace_all(" ", "-")
  }

  scheme_string
}

#' Add scenario column safely, handling NULL results
#' @param data Data frame or NULL from get_model_run_distribution
#' @param scenario_name Character. Name of the scenario to add
#' @return Tibble with scenario column, or empty tibble if input is NULL
#' @export
add_scenario_safe <- function(data, scenario_name) {
  if (is.null(data)) {
    return(tibble::tibble(
      value = numeric(),
      variant = character(),
      scenario = scenario_name
    ))
  }
  data |> dplyr::mutate(scenario = scenario_name)
}


core_chart_theme <- function() {
  ggplot2::theme(
    text = ggplot2::element_text(family = "Segoe UI", size = 12),
    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.title.position = "plot",
    legend.text = ggplot2::element_text(face = "bold", hjust = 0.1),
    legend.position = "bottom",
    strip.clip = "off"
  )
}


create_measure_label <- \(x) uppercase_init(sub("dd", "d D", sub("_", "-", x)))


bold_red <- \(x) paste0("<p style='color:red;'><strong>", x, "</strong></p>")

# fmt: skip
create_dt <- function(...) {
  purrr::partial(DT::datatable, rownames = FALSE, escape = FALSE,
    options = list(
      paging = FALSE, searching = FALSE, ordering = FALSE, dom = "t"
    ))(...)
}

swap_names <- function(vec) {
  stopifnot(rlang::is_named(vec))
  rlang::set_names(names(vec), vec)
}

tidy_dttm <- \(x) sub("Z", "", sub("T", " ", x))

get_pods <- \(x) x[["default"]][["pod"]]

is_not_null <- \(x) !is.null(x)

pull_unique <- \(df, col) unique(df[[col]])

uppercase_init <- \(x) sub("^([[:alpha:]])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)


error_on_zero_rows <- function(df) {
  if (nrow(df) == 0) {
    stop("Table has zero rows")
  } else {
    df
  }
}

require_rows <- \(x) {
  shiny::req(x)
  shiny::req(nrow(x) > 0)
  x
}
