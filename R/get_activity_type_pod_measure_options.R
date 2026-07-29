get_apm_lookup <- function() {
  yaml_data <- possibly_read_pods_lookup()
  msg <- "Unable to read POD lookup file from GitHub"
  azkit::check_that(yaml_data, is_not_null, msg)
  yaml_data |>
    purrr::pluck("default", "pod_measures") |>
    purrr::map(list_to_tbl) |>
    purrr::list_rbind(names_to = "activity_type") |>
    dplyr::mutate(dplyr::across(!"measure", forcats::fct_inorder))
}


#' Helper function to extract the required data fields from a list (from YAML)
#' @keywords internal
list_to_tbl <- function(lst) {
  tibble::tibble(
    activity_type_label = lst[["name"]],
    pod = names(lst[["pods"]]),
    pod_label = purrr::map_chr(lst[["pods"]], "name"),
    measure = purrr::map(lst[["pods"]], "measures")
  ) |>
    tidyr::unnest_longer("measure")
}
