#' Prepare a lookup table for all current TPMAs
#'
#' @returns A 4-column tibble, with columns `strategy`, `activity_type`,
#'  `change_factor` and `tpma_label`
#' @export
get_tpma_label_lookup <- function() {
  csv_data <- possibly_read_tpmas_lookup()
  msg <- "Unable to read TPMA lookup table from GitHub"
  azkit::check_that(csv_data, is_not_null, msg)
  csv_data |>
    dplyr::filter(dplyr::if_any("active_to", is.na)) |>
    dplyr::select(!"active_to") |>
    dplyr::mutate(
      tpma_label = glue::glue("{tpma_name} ({tpma_subtype})"),
      dplyr::across("tpma_type", \(x) tolower(sub(" ", "_", x))),
      dplyr::across("activity_type", convert_activity_type),
      .keep = "unused"
    ) |>
    dplyr::rename(change_factor = "tpma_type", strategy = "tpma_variable")
}
