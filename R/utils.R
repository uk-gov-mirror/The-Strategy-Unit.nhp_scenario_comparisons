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


pull_unique <- \(df, col) unique(df[[col]])
