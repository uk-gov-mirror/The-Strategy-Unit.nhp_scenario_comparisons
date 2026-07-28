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
