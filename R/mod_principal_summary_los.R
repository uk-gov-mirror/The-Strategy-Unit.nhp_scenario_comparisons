mod_principal_summary_los_data <- function(r, sites, measure) {
  pods <- reskit::get_principal_pods()

  has_tretspef_los <- !is.null(r$results[["tretspef+los_group"]])

  if (has_tretspef_los) {
    los_data <- r$results[["tretspef+los_group"]] |>
      dplyr::select(-"tretspef")
  }

  if (!has_tretspef_los) {
    los_data <- r$results[["los_group"]]
  }

  summary_los <- los_data |>
    dplyr::filter(.data[["measure"]] == .env[["measure"]]) |>
    trust_site_aggregation(sites) |>
    dplyr::inner_join(pods, by = "pod") |>
    dplyr::mutate(
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::select(
      "pod_name",
      "los_group",
      "baseline",
      "principal",
      "change",
      "change_pcnt"
    ) |>
    dplyr::arrange("pod_name", "los_group")

  summary_los[order(summary_los$pod_name, summary_los$los_group), ]
}
