create_summary_bar_chart <- function(summary_data, activity_type) {
  title_text <- glue::glue("{activity_type} - Summary Comparison")
  fill_colours <- c("#f9bf07", "#686f73")

  summary_data |>
    dplyr::filter(.data[["activity_type_label"]] == .env[["activity_type"]]) |>
    ggplot2::ggplot(ggplot2::aes(.data[["principal"]], .data[["pod_label"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = title_text, x = NULL, y = "Point of delivery") +
    core_chart_theme()
}


create_los_bar_chart <- function(los_data, pod, measure) {
  title_text <- glue::glue("{pod} {measure} - Length of Stay Comparison")
  fill_colours <- c("#f9bf07", "#686f73")

  los_data |>
    dplyr::filter(
      .data[["pod_label"]] == .env[["pod"]],
      .data[["measure"]] == .env[["measure"]]
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data[["principal"]], .data[["los_group"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = title_text, x = measure, y = "Length of stay group") +
    core_chart_theme()
}


create_principal_pi_bar_chart <- function(data, at, pod) {
  pod_lab <- glue::glue("{at} {pod}")
  titl <- glue::glue("{pod_lab} - Principal projection (with p10 and p90 bar)")
  fill_colours <- c("#f9bf07", "#686f73")

  data |>
    dplyr::filter(.data[["pod_label"]] == .env[["pod_lab"]]) |>
    ggplot2::ggplot(ggplot2::aes(.data[["principal"]], .data[["measure"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[["lower"]], xmax = .data[["upper"]]),
      width = 0.6,
      position = ggplot2::position_dodge(0.7)
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = titl, x = "Principal projection", y = "Measure") +
    core_chart_theme()
}


create_waterfall_chart <- function(waterfall_data, activity_type, measure) {
  waterfall_data |>
    dplyr::mutate(
      measure_label = create_measure_label(.data[["measure"]]),
      dplyr::across("activity_type_label", \(x) sub("s$", "", x))
    ) |>
    dplyr::filter(
      .data[["activity_type_label"]] == .env[["activity_type"]],
      .data[["measure_label"]] == .env[["measure"]]
    ) |>
    reskit::make_overall_cf_plot() +
    core_chart_theme() +
    ggplot2::facet_grid(rows = dplyr::vars(.data[["scenario"]]))
}


create_impact_chart <- function(impact_data, cf, at, measure) {
  cf_label <- ifelse(cf = "efficiencies", "Efficiencies", "Activity Avoidance")
  title_2 <- glue::glue("Impact of Individual {cf_label} TPMA Assumptions")
  title_text <- glue::glue("{at} {pod} - {title_2}")
  fill_colours <- c("#f9bf07", "#686f73")

  impact_data |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) sub("s$", "", x)),
      dplyr::across("tpma_label", \(x) stringr::str_wrap(x, 60)),
      measure_label = create_measure_label(.data[["measure"]])
    ) |>
    dplyr::filter(
      .data[["activity_type_label"]] == .env[["at"]],
      .data[["measure_label"]] == .env[["measure"]],
      .data[["value"]] < 0
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]], .data[["tpma_label"]])) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data[["scenario"]]),
      position = "dodge"
    ) +
    ggplot2::scale_fill_manual(name = "Scenario", values = fill_colours) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.5, 0.5))) +
    ggplot2::labs(title = title_text, x = measure, y = "TPMA") +
    core_chart_theme()
}
