create_summary_bar_chart <- function(data, activity_type, measure) {
  title_text <- glue::glue("{pod} {activity_type} - Summary Comparison")
  bar_cols <- c("#f9bf07", "#686f73")
  data |>
    dplyr::filter(
      .data[["activity_type"]] == .env[["activity_type"]],
      .data[["measure"]] == .env[["measure"]]
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(ggplot2::aes(.data[["principal"]], .data[["pod_name"]]))
    ) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[["id"]]), position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma_format) +
    ggplot2::scale_fill_manual(name = "Scenario", values = bar_cols) +
    ggplot2::labs(title = title_text, x = measure, y = "Point of delivery") +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Segoe UI", size = 12),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.text = ggplot2::element_text(face = "bold", hjust = 0.1),
      legend.position = "bottom"
    )
}


create_los_bar_chart <- function(data, pod, measure) {
  title_text <- glue::glue("{pod} {measure} - Length of Stay Comparison")
  bar_cols <- c("#f9bf07", "#686f73")
  data |>
    dplyr::filter(
      .data[["pod_name"]] == .env[["pod"]],
      .data[["measure"]] == .env[["measure"]]
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data[["principal"]], .data[["los_group"]])) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[["id"]]), position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma_format) +
    ggplot2::scale_fill_manual(name = "Scenario", values = bar_cols) +
    ggplot2::labs(title = title_text, x = measure, y = "Length of stay group") +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Segoe UI", size = 12),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.text = ggplot2::element_text(face = "bold", hjust = 0.1),
      legend.position = "bottom"
    )
}


create_distribution_bar_chart <- function(data, pod) {
  title_tx <- glue::glue("{pod} - Principal projection (with p10 and p90 bar)")
  bar_cols <- c("#f9bf07", "#686f73")

  data |>
    dplyr::filter(.data[["pod_name"]] == .env[["pod"]]) |>
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
    ggplot2::scale_x_continuous(labels = scales::comma_format) +
    ggplot2::scale_fill_manual(name = "Scenario", values = bar_cols) +
    ggplot2::labs(title = title_tx, x = "Principal projection", y = "Measure") +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Segoe UI", size = 12),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.text = ggplot2::element_text(face = "bold", hjust = 0.1),
      legend.position = "bottom"
    )
}
