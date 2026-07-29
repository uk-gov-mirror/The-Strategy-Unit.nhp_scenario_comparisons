mod_distribution_beeswarm_plot <- function(beeswarm_data) {
  min_x_value <- min(beeswarm_data[["value"]], beeswarm_data[["baseline"]])
  summary_tbl <- beeswarm_data |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), unique),
      .by = "scenario"
    )
  beeswarm_data |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]], 1)) +
    ggbeeswarm::geom_quasirandom(
      ggplot2::aes(colour = .data[["scenario"]]),
      orientation = "y",
      size = 3,
      alpha = 0.5
    ) +
    ggplot2::scale_colour_manual(values = c("red", "blue")) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data[["baseline"]]),
      data = summary_tbl,
      colour = "grey50",
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(
        xintercept = .data[["principal"]],
        colour = .data[["scenario"]]
      ),
      data = summary_tbl,
      linetype = "dashed",
      linewidth = 1.2
    ) +
    ggplot2::expand_limits(x = min_x_value) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(8),
      labels = scales::label_comma(),
      expand = ggplot2::expansion(0.01)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.15)) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Segoe UI", size = 12),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.text = ggplot2::element_text(face = "bold", hjust = 0.1),
      legend.position = "bottom",
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(rows = dplyr::vars(.data[["scenario"]]))
}


mod_distribution_ecdf_plot <- function(ecdf_plot_data) {
  ecdf_plot_data |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]])) +
    ggplot2::stat_ecdf(alpha = 0.8) +
    ggplot2::geom_segment(
      data = percentiles,
      ggplot2::aes(x = .data$p10, xend = .data$p10, y = 0, yend = 0.1),
      linetype = "dashed",
      show.legend = FALSE
    ) +
    # ggplot2::geom_segment(data = percentiles,
    #                       ggplot2::aes(x = p10[2], xend = p10[2], y = 0, yend = 0.1),
    #                       linetype = "dashed",
    #                       #color = "blue",
    #                      show.legend = FALSE) +
    ggplot2::geom_segment(
      data = percentiles,
      ggplot2::aes(x = .data$p90, xend = .data$p90, y = 0, yend = 0.9),
      linetype = "dashed",
      #color = "red",
      show.legend = FALSE
    ) +
    # ggplot2::geom_segment(data = percentiles,
    #                       ggplot2::aes(x = p90[2], xend = p90[2], y = 0, yend = 0.9),
    #                       linetype = "dashed",
    #                       #color = "blue",
    #                      show.legend = FALSE) +
    ggplot2::geom_segment(
      data = percentiles,
      ggplot2::aes(
        x = .data$principal,
        xend = .data$principal,
        y = 0,
        yend = 1
      ),
      linetype = "solid",
      #color = "red",
      show.legend = FALSE
    ) +
    # ggplot2::geom_segment(data = percentiles,
    #                       ggplot2::aes(x = principal[2], xend = principal[2], y = 0, yend = 0.9),
    #                       linetype = "solid",
    #                       #color = "blue",
    #                       show.legend = FALSE) +
    # Add horizontal dashed lines at 10% and 90%
    ggplot2::geom_segment(
      data = percentiles,
      ggplot2::aes(x = -Inf, xend = .data$p10, y = 0.1, yend = 0.1),
      linetype = "dashed",
      color = "grey20",
      show.legend = FALSE
    ) +
    # geom_segment(data = percentiles,
    #              aes(x = -Inf, xend = p10, y = 0.1, yend = 0.1),
    #              linetype = "dashed",
    #              color = "blue",
    #              show.legend = FALSE) +
    ggplot2::geom_segment(
      data = percentiles,
      ggplot2::aes(x = -Inf, xend = .data$p90, y = 0.9, yend = 0.9),
      linetype = "dashed",
      color = "grey20",
      show.legend = FALSE
    ) +
    # geom_segment(data = percentiles,
    #              aes(x = p10, xend = p90, y = 0.9, yend = 0.9),
    #              linetype = "dashed",
    #              color = "blue",
    #              show.legend = FALSE) +
    # ggplot2::geom_segment(data = percentiles,
    #                       ggplot2::aes(x = -Inf, xend = principal, y = 0.9, yend = 0.9),
    #              linetype = "dashed",
    #              color = "grey20",
    #              show.legend = FALSE) +
    # geom_segment(data = percentiles,
    #              aes(x = principal, xend = principal, y = 0.9, yend = 0.9),
    #              linetype = "dashed",
    #              color = "blue",
    #              show.legend = FALSE) +
    ggplot2::geom_vline(xintercept = percentiles$baseline, colour = "dimgrey") +
    # Add text labels at the baseline positions
    ggplot2::geom_text(
      ggplot2::aes(x = .data$baseline, y = 0.5, label = "Baseline"),
      colour = "dimgrey",
      angle = 90,
      hjust = 0,
      vjust = -0.1
    ) +
    ggplot2::labs(
      x = "Value",
      y = "Percentage of model runs",
      title = "S-curve (empirical cumulative distribution function)"
    ) +
    ggplot2::expand_limits(
      x = ifelse(show_origin, 0, percentiles$baseline[[1]])
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(10),
      labels = scales::comma,
      expand = c(0.002, 0),
      limits = c(min_x, NA)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(seq(0, 1, 0.1)),
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(
      values = c("red", "blue"),
      labels = get_label_map(data, id_col = .data$scenario)
    ) +
    ggplot2::theme(
      legend.text = ggtext::element_markdown(
        family = "Segoe UI",
        size = 12,
        color = "black",
        hjust = 0.5,
        lineheight = 1.5
      ),
      legend.position = "bottom"
    )
}


mod_distribution_ecdf_plot <- function(ecdf_plot_data) {
  min_x_value <- min(ecdf_plot_data[["value"]], ecdf_plot_data[["baseline"]])
  summary_tbl <- ecdf_plot_data |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), unique),
      .by = "scenario"
    )
  ecdf_fn <- stats::ecdf(ecdf_plot_data[["value"]])
  quantiles <- c(0.1, 0.9)
  x_quantiles <- stats::quantile(ecdf_fn, quantiles)
  x_vals <- sort(ecdf_plot_data[["value"]])
  y_vals <- sort(ecdf_fn(ecdf_plot_data[["value"]]))
  principal_diffs <- abs(principal_value - x_vals)
  principal_match_value <- which.min(principal_diffs)
  principal_pct <- y_vals[[principal_match_value]]
  min_x_value <- ifelse(show_zero, 0, min(baseline_value, min_value))
  plot_red <- "red"
  plot_blue <- "cornflowerblue"

  line_guides <- tibble::tibble(
    x_start = c(rep(min_x_value, 3), x_quantiles, principal_value),
    x_end = rep(c(x_quantiles, principal_value), 2),
    y_start = c(quantiles, principal_pct, rep(0, 3)),
    y_end = rep(c(quantiles, principal_pct), 2),
    colour = rep(c(rep(plot_blue, 2), plot_red), 2)
  )
  interim_plot <- tibble::tibble(x = x_vals, y = y_vals) |>
    ggplot2::ggplot(ggplot2::aes(.data[["x"]], .data[["y"]]))
  interim_plot +
    ggplot2::geom_step(colour = "grey50", linewidth = 1.2) +
    ggplot2::geom_segment(
      data = line_guides,
      ggplot2::aes(
        x = .data[["x_start"]],
        y = .data[["y_start"]],
        xend = .data[["x_end"]],
        yend = .data[["y_end"]]
      ),
      # colour removed as an explicit aesthetic because plotly doesn't respect
      # `show.legend = FALSE`
      colour = line_guides[["colour"]],
      linetype = "dashed",
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      xintercept = baseline_value,
      colour = "grey50",
      linewidth = 1.2
    ) +
    ggplot2::expand_limits(x = min_x_value) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(8),
      labels = scales::label_comma(),
      expand = ggplot2::expansion(0.01)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.1),
      labels = scales::percent,
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::labs(y = "Percentage of model runs") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_blank()
    )
}
