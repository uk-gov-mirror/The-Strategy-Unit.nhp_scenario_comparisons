create_beeswarm_chart <- function(beeswarm_data, activity_type, measure) {
  beeswarm_data <- beeswarm_data |>
    dplyr::filter()
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

  ggplot2::labs(
    y = input$filter2,
    title = glue::glue(
      "{input$filter1} {input$filter2} - Distribution of Model Runs"
    )
  )
}


mod_distribution_ecdf_plot <- function(ecdf_plot_data) {
  min_x_value <- min(ecdf_plot_data[["value"]], ecdf_plot_data[["baseline"]])
  quantiles <- c(0.1, 0.9)
  summary_tbl <- ecdf_plot_data |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), unique),
      ecdf_fn = list(stats::ecdf(.data[["value"]])),
      x_quantiles = list(stats::quantile(.data[["ecdf_fn"]], quantiles)),
      x_vals = list(sort(.data[["value"]])),
      y_vals = list(sort(.data[["ecdf_fn"]])),
      .by = "scenario"
    ) |>
    dplyr::mutate(
      principal_match_value = which.min(abs(
        .data[["principal"]] - .data[["x_vals"]]
      )),
      principal_pct = .data[["y_vals"]][[.data[["principal_match_value"]]]],
      .by = "scenario"
    ) |>
    dplyr::select(!c("ecdf_fn", "principal_match_value"))
  # ecdf_fn <- stats::ecdf(ecdf_plot_data[["value"]])
  # x_quantiles <- stats::quantile(ecdf_fn, quantiles)
  # x_vals <- sort(ecdf_plot_data[["value"]])
  # y_vals <- sort(ecdf_fn(ecdf_plot_data[["value"]]))
  # principal_diffs <- abs(principal_value - x_vals)
  # principal_match_value <- which.min(principal_diffs)
  # principal_pct <- y_vals[[principal_match_value]]

  line_guides <- tibble::tibble(
    x_start = c(rep(min_x_value, 3), x_quantiles, principal),
    x_end = rep(c(x_quantiles, principal), 2),
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
        yend = .data[["y_end"]],
        colour = line_guides[["colour"]],
      ),
      linetype = "dashed",
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      data = summary_tbl,
      ggplot2::aes(xintercept = .data[["baseline_value"]]),
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
      labels = scales::label_percent,
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::labs(y = "Percentage of model runs") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_blank()
    )

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
      labels = scales::label_comma(),
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
