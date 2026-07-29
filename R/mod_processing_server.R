mod_processing_server <- function(
  id,
  results_metadata_tbl,
  selections,
  scenario_selections,
  trigger,
  local_data_flag
) {
  shiny::moduleServer(id, function(input, output, session) {
    processed <- shiny::eventReactive(trigger(), {
      shiny::req(
        !identical(selections$main_scenario, selections$comparator_scenario) &&
          selections$main_scenario$start_year ==
            selections$comparator_scenario$start_year &&
          selections$main_scenario$end_year ==
            selections$comparator_scenario$end_year &&
          selections$main_scenario$app_version ==
            selections$comparator_scenario$app_version
      )

      shiny::withProgress(message = "Fetching scenarios...", value = 0, {
        selected <- scenario_selections()

        shiny::req(
          scenario_selections()$scenario_1,
          scenario_selections()$scenario_1_runtime,
          scenario_selections()$scenario_2,
          scenario_selections()$scenario_2_runtime
        )

        scenario_1_dir <- results_metadata_tbl |>
          dplyr::filter(
            .data[["scenario"]] == selected$scenario_1,
            .data[["create_datetime"]] == selected$scenario_1_runtime
          ) |>
          dplyr::pull("aggregated_results_path")

        scenario_2_dir <- results_metadata_tbl |>
          dplyr::filter(
            .data[["scenario"]] == selected$scenario_2,
            .data[["create_datetime"]] == selected$scenario_2_runtime
          ) |>
          dplyr::pull("aggregated_results_path")

        shiny::incProgress(0.1)

        shiny::req(all(lengths(c(scenario_1_dir, scenario_2_dir)) == 1))

        if (local_data_flag) {
          rds_paths <- dir("rds", "\\.rds$", full.names = TRUE)
          scenario_1_dir_short <- sub("^agg[^/]+", "rds", scenario_1_dir)
          scenario_2_dir_short <- sub("^agg[^/]+", "rds", scenario_2_dir)
          rds_path1 <- grepv(paste0("^", scenario_1_dir_short), rds_paths)
          rds_path2 <- grepv(paste0("^", scenario_2_dir_short), rds_paths)

          results_1 <- readRDS(rds_path1)
          results_2 <- readRDS(rds_path2)
        } else {
          results_1 <- read_azure_results(scenario_1_dir)
          shiny::incProgress(0.35)

          results_2 <- read_azure_results(scenario_2_dir)
          shiny::incProgress(0.35)
        }

        scenario_1_name <- scenario_selections()$scenario_1
        scenario_2_name <- scenario_selections()$scenario_2
        scenario_1_rt <- scenario_selections()$scenario_1_runtime
        scenario_2_rt <- scenario_selections()$scenario_2_runtime

        # scenario_1_id <- glue::glue("{scenario_1_name}+{scenario_1_rt}")
        # scenario_2_id <- glue::glue("{scenario_2_name}+{scenario_2_rt}")

        pod_lookup <-
          df1 <- reskit::compile_principal_pod_data(results_1, sites = NULL) |>
            dplyr::mutate(scenario = scenario_1_name, id = scenario_1_id)
        df2 <- reskit::compile_principal_pod_data(results_2, sites = NULL) |>
          dplyr::mutate(scenario = scenario_2_name, id = scenario_2_id)

        # data processing
        data <- dplyr::bind_rows(df1, df2)

        shiny::incProgress(0.1)

        admissions_data <- list(results_1, results_2) |>
          purrr::map2(c(scenario_1_id, scenario_2_id), \(x, y) {
            reskit::compile_principal_los_data(x, "admissions") |>
              dplyr::mutate(id = y)
          })
        beddays_data <- list(results_1, results_2) |>
          purrr::map2(c(scenario_1_id, scenario_2_id), \(x, y) {
            reskit::compile_principal_los_data(x, "beddays") |>
              dplyr::mutate(id = y)
          })
        los_data_combined <- list(admissions_data, beddays_data) |>
          purrr::map(\(x) {
            rlang::set_names(x, c(scenario_1_name, scenario_2_name)) |>
              purrr::list_rbind(names_to = "scenario")
          }) |>
          rlang::set_names(c("Admissions", "Bed Days")) |>
          purrr::list_rbind(names_to = "measure")

        shiny::incProgress(0.1)

        pcfs_1 <- prepare_all_principal_change_factors(
          r = results_1,
          site_codes = list(ip = NULL, op = NULL, aae = NULL)
        )

        pcfs_2 <- prepare_all_principal_change_factors(
          r = results_2,
          site_codes = list(ip = NULL, op = NULL, aae = NULL)
        )

        pcfs_comparison <- dplyr::bind_rows(
          scenario_1 = as.data.frame(dplyr::bind_rows(pcfs_1)) |>
            dplyr::mutate(scenario = scenario_1_name, id = scenario_1_id),
          scenario_2 = as.data.frame(dplyr::bind_rows(pcfs_2)) |>
            dplyr::mutate(scenario = scenario_2_name, id = scenario_2_id)
        )

        shiny::incProgress(0.1)

        data_distribution_summary <- list(
          reskit::compile_distribution_summary_data(results_1, "principal"),
          reskit::compile_distribution_summary_data(results_2, "principal"),
        ) |>
          purrr::map2(c(scenario_1_name, scenario_2_name), \(x, y) {
            dplyr::mutate(x, scenario = y)
          }) |>
          purrr::list_rbind() |>
          dplyr::summarise(
            dplyr::across(c("principal", "lower", "upper")),
            .by = c("scenario", "pod_label", "measure")
          )

        beeswarm_data1a <- results_1 |>
          reskit::compile_distribution_plot_data("admissions", "ip")
        beeswarm_data1b <- results_1 |>
          reskit::compile_distribution_plot_data("beddays", "ip")
        beeswarm_data1c <- results_1 |>
          reskit::compile_distribution_plot_data("attendances", "op")
        beeswarm_data1d <- results_1 |>
          reskit::compile_distribution_plot_data("tele_attendances", "op")
        beeswarm_data1e <- results_1 |>
          reskit::compile_distribution_plot_data("walk-in", "aae")
        beeswarm_data1f <- results_1 |>
          reskit::compile_distribution_plot_data("ambulance", "aae")
        beeswarm_data2a <- results_2 |>
          reskit::compile_distribution_plot_data("admissions", "ip")
        beeswarm_data2b <- results_2 |>
          reskit::compile_distribution_plot_data("beddays", "ip")
        beeswarm_data2c <- results_2 |>
          reskit::compile_distribution_plot_data("attendances", "op")
        beeswarm_data2d <- results_2 |>
          reskit::compile_distribution_plot_data("tele_attendances", "op")
        beeswarm_data2e <- results_2 |>
          reskit::compile_distribution_plot_data("walk-in", "aae")
        beeswarm_data2f <- results_2 |>
          reskit::compile_distribution_plot_data("ambulance", "aae")

        shiny::incProgress(0.1)
        list(
          data = data,
          results_1 = results_1,
          results_2 = results_2,
          los_data_combined = los_data_combined,
          scenario_1_name,
          scenario_2_name,
          scenario_1_id,
          scenario_2_id,
          waterfall_data = list(
            pcfs_1 = pcfs_1,
            pcfs_2 = pcfs_2
          ),
          pcfs_comparison = pcfs_comparison,
          data_distribution_summary = data_distribution_summary,
          beeswarm_data1a,
          beeswarm_data1b,
          beeswarm_data1c,
          beeswarm_data1d,
          beeswarm_data1e,
          beeswarm_data1f,
          beeswarm_data2a,
          beeswarm_data2b,
          beeswarm_data2c,
          beeswarm_data2d,
          beeswarm_data2e,
          beeswarm_data2f
        )
      })
    })
    processed
  })
}
