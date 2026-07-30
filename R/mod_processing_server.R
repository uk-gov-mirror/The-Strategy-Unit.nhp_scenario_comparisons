mod_processing_server <- function(
  id,
  results_metadata_tbl,
  selections,
  scenario_selections,
  trigger,
  local_data_flag
) {
  shiny::moduleServer(id, function(input, output, session) {
    processed_data <- shiny::eventReactive(trigger(), {
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
          scenario_selections()$scenario1,
          scenario_selections()$scenario1_rt,
          scenario_selections()$scenario2,
          scenario_selections()$scenario2_rt
        )

        scenario1_dir <- results_metadata_tbl |>
          dplyr::filter(
            .data[["scenario"]] == selected$scenario1,
            .data[["create_datetime"]] == selected$scenario1_rt
          ) |>
          dplyr::pull("aggregated_results_path")

        scenario2_dir <- results_metadata_tbl |>
          dplyr::filter(
            .data[["scenario"]] == selected$scenario2,
            .data[["create_datetime"]] == selected$scenario2_rt
          ) |>
          dplyr::pull("aggregated_results_path")

        shiny::incProgress(0.1)

        shiny::req(all(lengths(c(scenario1_dir, scenario2_dir)) == 1))

        if (local_data_flag) {
          rds_paths <- dir("rds", "\\.rds$", full.names = TRUE)
          scenario1_dir_short <- sub("^agg[^/]+", "rds", scenario1_dir)
          scenario2_dir_short <- sub("^agg[^/]+", "rds", scenario2_dir)
          rds_path1 <- grepv(paste0("^", scenario1_dir_short), rds_paths)
          rds_path2 <- grepv(paste0("^", scenario2_dir_short), rds_paths)

          results1 <- readRDS(rds_path1)
          results2 <- readRDS(rds_path2)
        } else {
          results1 <- read_azure_results(scenario1_dir)
          shiny::incProgress(0.35)

          results2 <- read_azure_results(scenario2_dir)
          shiny::incProgress(0.35)
        }

        scenario1_name <- scenario_selections()$scenario1
        scenario2_name <- scenario_selections()$scenario2
        scenario1_dttm <- scenario_selections()$scenario1_dttm
        scenario2_dttm <- scenario_selections()$scenario2_dttm

        # scenario_1_id <- glue::glue("{scenario_1_name}+{scenario_1_dttm}")
        # scenario_2_id <- glue::glue("{scenario_2_name}+{scenario_2_dttm}")

        # This requires a call to GitHub to retrieve a file.
        # Run once and then pass data to reskit functions, rather than running
        # each time a function is called
        fapm_lookup <- get_full_apm_lookup()
        capm_lookup <- get_condensed_apm_lookup()
        fapm_lookup2 <- dplyr::distinct(dplyr::select(fapm_lookup, !"measure"))
        capm_lookup2 <- dplyr::distinct(dplyr::select(capm_lookup, !"measure"))

        # Prepare data for Summary chart
        df1 <- reskit::compile_principal_pod_data(results_1, capm_lookup2) |>
          dplyr::mutate(scenario = scenario1_name)
        df2 <- reskit::compile_principal_pod_data(results_2, capm_lookup2) |>
          dplyr::mutate(scenario = scenario2_name)
        summary_data <- dplyr::bind_rows(df1, df2)

        shiny::incProgress(0.1)

        # Prepare data for LoS chart
        admissions_data <- list(results1, results2) |>
          purrr::map(\(x) {
            reskit::compile_principal_los_data(x, "admissions", capm_lookup2)
          })
        beddays_data <- list(results1, results2) |>
          purrr::map(\(x) {
            reskit::compile_principal_los_data(x, "beddays", capm_lookup2)
          })
        los_data <- list(admissions_data, beddays_data) |>
          purrr::map(\(x) {
            rlang::set_names(x, c(scenario1_name, scenario2_name)) |>
              purrr::list_rbind(names_to = "scenario")
          }) |>
          rlang::set_names(c("Admissions", "Bed Days")) |>
          purrr::list_rbind(names_to = "measure")

        shiny::incProgress(0.1)

        # Prepare data for Waterfall chart

        shiny::incProgress(0.1)

        # Prepare data for p10/p90 chart

        principal_pi_data <- list(
          reskit::compile_distribution_summary_data(results1, "principal"),
          reskit::compile_distribution_summary_data(results2, "principal")
        ) |>
          rlang::set_names(c(scenario1_name, scenario2_name)) |>
          purrr::list_rbind(names_to = "scenario")

        # Prepare data for Beeswarm chart
        pt_compile_distr_data <- function(...) {
          purrr::partial(
            reskit::compile_distribution_plot_data,
            pod_lookup = pod_lookup
          )(...)
        }

        beeswarm_combos_tbl <- pod_lookup |>
          dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
        beeswarm_data <- beeswarm_combos_tbl |>
          dplyr::mutate(
            data = purrr::pmap(beeswarm_combos_tbl, pt_compile_distr_data)
          )

        shiny::incProgress(0.1)

        # Create a list to export data as `processed_data`
        list(
          summary_data = summary_data,
          # results_1 = results_1,
          # results_2 = results_2,
          los_data = los_data,
          scenario1_name = scenario1_name,
          scenario2_name = scenario2_name,
          # scenario_1_id,
          # scenario_2_id,
          waterfall_data = waterfall_data,
          impact_data = impact_data,
          principal_pi_data = principal_pi_data,
          beeswarm_data = beeswarm_data,
          full_apm_lookup = full_apm_lookup,
          cond_apm_lookup = cond_apm_lookup
        )
      })
    })
    processed_data
  })
}
