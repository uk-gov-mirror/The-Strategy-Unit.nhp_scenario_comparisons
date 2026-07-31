rds_root <- "rds/v3.1/RXN"
scenario1_name <- "20240114NDG1V1"
scenario2_name <- "20241212NDG2V1"

list_dirs <- purrr::partial(dir, full.names = TRUE, recursive = TRUE)

results1 <- readRDS(list_dirs(file.path(rds_root, scenario1_name)))
results2 <- readRDS(list_dirs(file.path(rds_root, scenario2_name)))

full_apm_lookup <- get_full_apm_lookup()
cond_apm_lookup <- get_condensed_apm_lookup()
full_apm_lookup2 <- dplyr::distinct(dplyr::select(full_apm_lookup, !"measure"))
cond_apm_lookup2 <- dplyr::distinct(dplyr::select(cond_apm_lookup, !"measure"))

tpma_lookup <- reskit::get_tpma_label_lookup()


# Create core tables with a row for each pair of measure and activity_type,
# for pmapping
mat_combos_tbl <- cond_apm_lookup |>
  dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
mat_combos_tbl_full <- full_apm_lookup |>
  dplyr::distinct(dplyr::pick(c("measure", "activity_type")))
at_lookup <- cond_apm_lookup |>
  dplyr::mutate(dplyr::across("activity_type_label", \(x) sub("s$", "", x))) |>
  dplyr::distinct(dplyr::pick(c("activity_type", "activity_type_label")))


# listify_mat_scenarios_tbl <- function(mat_scenarios_tbl) {
#   mat_scenarios_tbl |>
#     tidyr::pivot_longer(
#       !c("measure", "activity_type"),
#       names_to = "scenario"
#     ) |>
#     dplyr::filter_out(purrr::map_int(.data[["value"]], nrow) == 0) |>
#     tidyr::unnest("value") |>
#     tidyr::nest(.by = "activity_type") |>
#     tibble::deframe() |>
#     purrr::map(\(x) tibble::deframe(tidyr::nest(x, .by = "measure"))) |>
#     purrr::map_depth(2, \(x) list(data = x)) # not sure if data names needed
# }

unnest_mat_scenarios_tbl <- function(mat_scenarios_tbl) {
  mat_scenarios_tbl |>
    tidyr::pivot_longer(
      !c("measure", "activity_type"),
      names_to = "scenario"
    ) |>
    dplyr::filter_out(purrr::map_int(.data[["value"]], nrow) == 0) |>
    tidyr::unnest("value")
}


unnest_cfmat_scenarios_tbl <- function(mat_scenarios_tbl) {
  mat_scenarios_tbl |>
    dplyr::select(!"measure") |>
    tidyr::pivot_longer(!"activity_type", names_to = "scenario") |>
    dplyr::filter_out(purrr::map_int(.data[["value"]], nrow) == 0) |>
    tidyr::unnest("value")
}


# listify_cfmat_scenarios_tbl <- function(cfmat_scenarios_tbl) {
#   cfmat_scenarios_tbl |>
#     tidyr::pivot_longer(
#       !c("measure", "activity_type"),
#       names_to = "scenario"
#     ) |>
#     dplyr::filter_out(purrr::map_int(.data[["value"]], nrow) == 0) |>
#     dplyr::select(!"measure") |>
#     tidyr::unnest("value") |>
#     tidyr::nest(.by = "change_factor") |>
#     tibble::deframe() |>
#     purrr::map(\(x) tibble::deframe(tidyr::nest(x, .by = "activity_type"))) |>
#     purrr::map_depth(2, \(x) {
#       tibble::deframe(tidyr::nest(x, .by = "measure"))
#     }) |>
#     purrr::map_depth(3, \(x) list(data = x)) # not sure if data names needed
# }

# Prepare data for Summary chart

pt_compile_principal_pod_data <- function(...) {
  purrr::partial(
    reskit::compile_principal_pod_data,
    pod_lookup = cond_apm_lookup2
  )(...)
}
summary_data <- list(
  pt_compile_principal_pod_data(results1),
  pt_compile_principal_pod_data(results2)
) |>
  rlang::set_names(c(scenario1_name, scenario2_name)) |>
  purrr::list_rbind(names_to = "scenario") |>
  dplyr::mutate(
    dplyr::across("activity_type_label", \(x) {
      dplyr::if_else(grepl("^Inp", x), x, paste0(x, " Activity"))
    }),
    dplyr::across("pod_label", \(x) {
      forcats::fct_reorder(x, .data[["baseline"]])
    })
  )


# Test creation of Summary chart

create_summary_bar_chart(summary_data, "Inpatient Admissions")

# Prepare data for LoS chart

pt_compile_principal_los_data <- function(...) {
  purrr::partial(
    reskit::compile_principal_los_data,
    pod_lookup = cond_apm_lookup2
  )(...)
}

admissions_data <- list(results1, results2) |>
  purrr::map(\(x) pt_compile_principal_los_data(x, "admissions"))
beddays_data <- list(results1, results2) |>
  purrr::map(\(x) pt_compile_principal_los_data(x, "beddays"))
los_data <- list(admissions_data, beddays_data) |>
  purrr::map(\(x) {
    rlang::set_names(x, c(scenario1_name, scenario2_name)) |>
      purrr::list_rbind(names_to = "scenario")
  }) |>
  rlang::set_names(c("Admissions", "Bed Days")) |>
  purrr::list_rbind(names_to = "measure")


# Test creation of LoS chart
create_los_bar_chart(los_data, "Elective Admission", "Admissions")


# Prepare data for Waterfall chart

pt_compile_cf_data <- function(...) {
  purrr::partial(
    reskit::compile_change_factor_data,
    pod_lookup = cond_apm_lookup2,
    tpma_lookup = tpma_lookup
  )(...)
}
pt_compile_cf_data1 <- function(...) {
  purrr::partial(pt_compile_cf_data, results = results1)(...)
}
pt_compile_cf_data2 <- function(...) {
  purrr::partial(pt_compile_cf_data, results = results2)(...)
}


waterfall_data <- mat_combos_tbl |>
  dplyr::mutate(
    !!scenario1_name := purrr::pmap(mat_combos_tbl, pt_compile_cf_data1),
    !!scenario2_name := purrr::pmap(mat_combos_tbl, pt_compile_cf_data2)
  ) |>
  # listify_mat_scenarios_tbl()
  unnest_mat_scenarios_tbl() |>
  dplyr::left_join(at_lookup, "activity_type")


# Test creation of Waterfall chart

create_waterfall_chart(waterfall_data, "Inpatient", "Admissions")

# Prepare data for individual change factor (TPMA) impact charts

pt_compile_icf_data <- function(...) {
  purrr::partial(
    reskit::compile_indiv_change_factor_data,
    pod_lookup = cond_apm_lookup2,
    tpma_lookup = tpma_lookup
  )(...)
}
pt_compile_icf_data1 <- function(...) {
  purrr::partial(pt_compile_icf_data, results = results1)(...)
}
pt_compile_icf_data2 <- function(...) {
  purrr::partial(pt_compile_icf_data, results = results2)(...)
}


impact_data <- mat_combos_tbl |>
  dplyr::mutate(
    !!scenario1_name := purrr::pmap(mat_combos_tbl, pt_compile_icf_data1),
    !!scenario2_name := purrr::pmap(mat_combos_tbl, pt_compile_icf_data2)
  ) |>
  # listify_cfmat_scenarios_tbl()
  unnest_cfmat_scenarios_tbl() |>
  dplyr::left_join(at_lookup, "activity_type")

# Check production of Activity Avoidance Individual Change Factors chart

create_impact_chart(
  impact_data,
  "activity_avoidance",
  "Inpatient",
  "Admissions"
)

# Check production of Efficiencies Individual Change Factors chart
create_impact_chart(impact_data, "efficiencies", "Inpatient", "Bed Days")

# Prepare data for p10/p90 chart

principal_pi_data <- list(
  reskit::compile_distribution_summary_data(results1, "principal"),
  reskit::compile_distribution_summary_data(results2, "principal")
) |>
  rlang::set_names(c(scenario1_name, scenario2_name)) |>
  purrr::list_rbind(names_to = "scenario")

# Test creation of p10/p90 chart

create_principal_pi_bar_chart(
  principal_pi_data,
  "Inpatients",
  "Elective Admission"
)

# Prepare data for Beeswarm and S-curve charts

pt_compile_distr_data <- function(...) {
  purrr::partial(
    reskit::compile_distribution_plot_data,
    pod_lookup = full_apm_lookup2
  )(...)
}
pt_compile_dst_data1 <- function(...) {
  purrr::partial(pt_compile_distr_data, results = results1)(...)
}
pt_compile_dst_data2 <- function(...) {
  purrr::partial(pt_compile_distr_data, results = results2)(...)
}

beeswarm_data <- mat_combos_tbl_full |>
  dplyr::mutate(
    !!scenario1_name := purrr::pmap(mat_combos_tbl_full, pt_compile_dst_data1),
    !!scenario2_name := purrr::pmap(mat_combos_tbl_full, pt_compile_dst_data2)
  ) |>
  listify_mat_scenarios_tbl()
