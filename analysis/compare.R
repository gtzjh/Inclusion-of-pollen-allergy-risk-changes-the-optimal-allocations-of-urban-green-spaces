library(tidyverse)
library(deaR)
library(patchwork)

EFFICIENCY_THRESHOLD <- 0.9999
TOP_DECILE_N <- 27L
EXPECTED_SHARED_N <- 267L
EXPECTED_MODEL_N <- 267L
FONT_FAMILY <- "Arial"
BASE_TEXT_SIZE_PT <- 12
PANEL_TITLE_SIZE_PT <- 12
SCENARIO_TITLE_SIZE_PT <- 12
MEDIAN_LABEL_SIZE_PT <- 12
PERCENT_LABEL_SIZE_PT <- 11.4
SCENARIOS <- tribble(
  ~scenario, ~exposure_column,
  "Weekend", "weekend",
  "Working day", "workday"
)
TYPE_PALETTE <- c(
  "PIU" = "#B47B84", "RU" = "#6295A2", "RPU" = "#7895CB",
  "GSP" = "#c0c4c2", "CP" = "#40A578", "SU" = "#A0C49D"
)

assert_that <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_columns <- function(data, columns, object_name) {
  missing_columns <- setdiff(columns, names(data))
  assert_that(length(missing_columns) == 0L,
    paste0(object_name, " missing columns: ", paste(missing_columns, collapse = ", ")))
}

assert_unique_ids <- function(data, object_name) {
  assert_that(!anyNA(data$sub_id) && !anyDuplicated(data$sub_id),
    paste0(object_name, " must have unique non-missing sub_id values."))
}

assert_finite <- function(data, columns, object_name) {
  invalid_columns <- columns[!vapply(data[columns], function(x) all(is.finite(x)), logical(1))]
  assert_that(length(invalid_columns) == 0L,
    paste0(object_name, " has non-finite values: ", paste(invalid_columns, collapse = ", ")))
}

min_max <- function(values) {
  bounds <- range(values)
  assert_that(diff(bounds) > 0, "Min-max normalization requires non-constant values.")
  (values - bounds[1]) / diff(bounds)
}

load_inputs <- function() {
  list(
    allergy = readr::read_csv(
      "sync/datapreparation/step5_allergy_risk/allergy_risk.csv",
      col_types = readr::cols_only(
        sub_id = readr::col_character(), hazard_normalized = readr::col_double(),
        vulnerability_normalized = readr::col_double()
      ), show_col_types = FALSE
    ),
    exposure = readr::read_csv(
      "sync/datapreparation/step3_heatmap/heatmap.csv",
      col_types = readr::cols_only(
        sub_id = readr::col_character(), weekend = readr::col_double(),
        workday = readr::col_double()
      ), show_col_types = FALSE
    ),
    eco = readr::read_csv(
      "sync/datapreparation/step6_eco/eco_services.csv",
      col_types = readr::cols_only(
        sub_id = readr::col_character(), ES_LA = readr::col_double(),
        ES_CO2 = readr::col_double(), ES_Q = readr::col_double()
      ), show_col_types = FALSE
    ),
    diversity = readr::read_csv(
      "sync/datapreparation/step7_shannon/shannon.csv",
      col_types = readr::cols_only(sub_id = readr::col_character(), shannon = readr::col_double()),
      show_col_types = FALSE
    ) |>
      rename(diversity = shannon),
    maintenance = readr::read_csv(
      "sync/datapreparation/step2_maintenance_cost/maintenance_cost.csv",
      col_types = readr::cols_only(sub_id = readr::col_character(), maintenance_cost = readr::col_double()),
      show_col_types = FALSE
    ) |>
      summarise(maintenance_cost = sum(maintenance_cost), .by = sub_id),
    land_price = readr::read_csv(
      "sync/datapreparation/step1_locational_conditions/land_prices.csv",
      col_types = readr::cols_only(sub_id = readr::col_character(), land_prices = readr::col_double()),
      show_col_types = FALSE
    ) |>
      rename(location_condition = land_prices),
    sites = readr::read_csv(
      "sync/datapreparation/dataclean/sample_data.csv",
      col_types = readr::cols_only(
        sub_id = readr::col_character(), lon = readr::col_double(),
        lat = readr::col_double(), type = readr::col_character()
      ), show_col_types = FALSE
    )
  )
}

validate_inputs <- function(inputs) {
  purrr::iwalk(inputs, ~ assert_unique_ids(.x, .y))
  assert_columns(inputs$allergy, c("sub_id", "hazard_normalized", "vulnerability_normalized"), "allergy")
  assert_columns(inputs$exposure, c("sub_id", "weekend", "workday"), "exposure")
}

join_shared_sites <- function(inputs) {
  shared_sites <- inputs$sites |>
    inner_join(inputs$allergy, by = "sub_id") |>
    inner_join(inputs$exposure, by = "sub_id") |>
    inner_join(inputs$eco, by = "sub_id") |>
    inner_join(inputs$diversity, by = "sub_id") |>
    inner_join(inputs$maintenance, by = "sub_id") |>
    inner_join(inputs$land_price, by = "sub_id")
  assert_that(nrow(shared_sites) == EXPECTED_SHARED_N, "Shared allergy-risk sites must equal 267.")
  assert_unique_ids(shared_sites, "shared_sites")
  assert_finite(shared_sites,
    setdiff(names(shared_sites), c("sub_id", "type")), "model candidates")
  shared_sites
}

build_scenario_data <- function(shared_sites, scenario, exposure_column) {
  scenario_sites <- shared_sites |>
    mutate(
      exposure = .data[[exposure_column]],
      exposure_normalized = min_max(exposure),
      raw_risk = hazard_normalized * exposure_normalized * vulnerability_normalized,
      allergy_risk = min_max(raw_risk)
    )
  model_sites <- scenario_sites |>
    mutate(social_benefit = exposure)
  assert_that(nrow(model_sites) == EXPECTED_MODEL_N, "Model sites must equal 267.")
  assert_unique_ids(model_sites, paste0(scenario, " model sites"))
  assert_finite(model_sites, c("exposure", "exposure_normalized", "raw_risk", "allergy_risk"), scenario)
  model_sites
}

run_unsbm <- function(model_sites) {
  input_columns <- c("maintenance_cost", "location_condition")
  output_columns <- c("social_benefit", "ES_LA", "ES_CO2", "ES_Q", "diversity", "allergy_risk")
  dea_data <- deaR::make_deadata(
    model_sites, dmus = "sub_id", inputs = input_columns, outputs = output_columns,
    ud_outputs = match("allergy_risk", output_columns)
  )
  model <- deaR::model_sbmeff(dea_data, orientation = "no", rts = "vrs")
  scores <- deaR::efficiencies(model)
  tibble(sub_id = stringr::str_remove(names(scores), "^DMU"), score = as.numeric(scores))
}

rank_scores <- function(scores) {
  scores |>
    mutate(
      is_optimal = score >= EFFICIENCY_THRESHOLD,
      score_for_rank = if_else(is_optimal, 1, score),
      rank = min_rank(desc(score_for_rank))
    ) |>
    arrange(desc(score_for_rank), as.numeric(sub_id)) |>
    mutate(top_decile = row_number() <= TOP_DECILE_N) |>
    select(-score_for_rank)
}

run_scenarios <- function(shared_sites) {
  purrr::pmap_dfr(SCENARIOS, function(scenario, exposure_column) {
    model_sites <- build_scenario_data(shared_sites, scenario, exposure_column)
    run_unsbm(model_sites) |>
      inner_join(model_sites |> select(sub_id, type, exposure, exposure_normalized, allergy_risk), by = "sub_id") |>
      rank_scores() |>
      mutate(scenario = scenario, .before = sub_id) |>
      select(scenario, sub_id, type, exposure, exposure_normalized, allergy_risk, score, rank, is_optimal, top_decile)
  })
}

get_type_median_order <- function(score_data) {
  score_data |>
    summarise(median_score = median(score), .by = type) |>
    arrange(desc(median_score), type) |>
    pull(type) |>
    paste(collapse = " > ")
}

rank_exposure <- function(score_data) {
  score_data |>
    mutate(value_rank = min_rank(desc(exposure))) |>
    arrange(desc(exposure), as.numeric(sub_id)) |>
    mutate(top_27 = row_number() <= TOP_DECILE_N) |>
    select(sub_id, value_rank, top_27)
}

create_sensitivity_table <- function(score_data) {
  scenario_summary <- score_data |>
    summarise(
      mean_exposure = mean(exposure), median_exposure = median(exposure),
      exposure_q1 = quantile(exposure, 0.25), exposure_q3 = quantile(exposure, 0.75),
      median_score = median(score), optimal_n = sum(is_optimal),
      suboptimal_pct = 100 * mean(!is_optimal), .by = scenario
    ) |>
    pivot_longer(-scenario, names_to = "metric", values_to = "estimate") |>
    mutate(
      metric = recode(metric, mean_exposure = "Mean exposure", median_exposure = "Median exposure",
        exposure_q1 = "Exposure Q1", exposure_q3 = "Exposure Q3", median_score = "Median score",
        optimal_n = "Optimal sites", suboptimal_pct = "Suboptimal sites (%)"),
      unit = case_when(metric == "Optimal sites" ~ "sites", metric == "Suboptimal sites (%)" ~ "%", TRUE ~ "value"),
      criterion = if_else(metric == "Suboptimal sites (%)", "> 72%", NA_character_),
      passed = if_else(metric == "Suboptimal sites (%)", estimate > 72, NA)
    ) |>
    transmute(panel = "A. Scenario summary", scenario, comparison = NA_character_, type = NA_character_, metric,
      estimate = as.character(estimate), unit, criterion, passed)
  type_order <- score_data |>
    reframe(type_median_order = get_type_median_order(pick(everything())), .by = scenario)
  scenario_order <- type_order |>
    transmute(panel = "A. Scenario summary", scenario, comparison = NA_character_, type = NA_character_,
      metric = "Type median-score order", estimate = type_median_order, unit = "order",
      criterion = "Not assessed", passed = NA)
  weekend_data <- score_data |> filter(scenario == "Weekend")
  workday_data <- score_data |> filter(scenario == "Working day")
  paired <- weekend_data |>
    select(sub_id, exposure_weekend = exposure, exposure_normalized_weekend = exposure_normalized,
      allergy_risk_weekend = allergy_risk, score_weekend = score, rank_weekend = rank) |>
    inner_join(workday_data |>
      select(sub_id, exposure_workday = exposure, exposure_normalized_workday = exposure_normalized,
        allergy_risk_workday = allergy_risk, score_workday = score, rank_workday = rank), by = "sub_id") |>
    inner_join(rank_exposure(weekend_data) |> rename(exposure_rank_weekend = value_rank, exposure_top_weekend = top_27), by = "sub_id") |>
    inner_join(rank_exposure(workday_data) |> rename(exposure_rank_workday = value_rank, exposure_top_workday = top_27), by = "sub_id")
  pairwise_summary <- tibble(
    panel = "B. Pairwise stability", scenario = NA_character_, comparison = "Weekend vs Working day", type = NA_character_,
    metric = c("Exposure Spearman rho", "Normalized-exposure MAE", "Exposure median rank displacement", "Exposure top-27 overlap (%)",
      "Allergy risk Spearman rho", "Allergy risk MAE", "Score Spearman rho", "Score MAE", "Score median rank displacement"),
    estimate = c(
      cor(paired$exposure_weekend, paired$exposure_workday, method = "spearman"),
      mean(abs(paired$exposure_normalized_weekend - paired$exposure_normalized_workday)),
      median(abs(paired$exposure_rank_weekend - paired$exposure_rank_workday)),
      100 * sum(paired$exposure_top_weekend & paired$exposure_top_workday) / TOP_DECILE_N,
      cor(paired$allergy_risk_weekend, paired$allergy_risk_workday, method = "spearman"),
      mean(abs(paired$allergy_risk_weekend - paired$allergy_risk_workday)),
      cor(if_else(paired$score_weekend >= EFFICIENCY_THRESHOLD, 1, paired$score_weekend),
        if_else(paired$score_workday >= EFFICIENCY_THRESHOLD, 1, paired$score_workday), method = "spearman"),
      mean(abs(paired$score_weekend - paired$score_workday)),
      median(abs(paired$rank_weekend - paired$rank_workday))
    ),
    unit = c("rho", "normalized value", "ranks", "%", "rho", "normalized value", "rho; scores capped at 0.9999", "score", "ranks"),
    criterion = c(">= 0.90", NA, "<= 13", ">= 80%", ">= 0.90", NA, ">= 0.90", "<= 0.05", "<= 13")
  ) |>
    mutate(passed = case_when(
      metric %in% c("Exposure Spearman rho", "Allergy risk Spearman rho", "Score Spearman rho") ~ estimate >= 0.90,
      metric == "Score MAE" ~ estimate <= 0.05,
      str_detect(metric, "rank displacement") ~ estimate <= 13,
      str_detect(metric, "overlap") ~ estimate >= 80,
      TRUE ~ NA
    )) |>
    mutate(estimate = as.character(estimate))
  type_summary <- score_data |>
    summarise(sites = n(), median_score = median(score), optimal_sites = sum(is_optimal),
      optimal_pct = 100 * mean(is_optimal), .by = c(scenario, type)) |>
    pivot_longer(c(sites, median_score, optimal_sites, optimal_pct), names_to = "metric", values_to = "estimate") |>
    mutate(
      metric = recode(metric, sites = "Sites", median_score = "Median score", optimal_sites = "Optimal sites", optimal_pct = "Optimal sites (%)"),
      unit = case_when(metric %in% c("Sites", "Optimal sites") ~ "sites", metric == "Optimal sites (%)" ~ "%", TRUE ~ "score")
    ) |>
    transmute(panel = "C. Type summary", scenario, comparison = NA_character_, type, metric,
      estimate = as.character(estimate), unit, criterion = NA_character_, passed = NA)
  bind_rows(scenario_summary, scenario_order, pairwise_summary, type_summary) |>
    select(panel, scenario, comparison, type, metric, estimate, unit, criterion, passed)
}

create_score_figure <- function(score_data, scenario) {
  scenario_data <- score_data |>
    filter(scenario == !!scenario)
  all_score_medians <- scenario_data |>
    summarise(median_score = median(score), .by = type) |>
    arrange(desc(median_score), type)
  all_type_order <- all_score_medians |> pull(type)
  all_score_data <- scenario_data |>
    mutate(type = factor(type, levels = all_type_order))
  all_score_medians <- all_score_medians |>
    mutate(type = factor(type, levels = all_type_order))
  optimal_summary <- scenario_data |>
    summarise(optimal_proportion = mean(is_optimal), .by = type) |>
    arrange(desc(optimal_proportion), type)
  optimal_type_order <- optimal_summary |> pull(type)
  optimal_summary <- optimal_summary |>
    mutate(type = factor(type, levels = optimal_type_order))
  suboptimal_data <- scenario_data |> filter(!is_optimal)
  suboptimal_medians <- suboptimal_data |>
    summarise(median_score = median(score), .by = type) |>
    arrange(desc(median_score), type)
  suboptimal_type_order <- suboptimal_medians |> pull(type)
  suboptimal_data <- suboptimal_data |>
    mutate(type = factor(type, levels = suboptimal_type_order))
  suboptimal_medians <- suboptimal_medians |>
    mutate(type = factor(type, levels = suboptimal_type_order))
  base_theme <- theme_bw(
    base_size = BASE_TEXT_SIZE_PT,
    base_family = FONT_FAMILY
  ) +
    theme(
      text = element_text(size = BASE_TEXT_SIZE_PT, family = FONT_FAMILY),
      plot.title = element_text(
        size = PANEL_TITLE_SIZE_PT,
        family = FONT_FAMILY
      )
    )
  all_scores <- ggplot(all_score_data, aes(x = type, y = score, colour = type, fill = type)) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    geom_text(
      data = all_score_medians,
      aes(x = type, y = max(all_score_data$score) * 0.9, label = sprintf("%.3f", median_score)),
      colour = "black", size = MEDIAN_LABEL_SIZE_PT, size.unit = "pt",
      vjust = 0, family = FONT_FAMILY
    ) +
    scale_colour_manual(values = TYPE_PALETTE, guide = "none") +
    scale_fill_manual(values = TYPE_PALETTE, guide = "none") +
    labs(x = "", y = "", title = "(a)") +
    guides(colour = "none", fill = "none") +
    base_theme
  optimal_share <- optimal_summary |>
    ggplot(aes(x = type, y = optimal_proportion, fill = type)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(
      aes(label = sprintf("%.3f%%", optimal_proportion * 100), y = optimal_proportion + 0.05),
      colour = "black", size = PERCENT_LABEL_SIZE_PT, size.unit = "pt",
      vjust = 1, family = FONT_FAMILY
    ) +
    scale_fill_manual(values = TYPE_PALETTE, guide = "none") +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, max(optimal_summary$optimal_proportion) * 1.2)
    ) +
    labs(x = "", y = "", title = "(b)") +
    base_theme
  suboptimal_scores <- suboptimal_data |>
    ggplot(aes(x = type, y = score, colour = type, fill = type)) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    geom_boxplot(width = 0.2, linewidth = 0.3, position = position_nudge(x = 0.3), alpha = 0.6) +
    geom_text(
      data = suboptimal_medians,
      aes(x = type, y = max(suboptimal_data$score) * 0.9, label = sprintf("%.3f", median_score)),
      colour = "black", size = MEDIAN_LABEL_SIZE_PT, size.unit = "pt",
      vjust = 0, family = FONT_FAMILY
    ) +
    scale_colour_manual(values = TYPE_PALETTE, guide = "none") +
    scale_fill_manual(values = TYPE_PALETTE, guide = "none") +
    labs(x = "", y = "", title = "(c)") +
    guides(colour = "none", fill = "none") +
    base_theme
  (all_scores + optimal_share + suboptimal_scores) +
    patchwork::plot_layout(nrow = 3, guides = "collect") +
    patchwork::plot_annotation(
      title = scenario,
      theme = theme(
        plot.title = element_text(
          size = SCENARIO_TITLE_SIZE_PT,
          family = FONT_FAMILY
        )
      )
    )
}

write_outputs <- function(score_data, table_data, figures) {
  dir.create("article/Tables", recursive = TRUE, showWarnings = FALSE)
  dir.create("article/Figures", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(table_data, "article/Tables/compare_exposure_daytype_sensitivity.csv")
  readr::write_csv(score_data |> select(-top_decile), "article/Figures/compare_exposure_daytype_score_data.csv")
  ggsave("article/Figures/compare_weekend_score_by_type.pdf", figures$weekend, device = grDevices::cairo_pdf, width = 178, height = 168, units = "mm")
  ggsave("article/Figures/compare_weekend_score_by_type.png", figures$weekend, width = 178, height = 168, units = "mm", dpi = 600, bg = "white")
  ggsave("article/Figures/compare_workday_score_by_type.pdf", figures$workday, device = grDevices::cairo_pdf, width = 178, height = 168, units = "mm")
  ggsave("article/Figures/compare_workday_score_by_type.png", figures$workday, width = 178, height = 168, units = "mm", dpi = 600, bg = "white")
  old_figures <- c(
    "article/Figures/compare_exposure_daytype_sensitivity.pdf",
    "article/Figures/compare_exposure_daytype_sensitivity.png"
  )
  file.remove(old_figures[file.exists(old_figures)])
}

main <- function() {
  inputs <- load_inputs()
  validate_inputs(inputs)
  shared_sites <- join_shared_sites(inputs)
  score_data <- run_scenarios(shared_sites)
  assert_that(nrow(score_data) == 534L, "Score data must contain 534 rows.")
  assert_that(n_distinct(score_data$sub_id) == EXPECTED_MODEL_N, "Score data must contain 267 sites.")
  assert_finite(score_data, c("exposure", "exposure_normalized", "allergy_risk", "score", "rank"), "score_data")
  table_data <- create_sensitivity_table(score_data)
  assert_that(nrow(table_data) == 73L, "Sensitivity table must contain 73 rows.")
  figures <- list(
    weekend = create_score_figure(score_data, "Weekend"),
    workday = create_score_figure(score_data, "Working day")
  )
  write_outputs(score_data, table_data, figures)
  message("Completed weekend and working-day sensitivity analysis: 534 score rows.")
}

main()
