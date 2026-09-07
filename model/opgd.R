library(tidyverse)
library(gdverse)
source("model/dis_cbd.R")

RESULTS_DIR <- "sync/results"
FIGURES_DIR <- "article/Figures"
SCORE_PATH <- file.path(RESULTS_DIR, "unsbm_exp_slack_rate.csv")
INDEPENDENCIES_DIR <- "sync/driving_factors/independencies"
OLD_DIS_CBD_PATH <- file.path(INDEPENDENCIES_DIR, "DIS_CBD.csv")
EXCLUDED_PREDICTORS <- c("DEN_WATER", "DEN_UGS")
EXCLUDED_PREDICTOR_PREFIXES <- "ACC_"
FUNCTIONALITY_SOURCE_COLUMNS <- c(
  "ACC_ACD", "ACC_COM", "ACC_FC", "ACC_HC", "ACC_HT", "ACC_LS",
  "ACC_RT", "ACC_SC", "ACC_SHOP", "ACC_SPORT", "ACC_TOUR"
)
EXPECTED_PREDICTOR_COUNT <- 9L
FACTOR_DISPLAY_LABELS <- c(
  DEN_BUILT = "Building Footprint",
  DEN_FLOOR = "Floor Area Ratio",
  DEN_DRIVE = "Density of Vehicle Road",
  DEN_WALK = "Density of Walkable Road",
  DIS_BUS = "Distance to Bus stop",
  DIS_METRO = "Distance to Metro Station",
  DIS_CBD = "Distance to CBD",
  FUNC_MIX = "POI Mixture",
  Functionality = "POI Density"
)
OPGD_DISCNUM <- 3:8
P_VALUE_THRESHOLD <- 0.05
SIGNIFICANCE_MARKER <- "**"
SIGNIFICANCE_CAPTION <- "** p < 0.05"
SIGNIFICANCE_MARKER_SIZE <- 2.5
SIGNIFICANCE_CAPTION_SIZE <- 9
SIGNIFICANCE_CAPTION_HJUST <- 1.2
SIGNIFICANCE_CAPTION_VJUST <- 1.4
PLOT_WIDTH_MM <- 114
PLOT_HEIGHT_MM <- 67
PLOT_LABEL_WRAP_WIDTH <- 24
PLOT_LABEL_ANGLE <- 35
PLOT_DPI <- 600
FACTOR_PLOT_PNG_PATH <- file.path(FIGURES_DIR, "opgd_factor_q.png")
FACTOR_PLOT_PDF_PATH <- file.path(FIGURES_DIR, "opgd_factor_q.pdf")
FACTOR_PLOT_DATA_PATH <- file.path(FIGURES_DIR, "opgd_factor_q_data.csv")

assert_that <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

read_predictor_file <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      .default = readr::col_double(),
      sub_id = readr::col_character()
    ),
    show_col_types = FALSE
  )
}

load_analysis_data <- function(
    score_path,
    independencies_dir,
    old_dis_cbd_path,
    excluded_predictors,
    excluded_predictor_prefixes
) {
  score_data <- readr::read_csv(
    score_path,
    col_types = readr::cols_only(
      sub_id = readr::col_character(),
      score = readr::col_double(),
      lon = readr::col_double(),
      lat = readr::col_double()
    ),
    show_col_types = FALSE
  )
  predictor_paths <- list.files(independencies_dir, pattern = "\\.csv$", full.names = TRUE)
  assert_that(
    old_dis_cbd_path %in% predictor_paths,
    "The legacy DIS_CBD input file is missing."
  )
  predictor_paths <- setdiff(predictor_paths, old_dis_cbd_path)
  assert_that(
    length(predictor_paths) == 6L,
    "Expected exactly six predictor CSV files after excluding legacy DIS_CBD.csv."
  )

  cbd_distance_data <- score_data |>
    select(sub_id, lon, lat) |>
    calculate_nearest_cbd_distance()

  predictor_data <- predictor_paths |>
    purrr::map(read_predictor_file) |>
    purrr::reduce(dplyr::inner_join, by = "sub_id") |>
    inner_join(
      cbd_distance_data |> select(sub_id, DIS_CBD),
      by = "sub_id"
    )

  score_data <- score_data |>
    select(sub_id, score)

  assert_that(!anyDuplicated(score_data$sub_id), "Score data have duplicate sub_id values.")
  assert_that(!anyDuplicated(predictor_data$sub_id), "Predictor data have duplicate sub_id values.")
  assert_that(
    all(excluded_predictors %in% names(predictor_data)),
    "Excluded predictors are missing from the input data."
  )

  predictor_names <- setdiff(names(predictor_data), "sub_id")
  raw_acc_columns <- predictor_names[
    purrr::map_lgl(
      predictor_names,
      ~ any(startsWith(.x, excluded_predictor_prefixes))
    )
  ]
  assert_that(
    length(raw_acc_columns) == length(FUNCTIONALITY_SOURCE_COLUMNS) &&
      setequal(raw_acc_columns, FUNCTIONALITY_SOURCE_COLUMNS),
    paste0(
      "Raw ACC columns must be exactly the configured ",
      length(FUNCTIONALITY_SOURCE_COLUMNS),
      " Functionality source columns."
    )
  )
  assert_that(
    all(vapply(predictor_data[raw_acc_columns], is.numeric, logical(1))),
    "All raw ACC columns must be numeric."
  )
  raw_acc_values <- as.matrix(predictor_data[FUNCTIONALITY_SOURCE_COLUMNS])
  assert_that(
    all(is.finite(raw_acc_values)),
    "All raw ACC values must be finite and non-missing."
  )
  assert_that(
    all(raw_acc_values >= 0),
    "All raw ACC values must be nonnegative."
  )
  predictors_to_exclude <- union(excluded_predictors, raw_acc_columns)

  predictor_data <- predictor_data |>
    mutate(
      Functionality = rowSums(
        pick(all_of(FUNCTIONALITY_SOURCE_COLUMNS)),
        na.rm = FALSE
      )
    )

  score_data |>
    inner_join(predictor_data, by = "sub_id") |>
    select(-all_of(predictors_to_exclude))
}

validate_analysis_data <- function(analysis_data) {
  predictors <- analysis_data |>
    select(-sub_id, -score) |>
    names()

  assert_that(nrow(analysis_data) == 267L, "Inner joins must yield exactly 267 rows.")
  assert_that(
    dplyr::n_distinct(analysis_data$sub_id) == 267L,
    "Analysis rows must have unique sub_id values."
  )
  assert_that(
    length(predictors) == EXPECTED_PREDICTOR_COUNT,
    "Unexpected number of predictors after exclusions."
  )
  assert_that(
    !any(startsWith(predictors, EXCLUDED_PREDICTOR_PREFIXES)),
    "Predictors with an excluded ACC_ prefix remain in the analysis data."
  )
  assert_that(
    !any(tolower(names(analysis_data)) == "type"),
    "TYPE must not be present in the analysis data."
  )
  assert_that(
    setequal(predictors, names(FACTOR_DISPLAY_LABELS)),
    "Predictors and configured display labels do not match."
  )
  assert_that(!anyNA(analysis_data), "Analysis data contain missing values.")
  assert_that(
    all(vapply(analysis_data[predictors], is.numeric, logical(1))),
    "All continuous predictors must be numeric."
  )
  assert_that(
    all(is.finite(as.matrix(analysis_data[predictors]))),
    "All continuous predictors must be finite."
  )
  assert_that(
    all(analysis_data$Functionality >= 0),
    "Functionality must be nonnegative."
  )
  assert_that(
    all(is.finite(analysis_data$score)),
    "Score must be finite and retained on its raw DEA scale."
  )

  predictors
}

run_opgd <- function(analysis_data, predictors) {
  detector_data <- analysis_data |>
    select(-sub_id)

  opgd_result <- gdverse::opgd(
    score ~ .,
    data = detector_data,
    discvar = predictors,
    discnum = OPGD_DISCNUM,
    type = "factor"
  )

  optimal_parameters <- opgd_result$opt_param |>
    tibble::as_tibble() |>
    transmute(
      factor = varibale,
      classes = discnum,
      method = method
    )

  factor_detector <- opgd_result$factor |>
    tibble::as_tibble() |>
    transmute(
      factor = variable,
      q = `Q-statistic`,
      p_value = `P-value`
    )

  factor_results <- tibble(
    factor = predictors,
    factor_kind = rep("built_environment", length(predictors))
  ) |>
    left_join(factor_detector, by = "factor") |>
    arrange(desc(q), factor)

  assert_that(
    nrow(optimal_parameters) == length(predictors),
    "OPGD must return one optimal discretization for each continuous predictor."
  )
  assert_that(
    nrow(factor_results) == length(predictors) &&
      !anyNA(factor_results[c("q", "p_value")]),
    "OPGD must return q and P values for all explanatory factors."
  )

  list(
    optimal_parameters = optimal_parameters,
    factor_results = factor_results
  )
}

prepare_factor_plot_data <- function(factor_results) {
  factor_results |>
    mutate(display_factor = unname(FACTOR_DISPLAY_LABELS[factor])) |>
    arrange(desc(q), factor)
}

build_factor_plot <- function(factor_plot_data) {
  factor_plot_data <- factor_plot_data |>
    mutate(
      display_factor = factor(
        stringr::str_wrap(display_factor, width = PLOT_LABEL_WRAP_WIDTH),
        levels = stringr::str_wrap(
          display_factor,
          width = PLOT_LABEL_WRAP_WIDTH
        )
      )
    )
  significant_factor_data <- factor_plot_data |>
    filter(p_value < P_VALUE_THRESHOLD)

  ggplot(factor_plot_data, aes(x = display_factor, y = q)) +
    geom_col(
      fill = scales::alpha("#7895CB", 0.5),
      color = "#7895CB",
      linewidth = 0.3,
      width = 0.7
    ) +
    geom_text(
      data = significant_factor_data,
      aes(y = q, label = SIGNIFICANCE_MARKER),
      vjust = -0.35,
      size = SIGNIFICANCE_MARKER_SIZE,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = SIGNIFICANCE_CAPTION,
      hjust = SIGNIFICANCE_CAPTION_HJUST,
      vjust = SIGNIFICANCE_CAPTION_VJUST,
      size = SIGNIFICANCE_CAPTION_SIZE / ggplot2::.pt,
      color = "black"
    ) +
    scale_y_continuous(
      breaks = c(0, 0.05, 0.10),
      labels = scales::label_number(accuracy = 0.01),
      expand = expansion(mult = c(0, 0.10))
    ) +
    labs(
      x = NULL,
      y = "q statistic"
    ) +
    theme_bw(base_size = 8, base_family = "sans") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold"),
      axis.text.y = element_text(color = "black"),
      axis.text.x = element_text(
        color = "black",
        angle = PLOT_LABEL_ANGLE,
        hjust = 1,
        vjust = 1
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey35", linewidth = 0.5),
      plot.margin = margin(3, 3, 2, 3)
    )
}

write_outputs <- function(
    analysis_data,
    optimal_parameters,
    factor_results,
    factor_plot,
    factor_plot_data
) {
  readr::write_csv(analysis_data, file.path(RESULTS_DIR, "opgd_analysis_data.csv"))
  readr::write_csv(
    optimal_parameters,
    file.path(RESULTS_DIR, "opgd_optimal_parameters.csv")
  )
  readr::write_csv(factor_results, file.path(RESULTS_DIR, "opgd_factor_results.csv"))
  dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(factor_plot_data, FACTOR_PLOT_DATA_PATH)
  ggsave(
    FACTOR_PLOT_PNG_PATH,
    factor_plot,
    width = PLOT_WIDTH_MM,
    height = PLOT_HEIGHT_MM,
    units = "mm",
    dpi = PLOT_DPI,
    bg = "white"
  )
  ggsave(
    FACTOR_PLOT_PDF_PATH,
    factor_plot,
    device = grDevices::pdf,
    width = PLOT_WIDTH_MM,
    height = PLOT_HEIGHT_MM,
    units = "mm",
    dpi = PLOT_DPI,
    bg = "white"
  )
}

main <- function() {
  analysis_data <- load_analysis_data(
    SCORE_PATH,
    INDEPENDENCIES_DIR,
    OLD_DIS_CBD_PATH,
    EXCLUDED_PREDICTORS,
    EXCLUDED_PREDICTOR_PREFIXES
  )
  predictors <- validate_analysis_data(analysis_data)
  opgd_analysis <- run_opgd(analysis_data, predictors)
  factor_plot_data <- prepare_factor_plot_data(opgd_analysis$factor_results)
  factor_plot <- build_factor_plot(factor_plot_data)
  write_outputs(
    analysis_data,
    opgd_analysis$optimal_parameters,
    opgd_analysis$factor_results,
    factor_plot,
    factor_plot_data
  )

  message(
    "OPGD complete: 267 observations, ",
    length(predictors),
    " explanatory factors. Results are exploratory and non-causal."
  )
}

main()
