source("plot/init.R")

EFFICIENCY_THRESHOLD <- 0.9999
EXPECTED_SAMPLE_COUNT <- 267L
EXPECTED_COMMON_NONFRONTIER_COUNT <- 202L
EXPECTED_SERVICE_ONLY_FRONTIER_COUNT <- 42L
EXPECTED_RISK_INCLUSIVE_FRONTIER_COUNT <- 65L
COMPARISON_PLOT_WIDTH_MM <- 87
COMPARISON_PLOT_HEIGHT_MM <- 70
COMPARISON_PDF_OUTPUT_PATH <- "article/Figures/comparison_rank.pdf"
COMPARISON_PNG_OUTPUT_PATH <- "article/Figures/comparison_rank.png"
COMPARISON_DATA_OUTPUT_PATH <- "article/Figures/comparison_rank_data.csv"
COMPARISON_PLOT_DPI <- 600
COMPARISON_FONT_FAMILY <- "Arial"

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

assert_true(nrow(scores_df) == EXPECTED_SAMPLE_COUNT,
            "The risk-inclusive score table must contain 267 samples.")
assert_true(nrow(base_scores_df) == EXPECTED_SAMPLE_COUNT,
            "The service-only score table must contain 267 samples.")
assert_true(!anyDuplicated(scores_df$sub_id),
            "Risk-inclusive sample IDs must be unique.")
assert_true(!anyDuplicated(base_scores_df$sub_id),
            "Service-only sample IDs must be unique.")
assert_true(setequal(scores_df$sub_id, base_scores_df$sub_id),
            "The two model tables must contain the same sample IDs.")
assert_true(all(is.finite(scores_df$score)) && all(is.finite(base_scores_df$score)),
            "Model scores must be finite and must not contain missing values.")

service_only <- base_scores_df %>%
  transmute(
    sub_id,
    type,
    service_only_score = score,
    service_only_frontier = score >= EFFICIENCY_THRESHOLD
  )

risk_inclusive <- scores_df %>%
  transmute(
    sub_id,
    type,
    risk_inclusive_score = score,
    risk_inclusive_frontier = score >= EFFICIENCY_THRESHOLD
  )

rank_audit <- service_only %>%
  inner_join(risk_inclusive, by = "sub_id", suffix = c("_service_only", "_risk_inclusive")) %>%
  mutate(
    type_agrees = type_service_only == type_risk_inclusive
  )

assert_true(all(rank_audit$type_agrees),
            "Sample types must agree by sub_id between model tables.")

rank_audit <- rank_audit %>%
  mutate(type = type_service_only) %>%
  select(-type_service_only, -type_risk_inclusive, -type_agrees) %>%
  mutate(
    frontier_transition = case_when(
      service_only_frontier & risk_inclusive_frontier ~ "frontier -> frontier",
      service_only_frontier & !risk_inclusive_frontier ~ "frontier -> non-frontier",
      !service_only_frontier & risk_inclusive_frontier ~ "non-frontier -> frontier",
      TRUE ~ "non-frontier -> non-frontier"
    ),
    common_nonfrontier = !service_only_frontier & !risk_inclusive_frontier
  )

common_nonfrontier <- rank_audit %>%
  filter(common_nonfrontier) %>%
  mutate(
    service_only_rank = rank(-service_only_score, ties.method = "average"),
    risk_inclusive_rank = rank(-risk_inclusive_score, ties.method = "average")
  ) %>%
  mutate(
    service_only_percentile = 100 * (EXPECTED_COMMON_NONFRONTIER_COUNT - service_only_rank) /
      (EXPECTED_COMMON_NONFRONTIER_COUNT - 1),
    risk_inclusive_percentile = 100 * (EXPECTED_COMMON_NONFRONTIER_COUNT - risk_inclusive_rank) /
      (EXPECTED_COMMON_NONFRONTIER_COUNT - 1),
    percentile_change = risk_inclusive_percentile - service_only_percentile,
    position_change = case_when(
      percentile_change > 0 ~ "higher",
      percentile_change < 0 ~ "lower",
      TRUE ~ "unchanged"
    )
  )

assert_true(nrow(common_nonfrontier) == EXPECTED_COMMON_NONFRONTIER_COUNT,
            "The common non-frontier cohort must contain 202 samples.")
assert_true(all(common_nonfrontier$service_only_percentile >= 0 &
                  common_nonfrontier$service_only_percentile <= 100),
            "Service-only percentiles must be within 0-100.")
assert_true(all(common_nonfrontier$risk_inclusive_percentile >= 0 &
                  common_nonfrontier$risk_inclusive_percentile <= 100),
            "Risk-inclusive percentiles must be within 0-100.")

rank_audit <- rank_audit %>%
  left_join(
    common_nonfrontier %>%
      select(
        sub_id,
        service_only_rank,
        risk_inclusive_rank,
        service_only_percentile,
        risk_inclusive_percentile,
        percentile_change,
        position_change
      ),
    by = "sub_id"
  ) %>%
  select(
    sub_id,
    type,
    service_only_score,
    risk_inclusive_score,
    service_only_frontier,
    risk_inclusive_frontier,
    frontier_transition,
    common_nonfrontier,
    service_only_rank,
    risk_inclusive_rank,
    service_only_percentile,
    risk_inclusive_percentile,
    percentile_change,
    position_change
  ) %>%
  arrange(as.integer(sub_id))

assert_true(nrow(rank_audit) == EXPECTED_SAMPLE_COUNT,
            "The audit table must preserve all 267 samples.")
assert_true(sum(rank_audit$common_nonfrontier) == EXPECTED_COMMON_NONFRONTIER_COUNT,
            "The audit table must mark exactly 202 common non-frontier samples.")
assert_true(all(is.na(rank_audit$service_only_percentile[!rank_audit$common_nonfrontier])),
            "Percentile fields must be NA outside the common cohort.")
assert_true(all(is.na(rank_audit$service_only_rank[!rank_audit$common_nonfrontier])) &&
              all(is.na(rank_audit$risk_inclusive_rank[!rank_audit$common_nonfrontier])) &&
              all(is.na(rank_audit$risk_inclusive_percentile[!rank_audit$common_nonfrontier])) &&
              all(is.na(rank_audit$percentile_change[!rank_audit$common_nonfrontier])) &&
              all(is.na(rank_audit$position_change[!rank_audit$common_nonfrontier])),
            "All rank-change fields must be NA outside the common cohort.")
assert_true(sum(rank_audit$service_only_frontier) == EXPECTED_SERVICE_ONLY_FRONTIER_COUNT,
            "The service-only frontier count must be 42.")
assert_true(sum(rank_audit$risk_inclusive_frontier) == EXPECTED_RISK_INCLUSIVE_FRONTIER_COUNT,
            "The risk-inclusive frontier count must be 65.")

readr::write_csv(rank_audit, COMPARISON_DATA_OUTPUT_PATH, na = "")

rank_comparison_plot <- ggplot(
  common_nonfrontier,
  aes(x = service_only_percentile, y = risk_inclusive_percentile, color = type)
) +
  geom_point(size = 2.2, alpha = 0.65) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "#e65050",
    linewidth = 0.35
  ) +
  scale_color_manual(values = color_palette) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    expand = c(0, 0)
  ) +
  coord_equal(expand = FALSE) +
  labs(
    x = "Service-only percentile",
    y = "Risk-inclusive percentile",
    color = "Type"
  ) +
  theme_bw(base_size = 9, base_family = COMPARISON_FONT_FAMILY) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9, family = COMPARISON_FONT_FAMILY),
    legend.text = element_text(size = 8, family = COMPARISON_FONT_FAMILY),
    legend.margin = margin(l = -3, unit = "pt"),
    legend.box.margin = margin(l = -3, unit = "pt"),
    aspect.ratio = 1,
    plot.margin = margin(t = 6, r = 1, b = 1, l = 1, unit = "pt"),
    text = element_text(size = 9, family = COMPARISON_FONT_FAMILY)
  )

ggsave(
  COMPARISON_PDF_OUTPUT_PATH,
  rank_comparison_plot,
  width = COMPARISON_PLOT_WIDTH_MM,
  height = COMPARISON_PLOT_HEIGHT_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  fallback_resolution = COMPARISON_PLOT_DPI
)

ggsave(
  COMPARISON_PNG_OUTPUT_PATH,
  rank_comparison_plot,
  device = ragg::agg_png,
  width = COMPARISON_PLOT_WIDTH_MM,
  height = COMPARISON_PLOT_HEIGHT_MM,
  units = "mm",
  dpi = COMPARISON_PLOT_DPI,
  bg = "white"
)

message(sprintf(
  "Generated %d-row audit table and %d-point common-cohort figure.",
  nrow(rank_audit),
  nrow(common_nonfrontier)
))
message("Position change is defined from percentile_change: positive = higher, negative = lower.")
