source("plot/init.R")

risk_input_file <- "sync/datapreparation/step5_allergy_risk/allergy_risk.csv"
sample_input_file <- "sync/datapreparation/dataclean/sample_data.csv"
output_file <- "article/Figures/allergyrisk.pdf"
png_output_file <- "article/Figures/allergyrisk.png"
plot_data_output_file <- "article/Figures/allergyrisk_data.csv"

expected_row_count <- 267L
type_codes <- names(color_palette)

metric_labels <- c(
  allergy_risk_normalized = "(a) Allergy risk",
  hazard_normalized = "(b) Hazard",
  exposure_normalized = "(c) Exposure",
  vulnerability_normalized = "(d) Vulnerability"
)

output_width_mm <- 178
output_height_mm <- 220
png_output_dpi <- 600
plot_font_family <- "Arial"
base_text_size_pt <- 12
panel_title_size_pt <- 12
axis_text_size_pt <- 9.6
normalized_limits <- c(0, 1)
plot_y_limits <- c(-0.04, 1)
y_breaks <- seq(0, 1, by = 0.25)
log_metrics <- c(
  "allergy_risk_normalized",
  "hazard_normalized"
)
log_sigma <- 0.001
log_plot_y_limits <- c(-0.0005, 1)
log_y_breaks <- c(0, 0.01, 0.1, 1)
log_y_labels <- sprintf("%.2f", log_y_breaks)

scatter_offset <- 0.22
scatter_width <- 0.12
scatter_alpha <- 0.45
scatter_size <- 1.7
box_width <- 0.16
box_alpha <- 0.55
violin_offset <- 0.12
violin_width <- 0.58
violin_alpha <- 0.28

risk_df <- readr::read_csv(
  risk_input_file,
  col_types = readr::cols_only(
    sub_id = readr::col_character(),
    allergy_risk_normalized = readr::col_double(),
    hazard_normalized = readr::col_double(),
    exposure_normalized = readr::col_double(),
    vulnerability_normalized = readr::col_double()
  ),
  show_col_types = FALSE
)

sample_df <- readr::read_csv(
  sample_input_file,
  col_types = readr::cols_only(
    sub_id = readr::col_character(),
    type = readr::col_character()
  ),
  show_col_types = FALSE
)

if (anyDuplicated(risk_df$sub_id) > 0L) {
  stop("Risk data contains duplicate sub_id values.", call. = FALSE)
}

if (anyDuplicated(sample_df$sub_id) > 0L) {
  stop("Sample data contains duplicate sub_id values.", call. = FALSE)
}

unmatched_ids <- risk_df |>
  dplyr::anti_join(sample_df, by = dplyr::join_by(sub_id))

if (nrow(unmatched_ids) > 0L) {
  stop("Some risk records have no matching sample type.", call. = FALSE)
}

allergy_risk_df <- risk_df |>
  dplyr::left_join(
    sample_df,
    by = dplyr::join_by(sub_id),
    relationship = "one-to-one"
  )

if (nrow(allergy_risk_df) != expected_row_count) {
  stop(
    sprintf(
      "Expected %d rows, found %d.",
      expected_row_count,
      nrow(allergy_risk_df)
    ),
    call. = FALSE
  )
}

observed_types <- sort(unique(allergy_risk_df$type))
if (!setequal(observed_types, type_codes)) {
  stop("Joined data does not contain the six configured type codes.", call. = FALSE)
}

metric_values <- unlist(
  allergy_risk_df[names(metric_labels)],
  use.names = FALSE
)
if (anyNA(metric_values) || any(!is.finite(metric_values))) {
  stop("Normalized metrics contain missing or non-finite values.", call. = FALSE)
}

if (any(
  metric_values < normalized_limits[1] |
    metric_values > normalized_limits[2]
)) {
  stop("Normalized metrics contain values outside [0, 1].", call. = FALSE)
}

readr::write_csv(
  allergy_risk_df |>
    dplyr::select(
      sub_id,
      type,
      dplyr::all_of(names(metric_labels))
    ) |>
    dplyr::arrange(type, sub_id),
  plot_data_output_file
)

plot_risk_metric <- function(data, metric, panel_title) {
  panel_data <- data |>
    dplyr::transmute(
      type,
      value = .data[[metric]]
    )

  type_order <- panel_data |>
    dplyr::summarise(
      median_value = stats::median(value),
      .by = type
    ) |>
    dplyr::arrange(dplyr::desc(median_value), type) |>
    dplyr::pull(type)

  panel_data <- panel_data |>
    dplyr::mutate(
      type = factor(type, levels = type_order),
      type_position = as.numeric(type),
      scatter_position = type_position - scatter_offset,
      violin_position = type_position + violin_offset
    )

  y_scale <- if (metric %in% log_metrics) {
    ggplot2::scale_y_continuous(
      transform = scales::transform_pseudo_log(
        base = 10,
        sigma = log_sigma
      ),
      limits = log_plot_y_limits,
      breaks = log_y_breaks,
      labels = log_y_labels,
      expand = ggplot2::expansion(mult = c(0, 0.02))
    )
  } else {
    ggplot2::scale_y_continuous(
      limits = plot_y_limits,
      breaks = y_breaks,
      labels = sprintf("%.2f", y_breaks),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    )
  }

  ggplot2::ggplot(panel_data, ggplot2::aes(y = value)) +
    ggbeeswarm::geom_quasirandom(
      ggplot2::aes(
        x = scatter_position,
        color = type,
        group = type
      ),
      width = scatter_width,
      alpha = scatter_alpha,
      size = scatter_size,
      show.legend = FALSE
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        x = type_position,
        color = type,
        fill = type,
        group = type
      ),
      width = box_width,
      alpha = box_alpha,
      linewidth = 0.35,
      outlier.shape = NA,
      show.legend = FALSE
    ) +
    gghalves::geom_half_violin(
      ggplot2::aes(
        x = violin_position,
        color = type,
        fill = type,
        group = type
      ),
      side = "r",
      width = violin_width,
      alpha = violin_alpha,
      linewidth = 0.45,
      trim = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(type_order),
      labels = type_order,
      expand = ggplot2::expansion(add = c(0.42, 0.62))
    ) +
    y_scale +
    ggplot2::scale_color_manual(values = color_palette) +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::labs(x = NULL, y = NULL, title = panel_title) +
    ggplot2::theme_bw(
      base_size = base_text_size_pt,
      base_family = plot_font_family
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = plot_font_family,
        size = base_text_size_pt
      ),
      plot.title = ggplot2::element_text(
        family = plot_font_family,
        size = panel_title_size_pt,
        face = "plain",
        hjust = 0,
        margin = ggplot2::margin(b = 1, unit = "pt")
      ),
      plot.title.position = "panel",
      axis.text.x = ggplot2::element_text(
        family = plot_font_family,
        size = axis_text_size_pt,
        face = "plain",
        margin = ggplot2::margin(t = 1.5, unit = "pt")
      ),
      axis.text.y = ggplot2::element_text(
        family = plot_font_family,
        size = axis_text_size_pt,
        face = "plain",
        margin = ggplot2::margin(r = 1.5, unit = "pt")
      ),
      axis.ticks.length = grid::unit(1.5, "pt"),
      plot.margin = ggplot2::margin(t = 2, r = 4, b = 2, l = 3, unit = "pt"),
      legend.position = "none"
    )
}

metric_plots <- purrr::imap(
  metric_labels,
  ~ plot_risk_metric(allergy_risk_df, .y, .x)
)

combined_figure <- patchwork::wrap_plots(metric_plots, ncol = 1)

ggplot2::ggsave(
  filename = output_file,
  plot = combined_figure,
  width = output_width_mm,
  height = output_height_mm,
  units = "mm",
  device = grDevices::cairo_pdf,
  fallback_resolution = png_output_dpi
)

ggplot2::ggsave(
  filename = png_output_file,
  plot = combined_figure,
  device = ragg::agg_png,
  width = output_width_mm,
  height = output_height_mm,
  units = "mm",
  dpi = png_output_dpi,
  bg = "white"
)
