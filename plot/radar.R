library(tidyverse)
library(ggplot2)

source("plot/init.R")

RADAR_PDF_OUTPUT_PATH <- "article/Figures/radar.pdf"
RADAR_PNG_OUTPUT_PATH <- "article/Figures/radar.png"
RADAR_PLOT_WIDTH_MM <- 178
RADAR_PLOT_HEIGHT_MM <- 104
RADAR_PLOT_DPI <- 600
RADAR_FONT_FAMILY <- "Arial"
RADAR_GROUP_HEADER_SIZE_PT <- 9
RADAR_METRIC_LABEL_SIZE_PT <- 7
RADAR_RADIUS_LABEL_SIZE_PT <- 6
RADAR_SMALL_LABEL_SIZE_PT <- 6
RADAR_LEGEND_KEY_WIDTH_MM <- 8
RADAR_LEGEND_KEY_HEIGHT_MM <- 3
RADAR_PLOT_MARGIN_VERTICAL_MM <- 1
RADAR_PLOT_MARGIN_HORIZONTAL_MM <- 3

metric_columns <- c(
    "GMC" = "rate_maintenance_cost",
    "LC" = "rate_location_condition",
    "SB" = "rate_social_benefit",
    "ES_LA" = "rate_ES_LA",
    "ES_CO2" = "rate_ES_CO2",
    "ES_Q" = "rate_ES_Q",
    "DIV" = "rate_diversity",
    "RISK" = "rate_allergy_risk"
)
metrics <- names(metric_columns)
metric_display_labels <- c(
    "GMC" = "plain('GMC')",
    "LC" = "plain('LC')",
    "SB" = "plain('SB')",
    "ES_LA" = "plain('ES_LA')",
    "ES_CO2" = "plain('ES_CO')[2]",
    "ES_Q" = "plain('ES_Q')",
    "DIV" = "plain('DIV')",
    "RISK" = "plain('RISK')"
)
metric_dimensions <- c(
    "GMC" = "INPUT",
    "LC" = "INPUT",
    "SB" = "SERVICES",
    "ES_LA" = "SERVICES",
    "ES_CO2" = "SERVICES",
    "ES_Q" = "SERVICES",
    "DIV" = "SERVICES",
    "RISK" = "RISK"
)
dimension_fill_colors <- c(
    "INPUT" = "#7895A5",
    "SERVICES" = "#8FA596",
    "RISK" = "#B88989"
)
dimension_outline_colors <- c(
    "INPUT" = "#5F7D8C",
    "SERVICES" = "#718878",
    "RISK" = "#996A6A"
)
type_order <- c("CP", "SU", "GSP", "RPU", "PIU", "RU")
type_headers <- setNames(
    sprintf("(%s) %s", letters[seq_along(type_order)], type_order),
    type_order
)
level_order <- c("overall", "high", "medium", "low")
inner_radius <- 0.15
sector_gap <- 0.055
reference_radii <- c(0.25, 0.50, 0.75, 1.00)
large_to_small_width <- 2.6
small_stack_gap_width <- 0.12
overall_xlim <- c(-1.15, 1.25)
overall_ylim <- c(-1.20, 1.20)
small_xlim <- c(-1.05, 1.05)
small_ylim <- c(-1.05, 1.05)
small_label_x <- -0.90
small_label_y <- 1.03

safe_mean <- function(x) {
    if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

raw_data <- read_csv(
    "sync/results/unsbm_exp_slack_rate.csv",
    show_col_types = FALSE
) %>%
    select(type, score, all_of(metric_columns)) %>%
    filter(!is.na(score), score < 0.9999, type %in% type_order)

if (nrow(raw_data) == 0) {
    stop("No non-frontier observations with score < 0.9999 were found.")
}

classified_data <- raw_data %>%
    group_by(type) %>%
    group_modify(~ {
        if (dplyr::n_distinct(.x$score) < 3) {
            stop(sprintf("Type %s has fewer than three distinct scores.", .y$type))
        }
        breaks <- classInt::classIntervals(.x$score, n = 3, style = "jenks")$brks
        .x %>%
            mutate(
                level = cut(
                    score,
                    breaks = breaks,
                    labels = c("low", "medium", "high"),
                    include.lowest = TRUE,
                    right = TRUE
                ),
                level_score_min = case_when(
                    level == "low" ~ breaks[1],
                    level == "medium" ~ breaks[2],
                    level == "high" ~ breaks[3]
                ),
                level_score_max = case_when(
                    level == "low" ~ breaks[2],
                    level == "medium" ~ breaks[3],
                    level == "high" ~ breaks[4]
                )
            )
    }) %>%
    ungroup()

level_counts <- classified_data %>% count(type, level)
if (nrow(level_counts) != length(type_order) * 3 || any(level_counts$n == 0)) {
    stop("Each UGS type must contain observations in all three Jenks levels.")
}

overall_summary <- classified_data %>%
    group_by(type) %>%
    summarise(
        level = "overall",
        score_min = min(score, na.rm = TRUE),
        score_max = max(score, na.rm = TRUE),
        across(all_of(metrics), safe_mean),
        .groups = "drop"
    )

level_summary <- classified_data %>%
    group_by(type, level) %>%
    summarise(
        score_min = first(level_score_min),
        score_max = first(level_score_max),
        across(all_of(metrics), safe_mean),
        .groups = "drop"
    )

summary_data <- bind_rows(overall_summary, level_summary) %>%
    mutate(
        type = factor(type, levels = type_order),
        level = factor(level, levels = level_order)
    ) %>%
    arrange(type, level)

radar_data_scaled <- summary_data %>%
    mutate(across(all_of(metrics), ~ {
        metric_max <- max(.x, na.rm = TRUE)
        if (!is.finite(metric_max)) {
            rep(NA_real_, length(.x))
        } else if (metric_max == 0) {
            ifelse(is.na(.x), NA_real_, 0)
        } else {
            .x / metric_max
        }
    }))

readr::write_csv(
    radar_data_scaled %>% mutate(type = as.character(type), level = as.character(level)),
    "article/Figures/radar_data.csv"
)

create_radar_plot <- function(data, title, show_labels = TRUE, show_legend = FALSE) {
    data_long <- data %>%
        select(all_of(metrics)) %>%
        pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
        mutate(
            metric = factor(metric, levels = metrics),
            dimension = unname(metric_dimensions[as.character(metric)])
        )

    n_metrics <- length(metrics)
    angles <- (pi / 2) - (2 * pi * (seq_len(n_metrics) - 1) / n_metrics)
    sector_width <- (2 * pi / n_metrics) - sector_gap
    angle_df <- tibble(metric = factor(metrics, levels = metrics), angle = angles)

    data_long <- data_long %>%
        left_join(angle_df, by = "metric") %>%
        filter(!is.na(value)) %>%
        mutate(outer_radius = inner_radius + value * (1 - inner_radius), petal_id = metric)

    petal_data <- pmap_dfr(
        data_long %>% select(metric, dimension, angle, outer_radius, petal_id),
        function(metric, dimension, angle, outer_radius, petal_id) {
            arc_angles <- seq(angle - sector_width / 2, angle + sector_width / 2, length.out = 18)
            tibble(
                metric = metric,
                dimension = dimension,
                petal_id = petal_id,
                x = c(outer_radius * cos(arc_angles), inner_radius * cos(rev(arc_angles))),
                y = c(outer_radius * sin(arc_angles), inner_radius * sin(rev(arc_angles)))
            )
        }
    )

    outline_data <- pmap_dfr(
        data_long %>% select(metric, dimension, angle, outer_radius, petal_id),
        function(metric, dimension, angle, outer_radius, petal_id) {
            start_angle <- angle - sector_width / 2
            end_angle <- angle + sector_width / 2
            arc_angles <- seq(start_angle, end_angle, length.out = 18)
            tibble(
                metric = metric,
                dimension = dimension,
                petal_id = petal_id,
                x = c(inner_radius * cos(start_angle), outer_radius * cos(arc_angles), inner_radius * cos(end_angle)),
                y = c(inner_radius * sin(start_angle), outer_radius * sin(arc_angles), inner_radius * sin(end_angle))
            )
        }
    )

    circle_data <- expand_grid(angle = seq(0, 2 * pi, length.out = 160), radius = reference_radii) %>%
        mutate(x = radius * cos(angle), y = radius * sin(angle))

    label_data <- tibble(
        x = 1.13 * cos(angles),
        y = 1.13 * sin(angles),
        metric = unname(metric_display_labels[metrics]),
        hjust = if_else(metrics == "SB", 1.5, 0.5)
    )
    radius_label_angle <- -pi / 8
    radius_label_data <- tibble(
        x = (reference_radii + 0.03) * cos(radius_label_angle),
        y = (reference_radii + 0.03) * sin(radius_label_angle),
        label = c("0.25", "0.50", "0.75", "1.00")
    )

    plot <- ggplot() +
        geom_path(data = circle_data, aes(x = x, y = y, group = radius), color = "grey82", linewidth = 0.25, linetype = "dashed") +
        geom_polygon(data = petal_data, aes(x = x, y = y, group = petal_id, fill = dimension), alpha = 0.75, color = NA) +
        geom_path(data = outline_data, aes(x = x, y = y, group = petal_id, color = dimension), linewidth = 0.25, lineend = "round", linejoin = "round") +
        scale_color_manual(values = dimension_outline_colors, guide = "none") +
        scale_fill_manual(
            values = dimension_fill_colors,
            breaks = c("INPUT", "SERVICES", "RISK"),
            labels = c("INPUT EXCESS", "SERVICE SHORTFALL", "RISK EXCESS"),
            name = NULL
        ) +
        coord_fixed(
            xlim = if (show_labels) overall_xlim else small_xlim,
            ylim = if (show_labels) overall_ylim else small_ylim,
            clip = "off"
        ) +
        labs(title = if (show_labels) title else NULL) +
        theme_void(base_family = RADAR_FONT_FAMILY) +
        theme(
            text = element_text(family = RADAR_FONT_FAMILY),
            plot.title = element_text(
                size = RADAR_GROUP_HEADER_SIZE_PT,
                face = "plain",
                hjust = 0,
                lineheight = 0.9
            ),
            plot.title.position = "plot",
            legend.position = if (show_legend) "bottom" else "none",
            legend.text = element_text(size = 8),
            legend.key.width = grid::unit(RADAR_LEGEND_KEY_WIDTH_MM, "mm"),
            legend.key.height = grid::unit(RADAR_LEGEND_KEY_HEIGHT_MM, "mm"),
            plot.margin = margin(
                RADAR_PLOT_MARGIN_VERTICAL_MM,
                RADAR_PLOT_MARGIN_HORIZONTAL_MM,
                RADAR_PLOT_MARGIN_VERTICAL_MM,
                RADAR_PLOT_MARGIN_HORIZONTAL_MM
            )
        )

    if (show_labels) {
        plot <- plot +
            geom_text(
                data = label_data,
                aes(x = x, y = y, label = metric, hjust = hjust),
                family = RADAR_FONT_FAMILY,
                size = RADAR_METRIC_LABEL_SIZE_PT / ggplot2::.pt,
                parse = TRUE,
                vjust = 0.5
            ) +
            geom_text(
                data = radius_label_data,
                aes(x = x, y = y, label = label),
                family = RADAR_FONT_FAMILY,
                size = RADAR_RADIUS_LABEL_SIZE_PT / ggplot2::.pt,
                color = "grey60",
                hjust = 0
            )
    } else if (!is.null(title)) {
        plot <- plot +
            annotate(
                "text",
                x = small_label_x,
                y = small_label_y,
                label = title,
                family = RADAR_FONT_FAMILY,
                size = RADAR_SMALL_LABEL_SIZE_PT / ggplot2::.pt,
                fontface = "plain",
                lineheight = 0.9,
                hjust = 1,
                vjust = 1
            )
    }
    if (!show_legend) {
        plot <- plot + guides(fill = "none")
    }
    plot
}

type_group_plot <- function(type_name) {
    type_data <- radar_data_scaled %>% filter(type == type_name)
    overall <- type_data %>% filter(level == "overall")
    levels <- type_data %>% filter(level != "overall") %>% arrange(level)

    overall_plot <- create_radar_plot(
        overall,
        unname(type_headers[type_name]),
        show_labels = TRUE,
        show_legend = type_name == type_order[1]
    )
    level_plots <- lapply(c("high", "medium", "low"), function(level_name) {
        level_data <- levels %>% filter(level == level_name)
        interval <- sprintf("%.3f-%.3f", level_data$score_min, level_data$score_max)
        create_radar_plot(
            level_data,
            paste0(str_to_title(level_name), "\n", interval),
            show_labels = FALSE
        )
    })

    overall_plot +
        patchwork::plot_spacer() +
        (level_plots[[1]] / level_plots[[2]] / level_plots[[3]]) +
        patchwork::plot_layout(widths = c(large_to_small_width, small_stack_gap_width, 1))
}

type_groups <- lapply(type_order, type_group_plot)
combined_radar_plot <-
    (type_groups[[1]] | type_groups[[2]] | type_groups[[3]]) /
    (type_groups[[4]] | type_groups[[5]] | type_groups[[6]]) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

ggsave(
    filename = RADAR_PDF_OUTPUT_PATH,
    plot = combined_radar_plot,
    device = grDevices::cairo_pdf,
    width = RADAR_PLOT_WIDTH_MM,
    height = RADAR_PLOT_HEIGHT_MM,
    units = "mm",
    fallback_resolution = RADAR_PLOT_DPI
)

ggsave(
    filename = RADAR_PNG_OUTPUT_PATH,
    plot = combined_radar_plot,
    device = ragg::agg_png,
    width = RADAR_PLOT_WIDTH_MM,
    height = RADAR_PLOT_HEIGHT_MM,
    units = "mm",
    dpi = RADAR_PLOT_DPI,
    bg = "white"
)
