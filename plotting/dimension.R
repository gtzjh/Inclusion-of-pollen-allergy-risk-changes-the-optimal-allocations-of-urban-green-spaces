library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)

INPUT_RATE_COLUMNS <- c(
    "rate_maintenance_cost",
    "rate_location_condition"
)

SERVICES_RATE_COLUMNS <- c(
    "rate_social_benefit",
    "rate_ES_LA",
    "rate_ES_CO2",
    "rate_ES_Q",
    "rate_diversity"
)

UGS_COLORS <- c(
    "PIU" = "#B47B84",
    "RU" = "#6295A2",
    "RPU" = "#7895CB",
    "GSP" = "#C0C4C2",
    "CP" = "#40A578",
    "SU" = "#A0C49D"
)
UGS_FILL_COLORS <- grDevices::adjustcolor(UGS_COLORS, alpha.f = 0.5)

DIMENSION_LABELS <- c(
    "Input" = "Relative input excess (%)",
    "Services" = "Relative service shortfall (%)",
    "AllergyRisk" = "Relative pollen allergy risk\nexcess (%)"
)

INPUT_PATH <- "sync/results/unsbm_exp_slack_rate.csv"
PDF_OUTPUT_PATH <- "article/Figures/dimension.pdf"
PNG_OUTPUT_PATH <- "article/Figures/dimension.png"
DIMENSION_DATA_OUTPUT_PATH <- "article/Figures/dimension_data.csv"
AXIS_HEADROOM <- 0.08
DIMENSION_PLOT_WIDTH_MM <- 178
DIMENSION_PLOT_HEIGHT_MM <- 77
DIMENSION_PLOT_DPI <- 600
DIMENSION_FONT_FAMILY <- "Arial"
RISK_INCLUSIVE_SCORE_THRESHOLD <- 0.9999

slack_rate_data <- read_csv(
    INPUT_PATH,
    na = c("", "NA"),
    col_types = cols_only(
        sub_id = col_character(),
        score = col_double(),
        type = col_character(),
        rate_maintenance_cost = col_double(),
        rate_location_condition = col_double(),
        rate_social_benefit = col_double(),
        rate_ES_LA = col_double(),
        rate_ES_CO2 = col_double(),
        rate_ES_Q = col_double(),
        rate_diversity = col_double(),
        rate_allergy_risk = col_double()
    ),
    show_col_types = FALSE
) %>%
    filter(
        !is.na(score),
        score < RISK_INCLUSIVE_SCORE_THRESHOLD
    )

dimension_data <- slack_rate_data %>%
    mutate(
        Input = if_else(
            if_any(all_of(INPUT_RATE_COLUMNS), ~ is.na(.x)),
            NA_real_,
            rowMeans(across(all_of(INPUT_RATE_COLUMNS)))
        ),
        Services = if_else(
            if_any(all_of(SERVICES_RATE_COLUMNS), ~ is.na(.x)),
            NA_real_,
            rowMeans(across(all_of(SERVICES_RATE_COLUMNS)))
        ),
        AllergyRisk = rate_allergy_risk,
        type = factor(type, levels = names(UGS_COLORS))
    ) %>%
    select(sub_id, type, Input, Services, AllergyRisk)

type_means <- dimension_data %>%
    summarise(
        across(
            c(Input, Services, AllergyRisk),
            ~ mean(.x, na.rm = TRUE)
        ),
        .by = type
    )

write_csv(
    type_means %>% mutate(type = as.character(type)),
    DIMENSION_DATA_OUTPUT_PATH
)

dimension_limits <- vapply(
    type_means[c("Input", "Services", "AllergyRisk")],
    function(values) max(values, na.rm = TRUE) * (1 + AXIS_HEADROOM),
    numeric(1)
)

create_pair_plot <- function(
    data,
    x_column,
    y_column,
    panel_title,
    panel_tag
) {
    panel_data <- data %>%
        filter(
            !is.na(.data[[x_column]]),
            !is.na(.data[[y_column]]),
            !is.na(type)
        )

    ggplot() +
        geom_point(
            data = panel_data,
            aes(
                x = .data[[x_column]],
                y = .data[[y_column]],
                color = type,
                fill = type
            ),
            shape = 21,
            size = 4.2,
            stroke = 0.8
        ) +
        scale_color_manual(
            values = UGS_COLORS,
            breaks = names(UGS_COLORS),
            drop = FALSE,
            name = "UGS type"
        ) +
        scale_fill_manual(
            values = UGS_FILL_COLORS,
            breaks = names(UGS_COLORS),
            drop = FALSE,
            name = "UGS type"
        ) +
        guides(
            color = "none",
            fill = guide_legend(
                nrow = 1,
                byrow = TRUE,
                override.aes = list(
                    color = unname(UGS_COLORS),
                    fill = unname(UGS_FILL_COLORS)
                )
            )
        ) +
        scale_x_continuous(
            limits = c(0, dimension_limits[[x_column]]),
            expand = expansion(mult = 0)
        ) +
        scale_y_continuous(
            limits = c(0, dimension_limits[[y_column]]),
            expand = expansion(mult = 0)
        ) +
        labs(
            title = panel_title,
            tag = panel_tag,
            x = DIMENSION_LABELS[[x_column]],
            y = DIMENSION_LABELS[[y_column]]
        ) +
        theme_bw(base_size = 9, base_family = DIMENSION_FONT_FAMILY) +
        theme(
            aspect.ratio = 1,
            plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
            plot.tag = element_text(size = 11, face = "bold"),
            plot.tag.position = c(0.02, 0.98),
            axis.title = element_text(size = 9, face = "bold"),
            axis.text = element_text(size = 8),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(size = 8, face = "bold"),
            legend.text = element_text(size = 8),
            legend.key.width = grid::unit(4, "mm"),
            legend.spacing.x = grid::unit(1, "mm"),
            panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
            panel.grid.minor = element_line(color = "grey95", linewidth = 0.2),
            panel.border = element_rect(color = "grey35", linewidth = 0.5)
        )
}

input_eco_plot <- create_pair_plot(
    type_means,
    "Input",
    "Services",
    "Input excess versus\nservice shortfall",
    "(a)"
)

allergy_eco_plot <- create_pair_plot(
    type_means,
    "AllergyRisk",
    "Services",
    "Risk excess versus\nservice shortfall",
    "(b)"
)

allergy_input_plot <- create_pair_plot(
    type_means,
    "AllergyRisk",
    "Input",
    "Risk excess versus\ninput excess",
    "(c)"
)

get_legend <- function(plot) {
    plot_grob <- ggplotGrob(plot)
    legend_index <- which(vapply(
        plot_grob$grobs,
        function(grob) identical(grob$name, "guide-box"),
        logical(1)
    ))
    if (length(legend_index) == 0) {
        return(grid::nullGrob())
    }
    plot_grob$grobs[[legend_index[1]]]
}

build_combined_plot <- function() {
    ragg::agg_capture(
        width = DIMENSION_PLOT_WIDTH_MM,
        height = DIMENSION_PLOT_HEIGHT_MM,
        units = "mm",
        res = DIMENSION_PLOT_DPI,
        background = "white"
    )
    on.exit(grDevices::dev.off(), add = TRUE)

    shared_legend <- get_legend(input_eco_plot)
    plot_row <- arrangeGrob(
        input_eco_plot + theme(legend.position = "none"),
        allergy_eco_plot + theme(legend.position = "none"),
        allergy_input_plot + theme(legend.position = "none"),
        nrow = 1
    )
    arrangeGrob(
        plot_row,
        shared_legend,
        ncol = 1,
        heights = c(10, 1.2)
    )
}

combined_plot <- build_combined_plot()

ggsave(
    filename = PDF_OUTPUT_PATH,
    plot = combined_plot,
    device = grDevices::cairo_pdf,
    width = DIMENSION_PLOT_WIDTH_MM,
    height = DIMENSION_PLOT_HEIGHT_MM,
    units = "mm",
    fallback_resolution = DIMENSION_PLOT_DPI
)

ggsave(
    filename = PNG_OUTPUT_PATH,
    plot = combined_plot,
    device = ragg::agg_png,
    width = DIMENSION_PLOT_WIDTH_MM,
    height = DIMENSION_PLOT_HEIGHT_MM,
    units = "mm",
    dpi = DIMENSION_PLOT_DPI,
    bg = "white"
)
