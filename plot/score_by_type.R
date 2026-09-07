source("plot/init.R")

efficiency_threshold <- 0.9999
pnas_output_width_mm <- 87
pnas_output_height_mm <- 135
value_label_size_mm <- 3.2
panel_horizontal_margin_mm <- 0.5
font_family <- "Arial"
base_text_size_pt <- 12
panel_title_size_pt <- 12
panel_title_bottom_margin_mm <- 1
pdf_output_file <- "article/Figures/score_by_type.pdf"
png_output_file <- "article/Figures/score_by_type.png"
png_output_dpi <- 600


score_below_threshold <- scores_df %>%
    filter(score < efficiency_threshold)
print(count(score_below_threshold))
print(formatC(summary(score_below_threshold$score), format = "f", digits = 3))

print(formatC(summary(scores_df$score), format = "f", digits = 3))



score_median <- aggregate(score ~ type, data = scores_df, FUN = median)
scores_df$type <- scores_df$type %>%
  factor(levels = score_median$type[order(score_median$score, decreasing = TRUE)])

global_median_values <- scores_df %>%
  group_by(type) %>%
  summarise(median_score = median(score, na.rm = TRUE))

readr::write_csv(
  scores_df %>%
    select(sub_id, type, score) %>%
    arrange(type),
  "article/Figures/score_by_type_a_data.csv"
)

type_score_global <- scores_df %>%
  ggplot(aes(x = type, y = score, color = type, fill = type)) +
  geom_jitter(width = .2, alpha = .6, size = 2) +
  geom_text(data = global_median_values,
            aes(x = type, y = max(scores_df$score) * .9,
            label = sprintf("%.3f", median_score)),
            color = "black", size = value_label_size_mm, vjust = 0,
            family = font_family) +
  scale_color_manual(values = color_palette, guide = "none") +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(x = NULL, y = NULL, title = "(a)") +
  guides(color = "none", fill = "none") +
  theme_bw(base_size = base_text_size_pt, base_family = font_family) +
  theme(
    text = element_text(size = base_text_size_pt, family = font_family),
    plot.margin = margin(0, panel_horizontal_margin_mm, 0,
                         panel_horizontal_margin_mm, unit = "mm"),
    plot.title = element_text(
      size = panel_title_size_pt,
      margin = margin(b = panel_title_bottom_margin_mm, unit = "mm")
    )
  )


efficient_counts <- scores_df %>%
  group_by(type) %>%
  summarise(
    total_samples = n(),
    efficient_samples = sum(score >= efficiency_threshold),
    proportion = efficient_samples / total_samples
  ) %>%
  arrange(desc(proportion))

efficient_counts$type <- factor(efficient_counts$type, levels = efficient_counts$type)

readr::write_csv(
  efficient_counts %>%
    select(type, total_samples, efficient_samples, proportion),
  "article/Figures/score_by_type_b_data.csv"
)

type_efficient_proportion <- ggplot(efficient_counts, aes(x = type, y = proportion, fill = type)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f%%", proportion * 100), y = proportion + 0.05),
            color = "black", size = value_label_size_mm, vjust = 1,
            family = font_family) +
  scale_fill_manual(values = color_palette, guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, max(efficient_counts$proportion) * 1.2)) +
  labs(x = NULL, y = NULL, title = "(b)") +
  theme_bw(base_size = base_text_size_pt, base_family = font_family) +
  theme(
    text = element_text(size = base_text_size_pt, family = font_family),
    plot.margin = margin(0, panel_horizontal_margin_mm, 0,
                         panel_horizontal_margin_mm, unit = "mm"),
    plot.title = element_text(
      size = panel_title_size_pt,
      margin = margin(b = panel_title_bottom_margin_mm, unit = "mm")
    )
  )


inefficient_scores_df <- scores_df %>%
  filter(score < efficiency_threshold)

inefficient_score_median <- aggregate(score ~ type, data = inefficient_scores_df, FUN = median)
inefficient_scores_df$type <- inefficient_scores_df$type %>%
  factor(levels = inefficient_score_median$type[order(inefficient_score_median$score,
  decreasing = TRUE)])

inefficient_median_values <- inefficient_scores_df %>%
  group_by(type) %>%
  summarise(median_score = median(score, na.rm = TRUE))

readr::write_csv(
  inefficient_scores_df %>%
    select(sub_id, type, score) %>%
    arrange(type),
  "article/Figures/score_by_type_c_data.csv"
)

type_score_inefficient <- inefficient_scores_df %>%
  ggplot(aes(x = type, y = score, color = type, fill = type)) +
  geom_jitter(width = .2, alpha = .6, size = 2) + 
  geom_boxplot(width = .2, linewidth = 0.3,
               position = position_nudge(x = .3), alpha = .6) +
  geom_text(data = inefficient_median_values,
            aes(x = type, y = max(inefficient_scores_df$score) * .9,
            label = sprintf("%.3f", median_score)),
            color = "black", size = value_label_size_mm, vjust = 0,
            family = font_family) +
  scale_color_manual(values = color_palette, guide = "none") +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(x = NULL, y = NULL, title = "(c)") +
  guides(color = "none", fill = "none") +
  theme_bw(base_size = base_text_size_pt, base_family = font_family) +
  theme(
    text = element_text(size = base_text_size_pt, family = font_family),
    plot.margin = margin(0, panel_horizontal_margin_mm, 0,
                         panel_horizontal_margin_mm, unit = "mm"),
    plot.title = element_text(
      size = panel_title_size_pt,
      margin = margin(b = panel_title_bottom_margin_mm, unit = "mm")
    )
  )


combined_figure <- type_score_global + type_efficient_proportion + type_score_inefficient +
  plot_layout(nrow = 3, guides = "collect")

ggsave(pdf_output_file,
       combined_figure,
       device = grDevices::cairo_pdf,
       width = pnas_output_width_mm,
       height = pnas_output_height_mm,
       units = "mm",
       fallback_resolution = png_output_dpi)

ggsave(png_output_file,
       combined_figure,
       device = ragg::agg_png,
       width = pnas_output_width_mm,
       height = pnas_output_height_mm,
       units = "mm",
       dpi = png_output_dpi,
       bg = "white")
