source("plot/init.R")

efficiency_threshold <- 0.9999
output_path <- "article/Tables/descriptive_table_data.csv"

type_labels <- c(
  RU = "Residential UGS (RU)",
  SU = "Streetside UGS (SU)",
  RPU = "Roadside and protection UGS (RPU)",
  PIU = "Public services and industry UGS (PIU)",
  GSP = "General and special Park (GSP)",
  CP = "Community Parks (CP)"
)

summarise_scores <- function(data, label) {
  data %>%
    summarise(
      Types = label,
      Count = n(),
      Min = min(score),
      Q25 = quantile(score, 0.25, names = FALSE),
      Median = median(score),
      Mean = mean(score),
      Q75 = quantile(score, 0.75, names = FALSE),
      Max = max(score)
    )
}

type_rows <- scores_df %>%
  mutate(type = factor(type, levels = names(type_labels))) %>%
  group_by(type) %>%
  summarise(
    Count = n(),
    Min = min(score),
    Q25 = quantile(score, 0.25, names = FALSE),
    Median = median(score),
    Mean = mean(score),
    Q75 = quantile(score, 0.75, names = FALSE),
    Max = max(score),
    .groups = "drop"
  ) %>%
  mutate(
    Types = unname(type_labels[as.character(type)]),
    .before = Count
  ) %>%
  select(Types, Count, Min, Q25, Median, Mean, Q75, Max)

descriptive_table <- bind_rows(
  type_rows,
  summarise_scores(
    filter(scores_df, score < efficiency_threshold),
    "Non-optimal samples"
  ),
  summarise_scores(scores_df, "Overall samples")
) %>%
  mutate(across(c(Min, Q25, Median, Mean, Q75, Max), ~ round(.x, 3)))

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(descriptive_table, output_path)

print(descriptive_table)
