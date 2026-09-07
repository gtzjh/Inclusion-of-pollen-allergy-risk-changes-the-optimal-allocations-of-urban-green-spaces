rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(gghalves)
library(patchwork)

color_palette <- c(
    "PIU" = "#B47B84",
    "RU" = "#6295A2",
    "RPU" = "#7895CB",
    "GSP" = "#c0c4c2",
    "CP" = "#40A578",
    "SU" = "#A0C49D"
)

scores_df <- readr::read_csv(
    "sync/results/unsbm_exp_slack_rate.csv",
    col_types = readr::cols_only(
        sub_id = readr::col_character(),
        score = readr::col_double(),
        type = readr::col_character()
    ),
    show_col_types = FALSE
) %>%
    select(sub_id, type, score)

base_scores_df <- readr::read_csv(
    "sync/results/unsbm_base_slack_rate.csv",
    col_types = readr::cols_only(
        sub_id = readr::col_character(),
        score = readr::col_double(),
        type = readr::col_character()
    ),
    show_col_types = FALSE
) %>%
    select(sub_id, type, score)
