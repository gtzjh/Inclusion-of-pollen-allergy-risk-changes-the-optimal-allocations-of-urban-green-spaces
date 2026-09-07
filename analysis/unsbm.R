library(tidyverse)
library(deaR)

source("model/load_data.R")

calculate_slack_rate <- function(slack, original_value) {
  if_else(original_value > 0, (slack / original_value) * 100, NA_real_)
}

inputs <- c("maintenance_cost", "location_condition")
outputs <- c(
  "social_benefit", "ES_LA", "ES_CO2", "ES_Q",
  "diversity", "allergy_risk"
)

dea_data <- make_deadata(
  data,
  dmus = "sub_id",
  inputs = inputs,
  outputs = outputs,
  ud_outputs = match("allergy_risk", outputs)
)

data_base <- data %>%
  select(-allergy_risk)
outputs_base <- setdiff(outputs, "allergy_risk")
dea_data_base <- make_deadata(
  data_base,
  dmus = "sub_id",
  inputs = inputs,
  outputs = outputs_base
)
results <- model_sbmeff(
  dea_data,
  orientation = "no",
  rts = "vrs"
)

results_base <- model_sbmeff(
  dea_data_base,
  orientation = "no",
  rts = "vrs"
)
eff_scores <- efficiencies(results)
eff_scores <- data.frame(sub_id = names(eff_scores),
                         score = as.numeric(eff_scores),
                         stringsAsFactors = FALSE)
eff_scores$sub_id <- gsub("^DMU", "", eff_scores$sub_id)

eff_scores_base <- efficiencies(results_base)
eff_scores_base <- data.frame(sub_id = names(eff_scores_base),
                              score = as.numeric(eff_scores_base),
                              stringsAsFactors = FALSE)
eff_scores_base$sub_id <- gsub("^DMU", "", eff_scores_base$sub_id)
slacks_df <- as.data.frame(slacks(results))
slacks_df$sub_id <- rownames(slacks_df)
slacks_df$sub_id <- gsub("^DMU", "", slacks_df$sub_id)
slacks_df <- slacks_df %>% select(sub_id, everything())

slacks_df_base <- as.data.frame(slacks(results_base))
slacks_df_base$sub_id <- rownames(slacks_df_base)
slacks_df_base$sub_id <- gsub("^DMU", "", slacks_df_base$sub_id)
slacks_df_base <- slacks_df_base %>% select(sub_id, everything())
eff_slacks_results <- inner_join(eff_scores, slacks_df, by = "sub_id")
eff_slacks_results_base <- inner_join(eff_scores_base, slacks_df_base, by = "sub_id")

print(summary(eff_slacks_results))

eff_slack_rate <- inner_join(eff_slacks_results, data, by = "sub_id") %>%
  mutate(
    rate_maintenance_cost = calculate_slack_rate(
      slack_input.maintenance_cost, maintenance_cost
    ),
    rate_location_condition = calculate_slack_rate(
      slack_input.location_condition, location_condition
    ),
    rate_social_benefit = calculate_slack_rate(
      slack_output.social_benefit, social_benefit
    ),
    rate_ES_LA = calculate_slack_rate(slack_output.ES_LA, ES_LA),
    rate_ES_CO2 = calculate_slack_rate(slack_output.ES_CO2, ES_CO2),
    rate_ES_Q = calculate_slack_rate(slack_output.ES_Q, ES_Q),
    rate_diversity = calculate_slack_rate(slack_output.diversity, diversity),
    rate_allergy_risk = calculate_slack_rate(
      slack_output.allergy_risk, allergy_risk
    )
  )

eff_slack_rate_base <- inner_join(eff_slacks_results_base, data_base, by = "sub_id") %>%
  mutate(
    rate_maintenance_cost = calculate_slack_rate(
      slack_input.maintenance_cost, maintenance_cost
    ),
    rate_location_condition = calculate_slack_rate(
      slack_input.location_condition, location_condition
    ),
    rate_social_benefit = calculate_slack_rate(
      slack_output.social_benefit, social_benefit
    ),
    rate_ES_LA = calculate_slack_rate(slack_output.ES_LA, ES_LA),
    rate_ES_CO2 = calculate_slack_rate(slack_output.ES_CO2, ES_CO2),
    rate_ES_Q = calculate_slack_rate(slack_output.ES_Q, ES_Q),
    rate_diversity = calculate_slack_rate(slack_output.diversity, diversity)
  )
print(summary(eff_slack_rate))
write_csv(eff_slack_rate, "sync/results/unsbm_exp_slack_rate.csv")
write_csv(eff_slack_rate_base, "sync/results/unsbm_base_slack_rate.csv")
