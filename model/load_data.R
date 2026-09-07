library(tidyverse)


allergy_risk_data <- read_csv(
    "sync/datapreparation/step5_allergy_risk/allergy_risk.csv",
    col_types = cols_only(
        sub_id = col_character(),
        allergy_risk_normalized = col_double()
    ),
    show_col_types = FALSE
) %>%
    rename(allergy_risk = allergy_risk_normalized)

population <- read_csv(
    "sync/datapreparation/step3_heatmap/heatmap.csv",
    col_types = cols_only(
        sub_id = col_character(),
        total = col_double()
    ),
    show_col_types = FALSE
) %>%
    rename(social_benefit = total)

eco_services <- read_csv(
    "sync/datapreparation/step6_eco/eco_services.csv",
    col_types = cols_only(
        sub_id = col_character(),
        ES_LA = col_double(),
        ES_CO2 = col_double(),
        ES_Q = col_double()
    ),
    show_col_types = FALSE
)

shannon <- read_csv(
    "sync/datapreparation/step7_shannon/shannon.csv",
    col_types = cols_only(
        sub_id = col_character(),
        shannon = col_double()
    ),
    show_col_types = FALSE
) %>%
    rename(diversity = shannon)

maintenance <- read_csv(
    "sync/datapreparation/step2_maintenance_cost/maintenance_cost.csv",
    col_types = cols_only(
        sub_id = col_character(),
        maintenance_cost = col_double()
    ),
    show_col_types = FALSE
) %>%
    group_by(sub_id) %>%
    summarise(maintenance_cost = sum(maintenance_cost), .groups = "drop")

land_prices <- read_csv(
    "sync/datapreparation/step1_locational_conditions/land_prices.csv",
    col_types = cols_only(
        sub_id = col_character(),
        land_prices = col_double()
    ),
    show_col_types = FALSE
) %>%
    rename(location_condition = land_prices)

data <- read_csv(
    "sync/datapreparation/dataclean/sample_data.csv",
    col_types = cols_only(
        sub_id = col_character(),
        lon = col_double(),
        lat = col_double(),
        type = col_character()
    ),
    show_col_types = FALSE
) %>%
    inner_join(allergy_risk_data, by = "sub_id") %>%
    inner_join(population, by = "sub_id") %>%
    inner_join(eco_services, by = "sub_id") %>%
    inner_join(shannon, by = "sub_id") %>%
    inner_join(maintenance, by = "sub_id") %>%
    inner_join(land_prices, by = "sub_id") %>%
    select(
        sub_id, lon, lat, type, allergy_risk, social_benefit,
        ES_LA, ES_CO2, ES_Q, diversity, maintenance_cost, location_condition
    )


print(data)
