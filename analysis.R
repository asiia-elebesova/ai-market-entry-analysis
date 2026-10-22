# Strategic Use of AI in International Market Entry: analysis script
# MSc thesis, FAU Erlangen-Nuremberg (Chair of Information Systems I), 2026
# Author: Asiia Elebesova
#
# Steps: load the quarterly dataset -> normalize indicators to 0-1 -> build composite indices
# -> descriptive statistics -> Pearson and Spearman correlations -> OLS models -> robustness check
# -> export all tables to outputs/analysis_results.xlsx
# Run from the repository root so that the relative paths work.

library(readxl)
library(dplyr)

file_path <- "data/analysis_dataset.xlsx"
raw <- read_excel(file_path, sheet = "Sheet1")
glimpse(raw)

raw %>%
  filter(firm == "Tesla", period == "2024Q2") %>%
  select(firm, period, ratings_volume, rating_velocity,
         positive_share, average_rating, engagement_available)

minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

scale_0_3 <- function(x) {
  x / 3
}

rating_norm <- function(x) {
  (x - 1) / 4
}

norm <- raw %>%
  mutate(
    ai_features_added_norm = minmax(ai_features_added),
    ota_software_change_intensity_norm = minmax(ota_software_change_intensity),
    app_ecosystem_depth_norm = scale_0_3(app_ecosystem_depth),
    
    product_service_adaptation_norm = scale_0_3(product_service_adaptation),
    local_language_ux_ui_norm = scale_0_3(local_language_ux_ui),
    local_production_facilities_norm = local_production_facilities,
    local_data_center_norm = local_data_center,
    local_rd_design_hub_norm = local_rd_design_hub,
    
    compliance_statements_norm = scale_0_3(compliance_statements),
    ota_data_localization_compliance_norm = scale_0_3(ota_data_localization_compliance),
    regulatory_approvals_norm = scale_0_3(regulatory_approvals),
    compliance_certifications_norm = scale_0_3(compliance_certifications),
    
    ratings_volume_norm = minmax(ratings_volume),
    rating_velocity_norm = minmax(rating_velocity),
    positive_share_norm = positive_share,
    average_rating_norm = rating_norm(average_rating),
    
    sales_volume_norm = minmax(sales_volume),
    ev_market_share_norm = minmax(ev_market_share),
    brand_ranking_score_norm = brand_ranking_score,
    
    gdp_nominal_trillion_usd_norm = minmax(gdp_nominal_trillion_usd),
    passenger_car_sales_units_norm = minmax(passenger_car_sales_units)
  )

# View(norm)

norm %>%
  filter(firm == "Tesla", period %in% c("2024Q2", "2024Q3", "2025Q4")) %>%
  select(firm, period,
         ratings_volume_norm, rating_velocity_norm,
         positive_share_norm, average_rating_norm)

scores_step1 <- norm %>%
  mutate(
    Infrastructure_norm = rowMeans(
      select(., local_production_facilities_norm,
             local_data_center_norm,
             local_rd_design_hub_norm),
      na.rm = FALSE
    ),
    
    Regulatory_Fit_norm = rowMeans(
      select(., compliance_statements_norm,
             ota_data_localization_compliance_norm,
             regulatory_approvals_norm,
             compliance_certifications_norm),
      na.rm = FALSE
    ),
    
    Platform_Engagement_norm = rowMeans(
      select(., ratings_volume_norm,
             rating_velocity_norm),
      na.rm = FALSE
    ),
    
    Sentiment_norm = rowMeans(
      select(., positive_share_norm,
             average_rating_norm),
      na.rm = FALSE
    )
  )

# View(scores_step1)

scores_step1 %>%
  filter(firm == "Tesla", period %in% c("2024Q2", "2024Q3", "2025Q4")) %>%
  select(firm, period,
         Infrastructure_norm,
         Regulatory_Fit_norm,
         Platform_Engagement_norm,
         Sentiment_norm)

scores <- scores_step1 %>%
  mutate(
    AI_Integration = rowMeans(
      select(., ai_features_added_norm,
             ota_software_change_intensity_norm,
             app_ecosystem_depth_norm),
      na.rm = FALSE
    ),
    
    Localization_Depth = rowMeans(
      select(., product_service_adaptation_norm,
             local_language_ux_ui_norm,
             Infrastructure_norm,
             Regulatory_Fit_norm),
      na.rm = FALSE
    ),
    
    User_Engagement = rowMeans(
      select(., Platform_Engagement_norm,
             Sentiment_norm),
      na.rm = FALSE
    ),
    
    Market_Success = rowMeans(
      select(., sales_volume_norm,
             ev_market_share_norm,
             brand_ranking_score_norm),
      na.rm = FALSE
    ),
    
    Host_Market_Capacity = rowMeans(
      select(., gdp_nominal_trillion_usd_norm,
             passenger_car_sales_units_norm),
      na.rm = FALSE
    )
  )

# View(scores)

scores %>%
  filter(
    (firm == "Tesla" & period %in% c("2024Q2", "2024Q3", "2025Q4")) |
      (firm == "BYD" & period %in% c("2024Q2", "2025Q4"))
  ) %>%
  select(firm, period,
         AI_Integration,
         Localization_Depth,
         User_Engagement,
         Market_Success,
         Host_Market_Capacity)


descriptive_stats <- scores %>%
  summarise(
    AI_n = sum(!is.na(AI_Integration)),
    AI_missing = sum(is.na(AI_Integration)),
    AI_mean = mean(AI_Integration, na.rm = TRUE),
    AI_sd = sd(AI_Integration, na.rm = TRUE),
    AI_min = min(AI_Integration, na.rm = TRUE),
    AI_max = max(AI_Integration, na.rm = TRUE),
    
    LOC_n = sum(!is.na(Localization_Depth)),
    LOC_missing = sum(is.na(Localization_Depth)),
    LOC_mean = mean(Localization_Depth, na.rm = TRUE),
    LOC_sd = sd(Localization_Depth, na.rm = TRUE),
    LOC_min = min(Localization_Depth, na.rm = TRUE),
    LOC_max = max(Localization_Depth, na.rm = TRUE),
    
    UE_n = sum(!is.na(User_Engagement)),
    UE_missing = sum(is.na(User_Engagement)),
    UE_mean = mean(User_Engagement, na.rm = TRUE),
    UE_sd = sd(User_Engagement, na.rm = TRUE),
    UE_min = min(User_Engagement, na.rm = TRUE),
    UE_max = max(User_Engagement, na.rm = TRUE),
    
    MS_n = sum(!is.na(Market_Success)),
    MS_missing = sum(is.na(Market_Success)),
    MS_mean = mean(Market_Success, na.rm = TRUE),
    MS_sd = sd(Market_Success, na.rm = TRUE),
    MS_min = min(Market_Success, na.rm = TRUE),
    MS_max = max(Market_Success, na.rm = TRUE),
    
    HMC_n = sum(!is.na(Host_Market_Capacity)),
    HMC_missing = sum(is.na(Host_Market_Capacity)),
    HMC_mean = mean(Host_Market_Capacity, na.rm = TRUE),
    HMC_sd = sd(Host_Market_Capacity, na.rm = TRUE),
    HMC_min = min(Host_Market_Capacity, na.rm = TRUE),
    HMC_max = max(Host_Market_Capacity, na.rm = TRUE)
  )
descriptive_stats

descriptive_table <- tibble::tibble(
  Variable = c(
    "AI Integration",
    "Localization Depth",
    "User Engagement",
    "Market Success",
    "Host-Market Capacity"
  ),
  n = c(
    descriptive_stats$AI_n,
    descriptive_stats$LOC_n,
    descriptive_stats$UE_n,
    descriptive_stats$MS_n,
    descriptive_stats$HMC_n
  ),
  missing = c(
    descriptive_stats$AI_missing,
    descriptive_stats$LOC_missing,
    descriptive_stats$UE_missing,
    descriptive_stats$MS_missing,
    descriptive_stats$HMC_missing
  ),
  mean = c(
    descriptive_stats$AI_mean,
    descriptive_stats$LOC_mean,
    descriptive_stats$UE_mean,
    descriptive_stats$MS_mean,
    descriptive_stats$HMC_mean
  ),
  sd = c(
    descriptive_stats$AI_sd,
    descriptive_stats$LOC_sd,
    descriptive_stats$UE_sd,
    descriptive_stats$MS_sd,
    descriptive_stats$HMC_sd
  ),
  min = c(
    descriptive_stats$AI_min,
    descriptive_stats$LOC_min,
    descriptive_stats$UE_min,
    descriptive_stats$MS_min,
    descriptive_stats$HMC_min
  ),
  max = c(
    descriptive_stats$AI_max,
    descriptive_stats$LOC_max,
    descriptive_stats$UE_max,
    descriptive_stats$MS_max,
    descriptive_stats$HMC_max
  )
)
descriptive_table



main_vars <- scores[, c(
  "AI_Integration",
  "Localization_Depth",
  "User_Engagement",
  "Market_Success",
  "Host_Market_Capacity"
)]

pearson_matrix <- cor(
  main_vars,
  use = "pairwise.complete.obs",
  method = "pearson"
)

spearman_matrix <- cor(
  main_vars,
  use = "pairwise.complete.obs",
  method = "spearman"
)
round(pearson_matrix, 3)
round(spearman_matrix, 3)


model1 <- lm(Localization_Depth ~ AI_Integration, data = scores)
summary(model1)

model2 <- lm(User_Engagement ~ AI_Integration + Localization_Depth, data = scores)
summary(model2)

model3 <- lm(Market_Success ~ User_Engagement + Host_Market_Capacity, data = scores)
summary(model3)

model3_robust <- scores %>%
  filter(!(firm == "Tesla" & period == "2025Q4")) %>%
  lm(Market_Success ~ User_Engagement + Host_Market_Capacity, data = .)
summary(model3_robust)

# install.packages("writexl")  # run once if needed
library(writexl)

normalized_indicators <- norm %>%
  select(
    firm, market, period, year, quarter,
    ai_features_added_norm,
    ota_software_change_intensity_norm,
    app_ecosystem_depth_norm,
    product_service_adaptation_norm,
    local_language_ux_ui_norm,
    local_production_facilities_norm,
    local_data_center_norm,
    local_rd_design_hub_norm,
    compliance_statements_norm,
    ota_data_localization_compliance_norm,
    regulatory_approvals_norm,
    compliance_certifications_norm,
    ratings_volume_norm,
    rating_velocity_norm,
    positive_share_norm,
    average_rating_norm,
    sales_volume_norm,
    ev_market_share_norm,
    brand_ranking_score_norm,
    gdp_nominal_trillion_usd_norm,
    passenger_car_sales_units_norm
  )

composite_scores <- scores %>%
  select(
    firm, market, period, year, quarter,
    AI_Integration,
    Localization_Depth,
    User_Engagement,
    Market_Success,
    Host_Market_Capacity,
    Infrastructure_norm,
    Regulatory_Fit_norm,
    Platform_Engagement_norm,
    Sentiment_norm
  )

descriptive_table <- tibble::tibble(
  Variable = c(
    "AI Integration",
    "Localization Depth",
    "User Engagement",
    "Market Success",
    "Host-Market Capacity"
  ),
  n = c(
    sum(!is.na(scores$AI_Integration)),
    sum(!is.na(scores$Localization_Depth)),
    sum(!is.na(scores$User_Engagement)),
    sum(!is.na(scores$Market_Success)),
    sum(!is.na(scores$Host_Market_Capacity))
  ),
  missing = c(
    sum(is.na(scores$AI_Integration)),
    sum(is.na(scores$Localization_Depth)),
    sum(is.na(scores$User_Engagement)),
    sum(is.na(scores$Market_Success)),
    sum(is.na(scores$Host_Market_Capacity))
  ),
  mean = c(
    mean(scores$AI_Integration, na.rm = TRUE),
    mean(scores$Localization_Depth, na.rm = TRUE),
    mean(scores$User_Engagement, na.rm = TRUE),
    mean(scores$Market_Success, na.rm = TRUE),
    mean(scores$Host_Market_Capacity, na.rm = TRUE)
  ),
  sd = c(
    sd(scores$AI_Integration, na.rm = TRUE),
    sd(scores$Localization_Depth, na.rm = TRUE),
    sd(scores$User_Engagement, na.rm = TRUE),
    sd(scores$Market_Success, na.rm = TRUE),
    sd(scores$Host_Market_Capacity, na.rm = TRUE)
  ),
  min = c(
    min(scores$AI_Integration, na.rm = TRUE),
    min(scores$Localization_Depth, na.rm = TRUE),
    min(scores$User_Engagement, na.rm = TRUE),
    min(scores$Market_Success, na.rm = TRUE),
    min(scores$Host_Market_Capacity, na.rm = TRUE)
  ),
  max = c(
    max(scores$AI_Integration, na.rm = TRUE),
    max(scores$Localization_Depth, na.rm = TRUE),
    max(scores$User_Engagement, na.rm = TRUE),
    max(scores$Market_Success, na.rm = TRUE),
    max(scores$Host_Market_Capacity, na.rm = TRUE)
  )
)

pearson_table <- as.data.frame(round(pearson_matrix, 3)) %>%
  tibble::rownames_to_column("Variable")

spearman_table <- as.data.frame(round(spearman_matrix, 3)) %>%
  tibble::rownames_to_column("Variable")

regression_table <- function(model, model_name) {
  
  coef_table <- as.data.frame(summary(model)$coefficients)
  
  coef_table$Variable <- rownames(coef_table)
  rownames(coef_table) <- NULL
  
  names(coef_table) <- c(
    "Estimate",
    "Std_Error",
    "t_value",
    "p_value",
    "Variable"
  )
  
  coef_table$Model <- model_name
  
  coef_table <- coef_table[, c(
    "Model",
    "Variable",
    "Estimate",
    "Std_Error",
    "t_value",
    "p_value"
  )]
  
  fit_table <- data.frame(
    Model = model_name,
    N = length(model$residuals),
    R_squared = summary(model)$r.squared,
    Adjusted_R_squared = summary(model)$adj.r.squared,
    Residual_SE = summary(model)$sigma,
    F_statistic = unname(summary(model)$fstatistic[1])
  )
  
  return(list(
    coefficients = coef_table,
    fit = fit_table
  ))
}


m1 <- regression_table(model1, "Model 1: Localization Depth")
m2 <- regression_table(model2, "Model 2: User Engagement")
m3 <- regression_table(model3, "Model 3: Market Success")
m3r <- regression_table(model3_robust, "Robustness: Model 3 excl. Tesla 2025Q4")


regression_coefficients <- bind_rows(
  m1$coefficients,
  m2$coefficients,
  m3$coefficients,
  m3r$coefficients
)
regression_fit <- bind_rows(
  m1$fit,
  m2$fit,
  m3$fit,
  m3r$fit
)
regression_coefficients
regression_fit


output_path <- "outputs/analysis_results.xlsx"

write_xlsx(
  list(
    raw_data = raw,
    normalized_indicators = normalized_indicators,
    composite_scores = composite_scores,
    descriptive_statistics = descriptive_table,
    pearson_correlations = pearson_table,
    spearman_correlations = spearman_table,
    regression_coefficients = regression_coefficients,
    regression_fit = regression_fit
  ),
  path = output_path
)

file.exists(output_path)


install.packages("car")

library(car)

vif(model2)

vif(model3)

vif(model3_robust)
