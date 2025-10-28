library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)


df <- read_csv("processed_data/df_amazon_newpca.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined)) |>
  arrange(muni_id, year) |>
  group_by(muni_id) |>
  mutate(lag_garimpo_ha_change = lag(garimpo_ha_change, n=1)) |>
  filter(between(year, 2002, 2022))

df_share <- df %>%
  group_by(muni_id) %>%
  summarise(avg_garimpo_ha = mean(garimpo_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    total_avg_garimpo = sum(avg_garimpo_ha, na.rm = TRUE),
    share_zi0 = avg_garimpo_ha / total_avg_garimpo
  ) %>%
  select(muni_id, share_zi0)

sum(df_share$share_zi0)


df_bartik <- df |> 
  left_join(
    df_share,
    by = c("muni_id")
  )

####################################################
# using difference in gold price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_gold1 * share_zi0,
    bartik2 = shift_gold2 * share_zi0,
    bartik3 = shift_gold3 * share_zi0,
    bartik4 = shift_gold4 * share_zi0
  )

####################################################

## REGRESSION: Reduced form
# Join temporarily
df_model <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))

##### robustness checks ###

# without controls

ols_nocntrols <- feols(forest_loss_all_gross ~ garimpo_ha_change | year,
                       data = df_model)

second_stage1_nocntrols <- feols(forest_loss_all_gross ~ 1 | year | garimpo_ha_change ~ bartik,
                                 data = df_model)
summary(second_stage1_nocntrols)

second_stage2_nocntrols <- feols(forest_loss_all_gross ~ 1 | year | garimpo_ha_change ~ bartik2,
                                 data = df_model)
summary(second_stage2_nocntrols)

second_stage3_nocntrols <- feols(forest_loss_all_gross ~ 1 | year | garimpo_ha_change ~ bartik3,
                                 data = df_model)

summary(second_stage3_nocntrols)

second_stage4_nocntrols <- feols(forest_loss_all_gross ~ 1 | year | garimpo_ha_change ~ bartik4,
                                 data = df_model)

summary(second_stage4_nocntrols)

etable(
  list(
    "OLS No Controls"       = ols_nocntrols,
    "IV Lag 1 No Controls"  = second_stage1_nocntrols,
    "IV Lag 2 No Controls"  = second_stage2_nocntrols,
    "IV Lag 3 No Controls"  = second_stage3_nocntrols,
    "IV Lag 4 No Controls"  = second_stage4_nocntrols
  ),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change", "%forest_loss_all_gross"),
  headers = c("OLS No Controls", "IV Lag 1 No Controls", "IV Lag 2 No Controls", "IV Lag 3 No Controls", "IV Lag 4 No Controls"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex    = TRUE,
  title   = "Estimates without Controls",
  extralines = list(
    "Covariates" = c("None", "None", "None", "None", "None")
  )
)


# with mining lag

ols_model_lag <- feols(forest_loss_all_gross ~ lag_garimpo_ha_change + spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year,
                       data = df_model)

# second stage for change_in_area
second_stage1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | lag_garimpo_ha_change ~ bartik,
                       data = df_model)
summary(second_stage1)

second_stage2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | lag_garimpo_ha_change ~ bartik2,
                       data = df_model)
summary(second_stage2)

second_stage3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | lag_garimpo_ha_change ~ bartik3,
                       data = df_model)
summary(second_stage3)

second_stage4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | lag_garimpo_ha_change ~ bartik4,
                       data = df_model)
summary(second_stage4)

second_stage_models_ols <- list(
  "OLS" = ols_model_lag,
  "t-1" = second_stage1,
  "t-2" = second_stage2,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)

etable(
  second_stage_models_ols,
  dict = c(
    lag_garimpo_ha_change     = "Lag Change in Garimpo Area",
    fit_lag_garimpo_ha_change = "Lag Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%lag_garimpo_ha_change", "%fit_lag_garimpo_ha_change"),
  headers = c("OLS", "t-1", "t-2", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full", "Full")
  )
)

# 2001 share as instrument

df <- read_csv("processed_data/df_amazon_newpca.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined)) |> 
  filter(between(year, 2001, 2022))

df_share <- df %>%
  group_by(year) %>%
  mutate(total_yearly_mining_area = sum(garimpo_ha, na.rm = TRUE)) %>%
  ungroup()|> 
  mutate(
    share_zi0 = garimpo_ha / total_yearly_mining_area
  ) |> 
  filter(year == 2001) |>
  select(muni_id, year, share_zi0)

sum(df_share$share_zi0)


df_bartik <- df |> 
  left_join(
    df_share,
    by = c("muni_id")
  ) |>
  rename(
    year = year.x
  ) |> 
  select(-c(year.y)) |>
  filter(between(year, 2002, 2022))

####################################################
# using difference in gold price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_gold1 * share_zi0,
    bartik2 = shift_gold2 * share_zi0,
    bartik3 = shift_gold3 * share_zi0,
    bartik4 = shift_gold4 * share_zi0
  )

## REGRESSION: Reduced form
# Join temporarily
df_model <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))

####################################


###### SECOND STAGE #########

# second stage for change_in_area
second_stage1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik,
                       data = df_model)
summary(second_stage1)

second_stage2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik2,
                       data = df_model)
summary(second_stage2)

second_stage3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                       data = df_model)
summary(second_stage3)

second_stage4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                       data = df_model)
summary(second_stage4)

# Combine second-stage models into a list
second_stage_models_change <- list(
  "t-1" = second_stage1,
  "t-2" = second_stage2,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)
# Create a summary table for second-stage models
modelsummary(
  second_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "fit_garimpo_ha_change" = "Change in Garimpo Area"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

####### OLS #########
ols_model <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change | year,
                   data = df_model)

second_stage_models_ols <- list(
  "OLS" = ols_model,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)

etable(
  second_stage_models_ols,
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("OLS", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full")
  )
)




