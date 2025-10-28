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

######## dummy variables #########

## post 2014 dummy and para dummy

df_model <- df_model %>%
  mutate(
    good = ifelse(between(year, 2006, 2013), 1, 0),
    para = ifelse(substr(muni_id, 1, 2) == "15", 1, 0)
  )

######## PARA #############

df_para <- df_model %>%
  filter(para == 1)

df_nonpara <- df_model %>%
  filter(para == 0)

ols_para <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change | year,
                  data = df_para)

ols_nonpara <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change | year,
                     data = df_nonpara)

etable(
  list("Para" = ols_para, "Non-Para" = ols_nonpara),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("Para", "Non-Para"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – Para vs Non-Para",
  extralines = list(
    "Covariates" = c("Full", "Full")
  )
)

second_stage3_param <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                               population_change + pop_dens_change + pa_tot_ha_change + 
                               n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                             data = df_para)

summary(second_stage3_param)

second_stage3_nonparam <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                                data = df_nonpara)

summary(second_stage3_nonparam)

second_stage4_param <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                               population_change + pop_dens_change + pa_tot_ha_change + 
                               n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                             data = df_para)

summary(second_stage4_param)

second_stage4_nonparam <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                                data = df_nonpara)

summary(second_stage4_nonparam)

etable(
  list(
    "Lag 3 – Para"     = second_stage3_param, 
    "Lag 3 – Non-Para" = second_stage3_nonparam,
    "Lag 4 – Para"     = second_stage4_param, 
    "Lag 4 – Non-Para" = second_stage4_nonparam
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lag 3 – Para", "Lag 3 – Non-Para", "Lag 4 – Para", "Lag 4 – Non-Para"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Para vs Non-Para (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)


#full sample of para with interaction terms

stage3_para <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       year | 
                       para*garimpo_ha_change ~ para*bartik3,
                     data = df_model)

stage4_para <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       year | 
                       para*garimpo_ha_change ~ para*bartik4,
                     data = df_model)

etable(
  list(
    "Lag 3 – Para" = stage3_para, 
    "Lag 4 – Para" = stage4_para
  ),
  dict = c(
    "fit_garimpo_ha_change"       = "Change in Garimpo Area",
    "fit_para:garimpo_ha_change"  = "Change in Garimpo Area x Para",
    "fit_para"                    = "Para",
    "forest_loss_all_gross"       = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change", "%fit_para:garimpo_ha_change", "%fit_para"),
  headers = c("Lag 3 – Para", "Lag 4 – Para"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Para vs Non-Para (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full")
  )
)


############# 4 Presidenti #############

df_pres <- df_model |> 
  mutate(
    lula = ifelse(between(year, 2003, 2010), 1, 0),
    rousseff = ifelse(between(year, 2011, 2016), 1, 0),
    temer = ifelse(between(year, 2017, 2018), 1, 0),
    bolsonaro = ifelse(between(year, 2019, 2022), 1, 0)
  )

## lula

lula <- df_pres |> 
  filter(lula == 1)

ols_lula <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change | year,
                  data = lula)

## rousseff

rousseff <- df_pres |> 
  filter(rousseff == 1)

ols_rousseff <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change | year,
                      data = rousseff)

## temer

temer <- df_pres |> 
  filter(temer == 1)

ols_temer <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change +
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change | year,
                   data = temer)

## bolsonaro

bolsonaro <- df_pres |> 
  filter(bolsonaro == 1)

ols_bolsonaro <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year,
                       data = bolsonaro)

etable(
  list(
    "Lula"      = ols_lula, 
    "Rousseff"  = ols_rousseff,
    "Temer"     = ols_temer, 
    "Bolsonaro" = ols_bolsonaro
  ),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – By President",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)


ols_lula <- feols(forest_loss_all_gross ~ garimpo_ha_change*lula + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change,
                  #| #year,
                  data = df_pres)

ols_rousseff <- feols(forest_loss_all_gross ~ garimpo_ha_change*rousseff + spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change,
                      #| #year,
                      data = df_pres)

ols_temer <- feols(forest_loss_all_gross ~ garimpo_ha_change*temer + spei_dry + gdp_pc_change +
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change,
                   #| #year,
                   data = df_pres)

ols_bolsonaro <- feols(forest_loss_all_gross ~ garimpo_ha_change*bolsonaro + spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change,
                       #| #year,
                       data = df_pres)

## create etable with all presidents and ols_president

etable(
  list(
    "Lula"      = ols_lula, 
    "Rousseff"  = ols_rousseff,
    "Temer"     = ols_temer, 
    "Bolsonaro" = ols_bolsonaro
  ),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – By President",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)

## IV strategy 

## lula 

stage3_lula <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       #year | 
                       lula*garimpo_ha_change ~ lula*bartik3,
                     data = df_pres)

summary(stage3_lula)

stage3_luls <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change | 
                       year | 
                       garimpo_ha_change ~ bartik3,
                     data = lula)

summary(stage3_luls)

## rousseff

stage3_rousseff <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                           population_change + pop_dens_change + pa_tot_ha_change + 
                           n_fined_change + brl_fined_change |
                           #year | 
                           rousseff*garimpo_ha_change ~ rousseff*bartik3,
                         data = df_pres)

summary(stage3_rousseff)

stage3_rousseffs <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                            population_change + pop_dens_change + pa_tot_ha_change + 
                            n_fined_change + brl_fined_change | 
                            year | 
                            garimpo_ha_change ~ bartik3,
                          data = rousseff)

summary(stage3_rousseffs)



## temer

stage3_temer <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change |
                        #year | 
                        temer*garimpo_ha_change ~ temer*bartik3,
                      data = df_pres)

summary(stage3_temer)

stage3_temers <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | 
                         year | 
                         garimpo_ha_change ~ bartik3,
                       data = temer)

summary(stage3_temers)

## bolsonaro

stage3_bolsonaro <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                            population_change + pop_dens_change + pa_tot_ha_change + 
                            n_fined_change + brl_fined_change |
                            #year | 
                            bolsonaro*garimpo_ha_change ~ bolsonaro*bartik3,
                          data = df_pres)

summary(stage3_bolsonaro)


stage3_bolsonaros <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                             population_change + pop_dens_change + pa_tot_ha_change + 
                             n_fined_change + brl_fined_change | 
                             year | 
                             garimpo_ha_change ~ bartik3,
                           data = bolsonaro)

summary(stage3_bolsonaros)

## create etable with all presidents and stage3_president 

etable(
  list(
    "Lula"      = stage3_lula, 
    "Rousseff"  = stage3_rousseff,
    "Temer"     = stage3_temer, 
    "Bolsonaro" = stage3_bolsonaro
  ),
  dict = c(
    "fit_garimpo_ha_change"       = "Change in Garimpo Area",
    "fit_lula:garimpo_ha_change"  = "Change in Garimpo Area x Lula",
    "fit_lula"                    = "Lula",
    "fit_rousseff:garimpo_ha_change"  = "Change in Garimpo Area x Rousseff",
    "fit_rousseff"                    = "Rousseff",
    "fit_temer:garimpo_ha_change"  = "Change in Garimpo Area x Temer",
    "fit_temer"                    = "Temer",
    "fit_bolsonaro:garimpo_ha_change"  = "Change in Garimpo Area x Bolsonaro",
    "fit_bolsonaro"                    = "Bolsonaro",
    "forest_loss_all_gross"       = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change", "%fit_lula:garimpo_ha_change", "%fit_lula",
              "%fit_rousseff:garimpo_ha_change", "%fit_rousseff",
              "%fit_temer:garimpo_ha_change", "%fit_temer",
              "%fit_bolsonaro:garimpo_ha_change", "%fit_bolsonaro"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – By President (Lag 3)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)

## create etable with all presidents and stage3_presidents

etable(
  list(
    "Lula"      = stage3_luls, 
    "Rousseff"  = stage3_rousseffs,
    "Temer"     = stage3_temers, 
    "Bolsonaro" = stage3_bolsonaros
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – By President (Lag 3)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)

## STAGE 4 ##

stage4_luls <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       year | 
                       garimpo_ha_change ~ bartik4,
                     data = lula)

stage4_rousseffs <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                            population_change + pop_dens_change + pa_tot_ha_change + 
                            n_fined_change + brl_fined_change | 
                            year | 
                            garimpo_ha_change ~ bartik4,
                          data = rousseff)

stage4_temers <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change +
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | 
                         year | 
                         garimpo_ha_change ~ bartik4,
                       data = temer)

stage4_bolsonaros <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | 
                              year | 
                              garimpo_ha_change ~ bartik4,
                            data = bolsonaro)
                         
## create etable for stage 4

etable(
  list(
    "Lula"      = stage4_luls, 
    "Rousseff"  = stage4_rousseffs,
    "Temer"     = stage4_temers, 
    "Bolsonaro" = stage4_bolsonaros
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – By President (Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)
