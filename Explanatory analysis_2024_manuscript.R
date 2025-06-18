rm(list = ls())

# Explanatory analysis: 
# Determinants of social policy change in 21st Century Latin America

library(dplyr)
library(data.table)
library(readr)
library(readxl)
library(broom)
library(tidyr)
library(prais)
library(AER)
# library(plm)
library(orcutt)
library(stringr)
library(clipr)
library(foreign)

# Constants 
LA17 <- c("216","221","222","224","225","226","229","230",
          "235","239","233","240","241","242","244","258","259")
# Data: 
explanatory_data <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data_2025.csv") 
codebook_countries_short <- read_csv ("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries_short.csv")

# Functions: 
prep_model<- function(model){
  
    tidy_model <- tidy(model) %>%
    transmute(term, m = estimate, 
              p = case_when(p.value > 0.05 ~ " ",
                            p.value < 0.05 & p.value > 0.01 ~ "*",
                            p.value < 0.01 & p.value > 0.001 ~ "**", 
                            p.value < 0.001 ~ "***"))
  r2 <- data.frame(term = "Adj r-squared", m = summary(model)$adj.r.squared, p = NA)
  n <- data.frame(term = "Regression N", m = length(model$residuals), p = NA)
  tidy_model %>%
    rbind(r2) %>%
    rbind(n)
}

## Comparison pink tide vs non-pink tide in progress in decomm

comparison_pink_tide <- explanatory_data %>%
  group_by(pinktide) %>%
  filter(!is.na(delta_decom_score)) %>%
  summarize(avg_decom = mean(delta_decom_score), 
            avg_incl = mean(delta_coverage_score),
            avg_gen = mean(delta_generosity_score), 
            avg_equity = mean(delta_equity_score))


 ##########################################################
 ## Regression ##### key variables as regressants #########
 ## Analysis   ############################################
 ################

#creating base values:
base_values <- explanatory_data %>%
  filter(year > 1999, year < 2006) %>%
  group_by(country) %>%
  arrange(year) %>%
  fill(c(1:ncol(explanatory_data)), .direction = "up") %>%
  filter(year == 2000) %>%
  select(-year)

vis_miss(base_values)
 
## filling some data on unemployment: 

explanatory_data2 <- explanatory_data %>%
   group_by(ccode) %>%
   fill(industrial_employment, .direction = "down") %>%
   ungroup() %>%
  # here I'm adding missing data on unemployment: 
  mutate(unemployment = case_when(ccode == "ECU" & year == 2002 ~ 8.47, # this is Ecuador's value for 2001
                                  ccode == "GTM" & year == 2002 ~ 2.2, # from datosmacro https://datosmacro.expansion.com/paro-epa/guatemala?sc=UEPAR-&anio=2002
                                  ccode == "GTM" & year == 2007 ~ 2.3, # from datosmacro
                                  ccode == "PER" & year == 2002 ~ 7.9, # from datosmacro.com https://datosmacro.expansion.com/paro-epa/peru?sc=UEPAR-&anio=2002
                                  ccode == "URY" & year == 2002 ~ 18.6, 
                                  TRUE ~ unemployment)) %>%
   left_join(base_values, by = "country", suffix = c("", "_base")) %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(cum_pcgdp_growth = cumsum(pcgdp_growth), 
         cum_curr_acct_blz = cumsum(curr_acct_blnz_pct_gdp), 
         cum_trade_openness = cumsum(trade_openness))

explanatory_data_no_2002 <- explanatory_data2 %>%
  filter(year > 2002) # Note that these eliminates 2000, 2001 and 2002 (bc  "base" values are filled with these years)

# write.dta(explanatory_data_no_2002, "/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data_no_2002.dta")

explanatory_with_base <- explanatory_data2 %>%
  left_join(base_values, by = "country", suffix = c("", "_base")) 


# check availability of strike activity indicators:
data_avail_strikes <- explanatory_data2 %>%
  group_by(year) %>%
  summarize(across(c(union_density, days_lost_to_strikes, workers_involved_in_strikes), 
                   ~ sum(!is.na(.))))
 
### MODELS ####


# Playing


### Progress in decommodification as dependent variable (including a base "decom_score_base" on the right side)
decom_model1 <- lm(decom_score ~ cum_pcgdp_growth + pinktide_yrs
                   + legit_crisis_dummy_2 + decom_score_base, 
                   explanatory_data_no_2002)
summary(decom_model1)

decom_model2 <- lm(decom_score ~ pinktide_yrs + cum_pcgdp_growth
                   + legit_crisis_dummy_2 +  cum_elect_comp_5yrs + decom_score_base, 
                   explanatory_data_no_2002)

summary(decom_model2)

decom_model3 <- lm(decom_score ~ pinktide_yrs + cum_pcgdp_growth
                   + legit_crisis_dummy_2 +  cum_elect_comp_5yrs 
                   + trade_openness + decom_score_base, 
                   explanatory_data_no_2002)
summary(decom_model3)

decom_model4 <- lm(decom_score ~ pinktide_yrs + cum_pcgdp_growth
                   +  cum_elect_comp_5yrs  + legit_crisis_dummy_2
                   + had_coup + decom_score_base, 
                   explanatory_data_no_2002)
summary(decom_model4)


models_decom  <- prep_model(decom_model1) %>%
  rename(m_1 = m, p_1 = p) %>%
  full_join(prep_model(decom_model2), by = "term") %>%
  rename(m_2 = m, p_2 = p) %>%
  full_join(prep_model(decom_model3), by = "term") %>%
  rename(m_3 = m, p_3 = p) %>%
  # full_join(prep_model(decom_model4), by = "term") %>%
  # rename(m_4 = m, p_4 = p) %>%  
  #  full_join(prep_model(decom_model5), by = "term") %>%
  #  rename(m_5 = m, p_5 = p) %>%  
  # full_join(prep_model(decom_model6), by = "term") %>%
  #  rename(m_6 = m, p_6 = p) %>% 
  mutate_if(is.numeric, round, 2) %>%
  mutate_all(~replace(., is.na(.), " ")) %>%
  arrange(str_detect(term, "Adj"))

write_clip(models_decom)

dwtest(decom_model1)
dwtest(decom_model2)
dwtest(decom_model3)
dwtest(decom_model4)

coeftest(decom_model1, vcov = NeweyWest(decom_model1, lag = 1))
coeftest(decom_model2, vcov = NeweyWest(decom_model2, lag = 1))
coeftest(decom_model3, vcov = NeweyWest(decom_model3, lag = 1))
coeftest(decom_model4, vcov = NeweyWest(decom_model4, lag = 1))

# Now trying Generalized Linear Model (FGLM) using Cochrane-Orcutt estimators 
cochrane.orcutt(decom_model1, convergence = 8, max.iter=100) %>% summary.orcutt()
cochrane.orcutt(decom_model2, convergence = 8, max.iter=100) %>% summary.orcutt()
cochrane.orcutt(decom_model3, convergence = 8, max.iter=100) %>% summary.orcutt()



# Now trying models with lower level variables to increase the N
# PCA detected the following vars as main ones: 

# For the whole data set: 
# cct_incl
# elderly_covered_any_pension
# pen_rate_occup_all
# UHC_index

# For equity 
# ncp_social_deficit
# pen_gap

# For generosity
# cct_vis_med_income_wageearner
# health_spending


# cct_incl

cct_incl_model6 <- lm(cct_incl  ~ cum_pcgdp_growth + pinktide_yrs  
                      +  legit_crisis_dummy_2 + cum_elect_comp_5yrs   
                      + trade_openness + 
                        + cct_incl_base, 
                      explanatory_data2)
summary(cct_incl_model6)

# elderly_covered_any_pension

elderly_covered_model6 <- lm(elderly_covered_any_pension  ~ cum_pcgdp_growth + pinktide_yrs  
                             + cum_elect_comp_5yrs   
                             + trade_openness + 
                               +  legit_crisis_dummy_2
                             + elderly_covered_any_pension_base, 
                             explanatory_data2)
summary(elderly_covered_model6)


# pen_rate_occup_all

pen_rate_model6 <- lm(pen_rate_occup_all  ~ cum_pcgdp_growth + pinktide_yrs  
                      + cum_elect_comp_5yrs   
                      + trade_openness + 
                        +  legit_crisis_dummy_2
                      + pen_rate_occup_all_base, 
                      explanatory_data2)
summary(pen_rate_model6)


# For generosity

# cct_vis_med_income_wageearner

cct_gen_model6 <- lm(cct_vis_med_income_wageearner  ~ cum_pcgdp_growth + pinktide_yrs  
                     + cum_elect_comp_5yrs   
                     + trade_openness + 
                       +  legit_crisis_dummy_2
                     + cct_vis_med_income_wageearner_base, 
                     explanatory_data2)
summary(cct_gen_model6)

# health_spending

hexp_ppp_model6 <- lm(gov_hexp_ppp_WB_cnstnt  ~ cum_pcgdp_growth + pinktide_yrs  
                      
                      + cum_elect_comp_5yrs   
                      + trade_openness + 
                        +  legit_crisis_dummy_2
                      + gov_hexp_ppp_WB_cnstnt_base, 
                      explanatory_data2)
summary(hexp_ppp_model6)

govt_hexp_pctGDP_WB_model6 <- lm(govt_hexp_pctGDP_WB  ~ cum_pcgdp_growth + pinktide_yrs  
                                 
                                 + cum_elect_comp_5yrs   
                                 + trade_openness + 
                                   +  legit_crisis_dummy_2
                                 + govt_hexp_pctGDP_WB_base, 
                                 explanatory_data2)
summary(govt_hexp_pctGDP_WB_model6)

priv_health_exp_pct_model <- lm(priv_health_exp_pct  ~ cum_pcgdp_growth + pinktide_yrs  
                                
                                + cum_elect_comp_5yrs   
                                + trade_openness + 
                                  +  legit_crisis_dummy_2
                                + priv_health_exp_pct_base, 
                                explanatory_data2)
summary(priv_health_exp_pct_model)

pct_priv_primschool_model <- lm(pct_priv_primschool  ~ cum_pcgdp_growth + pinktide_yrs  
                                
                                + cum_elect_comp_5yrs   
                                + trade_openness + 
                                  +  legit_crisis_dummy_2
                                + pct_priv_primschool_base, 
                                explanatory_data2)
summary(pct_priv_primschool_model)


various_dp_models6 <- prep_model(cct_incl_model6) %>%
  rename(cct_incl = m, p_1 = p) %>%
  full_join(prep_model(cct_gen_model6), by = "term") %>%
  rename(cct_gen_model6 = m, p_2 = p) %>% 
  full_join(prep_model(elderly_covered_model6), by = "term") %>%
  rename(elderly_cov = m, p_3 = p) %>%
  full_join(prep_model(pen_rate_model6), by = "term") %>%
  rename(pen_rate = m, p_4 = p) %>%
  full_join(prep_model(govt_hexp_pctGDP_WB_model6), by = "term") %>%
  rename(hexp_pctGDP = m, p_5 = p) %>%  
  full_join(prep_model(priv_health_exp_pct_model), by = "term") %>%
  rename(priv_health_exp = m, p_6 = p) %>% 
  full_join(prep_model(pct_priv_primschool_model), by = "term") %>%
  rename(priv_prim_school = m, p_7 = p) %>% 
  mutate_if(is.numeric, round, 2) %>%
  mutate_all(~replace(., is.na(.), " ")) %>%
  arrange(str_detect(term, "Adj"))

write_clip(various_dp_models6)

dwtest(cct_incl_model6)
dwtest(elderly_covered_model6)
dwtest(govt_hexp_pctGDP_WB_model6)
dwtest(pen_rate_model6)
dwtest(cct_gen_model6)


