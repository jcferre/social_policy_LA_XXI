## SCRIPT TO PULL DATA FROM CEPAL FOR EXPLANATORY ANALYSIS -- Dissertation
rm(list = ls())

# Libraries
library(dplyr)
library(xml2)
library(data.table)
library(readr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(haven)
detach("package:plm", unload = TRUE) # Just in case. This package has a "lag" function
  # that messes up my script below, where I use dplyr's "lag" function

# DATA
codebook_countries <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries.csv", drop = 1)
codebook_countries_short <- read_csv ("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries_short.csv")
codebook_years <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_years.csv", drop = 1)
codebook_quintiles <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_quintiles.csv")
codebook_pinktide_levitsky <- read_csv("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_pinktide_levitsky.csv")
cut_2002 <- read_csv("/Users/juan/Documents/Dissertation/write-ups/tables/Ch4/cut_2002_24.csv") %>%
  rename(coverage_score = Inclusion, generosity_score = Generosity, equity_score = Equity, decom_score = Decommodification)
master_data3 <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/master_data3_24.csv") %>%
  mutate(country = ifelse(str_detect(country, "Bolivia"), "Bolivia", 
                          ifelse(str_detect(country, "Venezuela"), "Venezuela", country))) 

# Constants 
LA17 <- c("216","221","222","224","225","226","229","230",
          "235","239","233","240","241","242","244","258","259")

# Functions

import_expl_files <- function(filename)
{
  read_excel(filename) %>%
    mutate(ind_short = str_remove(filename, ".xlsx$"))
}
line_plot <- function(data, x_var, y_var, group_var = NULL){
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}}, color = {{group_var}}, linetype = {{group_var}})) +
    geom_line()+
    theme_minimal()
  
}

setwd("/Users/juan/Documents/Dissertation/DATA/explanatory")
files <- dir()
files_list <- lapply(files, import_expl_files)
explanatory_raw <- rbindlist(files_list, fill = T) %>%
  rename(year = `Años__ESTANDAR`, 
         pais = `País__ESTANDAR`) %>%
  mutate(country = case_when(pais == "Bolivia (Estado Plurinacional de)" ~ "Bolivia", 
                             pais == "Brasil" ~ "Brazil", 
                             pais == "México" ~ "Mexico", 
                             pais == "Panamá" ~ "Panama", 
                             pais == "Perú" ~ "Peru", 
                             pais == "Venezuela (República Bolivariana de)" ~ "Venezuela", 
                             TRUE ~ pais)) %>%
  select(country, year, ind_short, value) %>%
  filter(year > 1994) %>%
  left_join(codebook_countries_short) %>%
  filter(cnum %in% LA17) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(codebook_pinktide_levitsky)

count(explanatory_raw, country, ind_short, year) %>%
  filter(n>1)

explanatory_data_cepal_wide <- explanatory_raw %>%
  pivot_wider(names_from = ind_short, values_from = value) %>%
  group_by(country) %>%
  arrange(year) %>%
  fill(pop_over65, .direction = "down") %>%
  ungroup()

# write_csv(explanatory_data_cepal_wide, "/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data_cepal_wide_24.csv")


# ILO data: 

collective_bargaining <- read_csv("/Users/juan/Documents/Dissertation/DATA/ILO/collective bargaining.csv") %>%
  mutate(ref_area.label = ifelse(str_detect(ref_area.label, "Bolivia"), "Bolivia", 
                                 ifelse(str_detect(ref_area.label, "Venezuela"), "Venezuela", ref_area.label))) %>%
  left_join(codebook_countries_short, by = c("ref_area.label" = "country")) %>%
  filter(!is.na(ccode))
data_points_coll_agre <- collective_bargaining %>%
  count(ccode, time)
# not enough data

union_density <- read_csv("/Users/juan/Documents/Dissertation/DATA/ILO/trade union density.csv")  %>%
  mutate(ref_area.label = ifelse(str_detect(ref_area.label, "Bolivia"), "Bolivia", 
                                 ifelse(str_detect(ref_area.label, "Venezuela"), "Venezuela", ref_area.label))) %>%
  left_join(codebook_countries_short, by = c("ref_area.label" = "country")) %>%
  filter(!is.na(ccode)) %>%
  select(country = ref_area.label, ccode, cnum, year = time, union_density = obs_value)

data_points_union <- union_density %>%
  count(ccode, year)
# it might be enough for one data point prior or at the beginning of the period

days_lost_to_strikes <- read_csv ("/Users/juan/Documents/Dissertation/DATA/ILO/Days not worked due to strikes and lockouts by economic activity.csv") %>%
  mutate(ref_area.label = ifelse(str_detect(ref_area.label, "Bolivia"), "Bolivia", 
                                 ifelse(str_detect(ref_area.label, "Venezuela"), "Venezuela", ref_area.label))) %>%
  left_join(codebook_countries_short, by = c("ref_area.label" = "country")) %>%
  filter(!is.na(ccode)) %>%
  arrange(desc(obs_value)) %>%
  distinct(ref_area.label, time, .keep_all = T) %>%
  select(country = ref_area.label, ccode, cnum, year = time, days_lost_to_strikes = obs_value)
data_points_days_lost <- days_lost_to_strikes %>%
  count(ccode, year)
# apparently very complete!

workers_involved_in_strikes <- read_csv("/Users/juan/Documents/Dissertation/DATA/ILO/Workers involved in strikes and lockouts by economic activity (thousands).csv") %>%
  mutate(ref_area.label = ifelse(str_detect(ref_area.label, "Bolivia"), "Bolivia", 
                                 ifelse(str_detect(ref_area.label, "Venezuela"), "Venezuela", ref_area.label))) %>%
  left_join(codebook_countries_short, by = c("ref_area.label" = "country")) %>%
  filter(!is.na(ccode)) %>%
  arrange(desc(obs_value)) %>%
  distinct(ref_area.label, time, .keep_all = T) %>%
  select(country = ref_area.label, ccode, cnum, year = time, workers_involved_in_strikes = obs_value)
data_points_workers_involved <- workers_involved_in_strikes %>%
  count(ccode, year)
# apparently very complete!


# exploratory analysis to see data completeness: 

line_plot(union_density, year, union_density, ccode)


# Joining all variables in one table: 
ILO_data <- union_density %>%
  full_join(days_lost_to_strikes, by = c("country", "ccode", "cnum", "year")) %>%
  full_join(workers_involved_in_strikes, by = c("country", "ccode", "cnum", "year"))

scores3 <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/scores3_24.csv") %>%
  mutate(country = ifelse(str_detect(country, "Bolivia"), "Bolivia", 
                          ifelse(str_detect(country, "Venezuela"), "Venezuela", country))) 

# Altman's electoral competition "C" index:
C_index <- read_dta("/Users/juan/Documents/Dissertation/DATA/Altman's C index/Competition C-index Altman-Perez-Linan.dta") %>%
  select(ccode, year, c_h) %>%
  rename(elect_comp = c_h)

# Protest data from MM (Harvard):
LA_protest <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/LA_protest.csv")

# Conflict data from cnts 
cnts_LA_17_conflict <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/cnts_LA_17_conflict.csv")

# just to check:
cor.test(ILO_data$workers_involved_in_strikes, ILO_data$days_lost_to_strikes)
# I have a problem!

explanatory_data <- ILO_data %>%
  full_join(explanatory_data_cepal_wide) %>%
  full_join(scores3) %>%
  left_join(cut_2002, by = "country", suffix = c( "","_base")) %>%
  mutate(imports = - imports, # for some reason, imports are expressed in negative values
         trade_openness = (exports + imports)/gdp) %>%
  filter(cnum %in% LA17, year > 1994) %>%
  mutate(delta_decom_score = ifelse(year == 2002, NA, decom_score - decom_score_base), 
         delta_generosity_score = ifelse(year == 2002, NA, generosity_score - generosity_score_base),
         delta_equity_score = ifelse(year == 2002, NA, equity_score - equity_score_base),
         delta_coverage_score = ifelse(year == 2002, NA, coverage_score - coverage_score_base)) %>%
  full_join(master_data3, by = c("ccode", "cnum", "country", "year")) %>%
  mutate(cct_incl = ifelse(cct_program == 0, 0, cct_cov_n_people / poverty_n),
         cct_incl = ifelse(cct_incl > 2, 2, cct_incl),
         ncp_social_deficit = (1 - ncp_vis_cont/100) * only_ncp) %>%
  group_by(country) %>%
  arrange(year) %>%
  fill(pop_over65, .direction = "down") %>%
  full_join(LA_protest) %>%
  #  Create a cummulative years of large protests
  mutate(lag_protest1 = lag(large_protest_n)
         , lag_protest2 = lag(large_protest_n, n = 2)
         , lag_protest3 = lag(large_protest_n, n = 3)
         , lag_protest4 = lag(large_protest_n, n = 4)
         , lag_protest5 = lag(large_protest_n, n = 5)
         , lag_protest6 = lag(large_protest_n, n = 6)
         , lag_protest7 = lag(large_protest_n, n = 7)
         , lag_protest8 = lag(large_protest_n, n = 8)
         , lag_protest9 = lag(large_protest_n, n = 9)
         , lag_protest10 = lag(large_protest_n, n = 10), 
         large_protest_cum_10yrs = lag_protest1 + lag_protest2 + lag_protest3 + lag_protest4 + lag_protest5 
         + lag_protest6 + lag_protest7 + lag_protest8 + lag_protest9 + lag_protest10, 
         large_protest_cum_5yrs = lag_protest1 + lag_protest2 + lag_protest3 + lag_protest4 + lag_protest5, 
         ) %>%
  left_join(cnts_LA_17_conflict) %>%
  mutate(lag_general_strikes1 = lag(general_strikes)
         , lag_general_strikes2 = lag(general_strikes, n = 2)
         , lag_general_strikes3 = lag(general_strikes, n = 3)
         , lag_general_strikes4 = lag(general_strikes, n = 4)
         , lag_general_strikes5 = lag(general_strikes, n = 5), 
         cum_general_strikes_5yrs = lag_general_strikes1 + lag_general_strikes2 + lag_general_strikes3 + lag_general_strikes4 + lag_general_strikes5) %>%
  fill(cum_general_strikes_5yrs, .direction = "down") %>%
  left_join(C_index) %>%
  mutate(lag_elect_comp1 = lag(elect_comp)
         , lag_elect_comp2 = lag(elect_comp, n = 2)
         , lag_elect_comp3 = lag(elect_comp, n = 3)
         , lag_elect_comp4 = lag(elect_comp, n = 4)
         , lag_elect_comp5 = lag(elect_comp, n = 5), 
         cum_elect_comp_5yrs = lag_elect_comp1 + lag_elect_comp2 + lag_elect_comp3 + lag_elect_comp4 + lag_elect_comp5) %>%
  fill(cum_elect_comp_5yrs, .direction = "down") %>%
  mutate(lag_tax_rev_pct_gdp1 = lag(tax_rev_pct_gdp)
         , lag_tax_rev_pct_gdp2 = lag(tax_rev_pct_gdp, n = 2)
         , lag_tax_rev_pct_gdp3 = lag(tax_rev_pct_gdp, n = 3)
         , lag_tax_rev_pct_gdp4 = lag(tax_rev_pct_gdp, n = 4)
         , lag_tax_rev_pct_gdp5 = lag(tax_rev_pct_gdp, n = 5), 
         cum_tax_rev_pct_gdp_5yrs = lag_tax_rev_pct_gdp1 + lag_tax_rev_pct_gdp2 + lag_tax_rev_pct_gdp3 + lag_tax_rev_pct_gdp4 + lag_tax_rev_pct_gdp5) %>%
  fill(cum_tax_rev_pct_gdp_5yrs, .direction = "down") %>%
filter(year > 1998, year < 2021) %>%
  mutate(lag_pcgdp_growth = lag(pcgdp_growth)
         , lag2_pcgdp_growth = lag(pcgdp_growth, n = 2)
         , lag3_pcgdp_growth = lag(pcgdp_growth, n = 3)
         , lag_curr_acct_blz = lag(curr_acct_blnz_pct_gdp)
         , lag2_curr_acct_blz = lag(curr_acct_blnz_pct_gdp, n = 2)
         , lag3_curr_acct_blz = lag(curr_acct_blnz_pct_gdp, n = 3), 
         last3yrs_pcgdp_growth_avg = (lag_pcgdp_growth + lag2_pcgdp_growth + lag3_pcgdp_growth)/3,
         last3yrs_curr_acct_blz = (lag_curr_acct_blz + lag2_curr_acct_blz + lag3_curr_acct_blz)/3) %>%
  mutate(lag_total_social_xpnd_pctGDP = lag(total_social_xpnd_pctGDP)) %>%
  mutate(delta_total_social_exp_pctGDP = total_social_xpnd_pctGDP - lag_total_social_xpnd_pctGDP) %>%
  mutate(lag_educ_xpnd_pctGDP = lag(educ_xpnd_pctGDP)) %>%
  mutate(delta_educ_exp_pctGDP = educ_xpnd_pctGDP - lag_educ_xpnd_pctGDP)%>%
  mutate(lag_govt_hexp_pctGDP_WB = lag(govt_hexp_pctGDP_WB)) %>%
  mutate(delta_govt_hexp_pctGDP_WB = govt_hexp_pctGDP_WB - lag_govt_hexp_pctGDP_WB) %>%
  mutate(lag_priv_health_exp_pct = lag(priv_health_exp_pct)) %>%
  mutate(delta_priv_health_exp_pct = priv_health_exp_pct - lag_priv_health_exp_pct) %>%
  mutate(lag_elderly_covered_any_pension = lag(elderly_covered_any_pension)) %>%
  mutate(delta_elderly_covered_any_pension = elderly_covered_any_pension - lag_elderly_covered_any_pension) %>%
  ungroup() %>%
  #left_join(codebook_pinktide_2020, by = c("ccode", "year")) %>%
  mutate(pre_2013 = ifelse(year < 2013, 1, 0), 
         pcgdp_in_k = pcgdp/1000) %>%
  # Create a cummulative years of pink tide gov variable: 
  group_by(ccode) %>%
  arrange(year) %>%
  mutate(pinktide_yrs = cumsum(pinktide)) %>%
  ungroup() %>%
  mutate(had_coup = case_when(ccode == "HND" & year > 2009 ~ 1, 
                              ccode == "PRY" & year > 2012 ~ 1, 
                              ccode == "BRA" & year > 2016 ~ 1,
                              ccode == "BOL" & year > 2019 ~ 1,
                              TRUE ~ 0), 
         legit_crisis_dummy = case_when(ccode == "VEN" ~ 1, 
                                        ccode == "ARG" & year > 2001 ~ 1, 
                                        ccode == "URY" & year > 2002 ~ 1,
                                        ccode == "ECU" & year > 2000 ~ 1,
                                        ccode == "BOL" & year > 2002 ~ 1,
                                        ccode == "GTM" & year > 2015 ~ 1,
                                        TRUE ~ 0), 
         legit_crisis_dummy_2 = case_when(ccode == "VEN" ~ 1, 
                                        ccode == "ARG" & year > 2001 ~ 1, 
                                        ccode == "URY" & year > 2002 ~ 1,
                                        ccode == "ECU" & year > 2000 ~ 1,
                                        ccode == "BOL" & year > 2002 ~ 1,
                                        ccode == "GTM" & year > 2015 ~ 1,
                                        ccode == "PRY" & year > 2002 ~ 1, 
                                        ccode == "PER" & year > 2004 ~ 1, 
                                        TRUE ~ 0), 
         legit_crisis_dummy_3 = case_when(ccode == "VEN" ~ 1, # This one is without Uruguay
                                          ccode == "ARG" & year > 2001 ~ 1, 
                                          ccode == "ECU" & year > 2000 ~ 1,
                                          ccode == "BOL" & year > 2002 ~ 1,
                                          ccode == "GTM" & year > 2015 ~ 1,
                                          ccode == "PRY" & year > 2002 ~ 1, 
                                          ccode == "PER" & year > 2004 ~ 1, 
                                          TRUE ~ 0), 
         legit_crisis_aftermath = case_when(ccode == "VEN" & year <= 1999 ~ 2, 
                                            ccode == "VEN" & year <= 2004 ~ 1, 
                                            ccode == "ARG" & year > 2001 & year <= 2006 ~ 2, 
                                            ccode == "ARG" & year > 2006 & year <= 2011 ~ 1, 
                                            ccode == "ARG" & year > 2011 ~ 0, 
                                            ccode == "URY" & year > 2002 & year <= 2007 ~ 2,
                                            ccode == "URY" & year > 2007 & year <= 2012 ~ 1,
                                            ccode == "URY" & year > 2012 ~ 0,
                                            ccode == "ECU" & year > 2000 & year <= 2005  ~ 2,
                                            ccode == "ECU" & year > 2005 & year <= 2010  ~ 1,
                                            ccode == "ECU" & year > 2010 ~ 0,
                                            ccode == "BOL" & year > 2002 & year <= 2007  ~ 2, 
                                            ccode == "BOL" & year > 2007 & year <= 2012  ~ 1, 
                                            ccode == "BOL" & year > 2012 ~ 0,
                                            ccode == "GTM" & year > 2015 ~ 2,
                                            TRUE ~ 0)) 
  
# write_csv(explanatory_data, "/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data_2025.csv")
# explanatory_data <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data_2025.csv")

## Pink tide table for chapter 5: 

table7 <- codebook_pinktide_levitsky %>%
  left_join(codebook_countries_short) %>%
  filter(cnum %in% LA17) %>%
  select(country, year, pinktide) %>%
  mutate(pinktide = factor(pinktide, levels = c(0, 1), labels = c("No", "Yes"))) %>%
  filter(year> 1999, year < 2021) %>%
  pivot_wider(names_from = year, values_from = pinktide, names_sort = T) 

write_clip(table7)
