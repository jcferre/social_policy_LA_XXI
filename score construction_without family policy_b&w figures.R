# SCORE CONSTRUCTION

rm(list = ls())

# Packages
library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(visdat)
library(lubridate)

# codebooks:
codebook_countries <- fread("/Users/juan/Dropbox (Princeton)/Dissertation/DATA/codebooks/codebook_countries.csv", drop = 1)
codebook_countries_short <- read_csv ("/Users/juan/Dropbox (Princeton)/Dissertation/DATA/codebooks/codebook_countries_short.csv")
codebook_quintiles <- fread("/Users/juan/Dropbox (Princeton)/Dissertation/DATA/codebooks/codebook_quintiles.csv")

# Constants: 
LA17 <- c("216","221","222","224","225","226","229","230",
          "235","239","233","240","241","242","244","258","259")
# Input data:

master_data3 <- read_csv("/Users/juan/Dropbox (Princeton)/Dissertation/DATA/my data sets/master_data3_24.csv")

# Functions: 

consolidate_5_years <- function (data, selected_year){
  
  year_data <- data %>%
    group_by(ccode) %>%
    filter(year %in% c(selected_year-1, selected_year, selected_year+1)) %>%
    fill(c(1:ncol(data)), .direction = "downup") %>%
    filter(year == selected_year)
  
  year_plus_2_data <- data %>%
    group_by(ccode) %>%
    filter(year == selected_year+2) 
  
  year_minus_2_data <- data %>%
    group_by(ccode) %>%
    filter(year == selected_year-2)
  
  year_minus_2_data %>% 
    rbind(year_data, year_plus_2_data) %>%
    fill(c(6:ncol(data)), .direction = "downup") %>%
    filter(year == selected_year)
}

fill_with_closest_value_within_8_yrs <- function (consolidated_data, source_data, selected_year){
  data <- source_data %>%
    filter(year != selected_year) %>%
    rbind(consolidated_data)
  
  year_data <- data %>%
    group_by(ccode) %>%
    filter(year %in% c(selected_year-3, selected_year, selected_year+3)) %>%
    fill(c(1:ncol(data)), .direction = "downup") %>%
    filter(year == selected_year)
  
  year_plus_4_data <- data %>%
    group_by(ccode) %>%
    filter(year == selected_year+4) 
  
  year_minus_4_data <- data %>%
    group_by(ccode) %>%
    filter(year == selected_year-4)
  
  year_minus_4_data %>% 
    rbind(year_data, year_plus_4_data) %>%
    fill(c(1:ncol(data)), .direction = "downup") %>%
    filter(year == selected_year)
}

normalize <- function(consolidated_data){
  consolidated_data %>%
    ungroup() %>%
    mutate(cnum = as.character(cnum),
           year = as.Date(as.character(year), format = "%Y")) %>%
    # so that I can standardize all numeric vars
    mutate_if(is.numeric, ~(scale(.) %>% as.vector)) %>%
    mutate(leave_gender_bias = recode(leave_gender_bias, "equity" = 2, "supportive paternal" = 1, 
                                      "minimal paternal" = 0.5, "no paternal" = 0)) %>%
    mutate(year = year(year))
}
  
cap_score <- function(standardized_data){
  standardized_data %>%
    mutate(cct_incl = ifelse(cct_incl>2.5, 2.5, cct_incl), 
           cct_xpnd_pctGDP = ifelse(cct_xpnd_pctGDP>2.5, 2.5, cct_xpnd_pctGDP), 
           pen_rate_occup_all = ifelse(pen_rate_occup_all>2.5, 2.5, pen_rate_occup_all), 
           pen_repl_rate_median_income = ifelse(pen_repl_rate_median_income>2.5, 2.5, pen_repl_rate_median_income), 
           elderly_covered_any_pension = ifelse(elderly_covered_any_pension>2.5, 2.5, elderly_covered_any_pension), 
           pen_gap = ifelse(pen_gap>2.5, 2.5, pen_gap), 
           ncp_vis_cont = ifelse(ncp_vis_cont>2.5, 2.5, ncp_vis_cont), 
           priv_pens_pct_gdp = ifelse(priv_pens_pct_gdp>2.5, 2.5, priv_pens_pct_gdp), 
           govt_hexp_pctGDP_WB = ifelse(govt_hexp_pctGDP_WB>2.5, 2.5, govt_hexp_pctGDP_WB), 
           gov_hexp_ppp_WB_cnstnt = ifelse(gov_hexp_ppp_WB_cnstnt>2.5, 2.5, gov_hexp_ppp_WB_cnstnt), 
           UHC_index = ifelse(UHC_index>2.5, 2.5, UHC_index),
           cat_hexp_sec = ifelse(cat_hexp_sec >2.5, 2.5, cat_hexp_sec), 
           priv_health_exp_pct = ifelse(priv_health_exp_pct>2.5, 2.5, priv_health_exp_pct), 
           oop_health_exp_pc_ppp = ifelse(oop_health_exp_pc_ppp>2.5, 2.5, oop_health_exp_pc_ppp), 
           primschoolenrol = ifelse(primschoolenrol>2.5, 2.5, primschoolenrol), 
           preprim_enr_rate = ifelse(preprim_enr_rate>2.5, 2.5, preprim_enr_rate), 
           educ_xpnd_pctGDP = ifelse(educ_xpnd_pctGDP>2.5, 2.5, educ_xpnd_pctGDP), 
           pct_priv_primschool = ifelse(pct_priv_primschool>2.5, 2.5, pct_priv_primschool), 
           pct_priv_preprim = ifelse(pct_priv_preprim>2.5, 2.5, pct_priv_preprim), 
           leave_generosity = ifelse(leave_generosity>2.5, 2.5, leave_generosity))
}

# This are the function "make_score10" and "plot_scores6" in the score construction3 script. 
make_score <- function(standardized_data){
  standardized_data %>%
    mutate(coverage_score = cct_incl + elderly_covered_any_pension +
             pen_rate_occup_all + UHC_index + school_enrol + preprim_enr_rate, 
           
           generosity_score = cct_vis_med_income_wageearner + pen_repl_rate_median_income
           + health_spending + educ_xpnd_pctGDP + mat_leave_generosity, 
           
           equity_score = - ncp_social_deficit - pen_gap - priv_pens_pct_gdp -
             priv_health - pct_priv_school,
           
           decom_score = coverage_score + generosity_score + equity_score, 
           
           transfers_nc_score = cct_incl + cct_vis_med_income_wageearner 
           + elderly_covered_any_pension - ncp_social_deficit, 
           
           transfers_cont_score = pen_rate_occup_all + pen_repl_rate_median_income 
           + mat_leave_generosity - pen_gap - priv_pens_pct_gdp, 
           
           health_score = health_spending + UHC_index - priv_health, 
           
           educ_score = school_enrol + educ_xpnd_pctGDP + preprim_enr_rate - pct_priv_school) %>%
    select(country, ccode, year, ends_with("score")) %>%
    arrange(ccode)
}

plot_scores <- function (score_data, caption_label = NULL) {
  
  coverage_plot <-  ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = coverage_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(coverage_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Inclusion Score", caption = caption_label, y = "Country",
         fill = "Inclusion") + 
    theme(legend.position = "none")
  
  
  generosity_plot <- ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = generosity_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(generosity_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Generosity Score", caption = caption_label, y = "Country",
         fill = "Generosity") + 
    theme(legend.position = "none")
  
  equity_plot <-  ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = equity_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(equity_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Equity Score", caption = caption_label, y = "Country", fill = "Equity") + 
    theme(legend.position = "none")
  
  decom_plot <-  ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = decom_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(decom_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Decommodification Score", caption = caption_label, y = "Country", 
         fill = "Decommodification") + 
    theme(legend.position = "none")
  
  list(coverage_plot, generosity_plot, equity_plot, decom_plot)
  
}

plot_area_scores10 <- function (score_data, caption_label = NULL) {
  
  transfers_nc_plot <-  ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = transfers_nc_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(transfers_nc_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Non-contributory Transfers", caption = caption_label, y = "Country") + 
    theme(legend.position = "none")
  
  transfers_cont_plot <-  ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = transfers_cont_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(transfers_cont_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Contributory Transfers", caption = caption_label, y = "Country") + 
    theme(legend.position = "none")
  
  health_plot <- ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = health_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(health_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Health", caption = caption_label, y = "Country") + 
    theme(legend.position = "none")
  
  educ_plot <-  ggplot(score_data, aes(year, reorder(ccode, desc(ccode)), fill = educ_score)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(educ_score, digits = 1))) +
    scale_fill_gradient(low = "grey20", high = "white") +
    scale_x_continuous( breaks = c(2002, 2007, 2012, 2017)) +
    labs(title = "Education", caption = caption_label, y = "Country") + 
    theme(legend.position = "none")
  
  
  list(transfers_nc_plot, transfers_cont_plot, health_plot, educ_plot)
  
}

plot_variables_progress_simple_score <- function(standardized_data, title_label = NULL){
  incl_table_tidy <- standardized_data %>%
    select(ccode, year, cct_incl,  pen_rate_occup_all, elderly_covered_any_pension,    
           UHC_index , school_enrol , preprim_enr_rate) %>%
    pivot_longer(cols = c(3:8), names_to = "variable", values_to = "value")
  incl_vars_plot <- ggplot(incl_table_tidy, aes(year, reorder(ccode, desc(ccode)), fill = value)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(value, digits = 1)))+
    facet_wrap(vars(variable))+
    scale_fill_gradient(low = "blue", high = "lightblue") +
    labs(y = "Country", fill = "Score")
  
  gen_table_tidy <- standardized_data %>%
    select(ccode, year, cct_vis_med_income_wageearner, pen_repl_rate_median_income,
           health_spending, educ_xpnd_pctGDP, mat_leave_generosity) %>%
    pivot_longer(cols = c(3:7), names_to = "variable", values_to = "value")
  gen_vars_plot <- ggplot(gen_table_tidy, aes(year, reorder(ccode, desc(ccode)), fill = value)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(value, digits = 1)))+
    facet_wrap(vars(variable))+
    scale_fill_gradient(low = "blue", high = "lightblue") +
    labs(y = "Country", fill = "Score")
  
  segm_table_tidy <- standardized_data %>%
    select(ccode, year, ncp_social_deficit, pen_gap, priv_pens_pct_gdp,
             priv_health, pct_priv_school) %>%
    pivot_longer(cols = c(3:7), names_to = "variable", values_to = "value")
  segm_vars_plot <- ggplot(segm_table_tidy, aes(year, reorder(ccode, desc(ccode)), fill = value)) +
    geom_tile() +
    theme_minimal() +
    geom_text(aes(label = round(value, digits = 1)))+
    facet_wrap(vars(variable))+
    scale_fill_gradient(low = "orange", high = "yellow")+
    labs(y = "Country", fill = "Score")
  
  list(incl_vars_plot, gen_vars_plot, segm_vars_plot)
}

plot_progress_dimensions_by_area <- function(tidy_data, country_code) {
  country_data <-tidy_data %>%
    filter(ccode == country_code, year %in% c(2002, 2017), !is.na(dimension), !is.na(area)) %>%
    group_by(country, year, area, dimension) %>%
    summarize(value = mean(value))
  ggplot(country_data, aes(value, dimension, color = as.factor(year))) +
    geom_point()+
    geom_line(color = "grey20")+
    geom_vline(xintercept = 0, linetype = "dotted")+
    facet_wrap(~ area)+
    labs(title = country_data$country, color = "Year", x = "Score in SDs")+
    theme_classic()
}

plot_progress_dimensions_by_area_country <- function(tidy_data, country_code) {
  country_data <-tidy_data %>%
    filter(ccode %in% country_code, year %in% c(2002, 2017), !is.na(dimension), !is.na(area)) %>%
    group_by(country, year, area, dimension) %>%
    summarize(value = mean(value))
  ggplot(country_data, aes(value, dimension, color = as.factor(year))) +
    geom_point()+
    geom_line(color = "grey20")+
    geom_vline(xintercept = 0, linetype = "dotted")+
    facet_grid(vars(area), vars(country))
    labs(title = country_data$country, color = "Year", x = "Score in SDs")+
    theme_classic()
}

get_progress <- function(score_data){
  progress_cov <- score_data %>%
    select(ccode, year, coverage_score) %>%
    filter(year %in% c(2002, 2017)) %>%
    pivot_wider(names_from = year, names_prefix = "year_", values_from = coverage_score) %>%
    mutate(progress = year_2017 - year_2002)
  
  progress_gen <- score_data %>%
    select(ccode, year, generosity_score) %>%
    filter(year %in% c(2002, 2017)) %>%
    pivot_wider(names_from = year, names_prefix = "year_", values_from = generosity_score) %>%
    mutate(progress = year_2017 - year_2002)
  
  progress_equity <- score_data %>%
    select(ccode, year, equity_score) %>%
    filter(year %in% c(2002, 2017)) %>%
    pivot_wider(names_from = year, names_prefix = "year_", values_from = equity_score) %>%
    mutate(progress = year_2017 - year_2002)
  
  progress_decom <- score_data %>%
    select(ccode, year, decom_score) %>%
    filter(year %in% c(2002, 2017)) %>%
    pivot_wider(names_from = year, names_prefix = "year_", values_from = decom_score) %>%
    mutate(progress = year_2017 - year_2002)
  
  progress_all <- progress_cov %>%
    rename(coverage = progress) %>%
    select(ccode, coverage) %>%
    left_join(progress_gen, by = "ccode") %>%
    select(-year_2002, -year_2017) %>%
    rename(generosity = progress) %>%
    left_join(progress_equity, by = "ccode") %>%
    select(-year_2002, -year_2017) %>%
    rename(equity = progress) %>%
        left_join(progress_decom, by = "ccode") %>%
    select(-year_2002, -year_2017) %>%
    rename(decommodification = progress)
}

# First: narrow it down to the variables I need to calculate the score
# (although I'm still not calculating the score)
regime_input <- master_data3 %>%
  transmute(country, ccode, cnum, year, cct_program,
            cct_incl = ifelse(cct_program == 0, 0, cct_cov_n_people / poverty_n),
            cct_incl = ifelse(cct_incl > 2, 2, cct_incl), # capping cct inclusion at a max of 2 times the pop living in poverty
            cct_xpnd_pctGDP, 
            #cct = cct_incl * cct_vis_med_income_wageearner,
            cct_vis_med_income_wageearner,
            pen_rate_occup_all, pen_repl_rate_median_income, 
            elderly_covered_any_pension, 
            pen_gap = (pen_gap_waged_non_waged_all + pen_diff_q1q5_occup) / 2, # This is an avg of 2 vars: pension coverage differential between 
                                                                               # waged and non_waged and differential between q1 and q5
            ncp_social_deficit = (1 - ncp_vis_cont/100) * only_ncp, ncp_vis_cont, priv_pens_pct_gdp, 
            health_spending = govt_hexp_pctGDP_WB * gov_hexp_ppp_WB_cnstnt, 
            govt_hexp_pctGDP_WB, gov_hexp_ppp_WB_cnstnt, UHC_index, cat_hexp_sec,
            priv_health = priv_health_exp_pct * oop_health_exp_pc_ppp, priv_health_exp_pct, 
            oop_health_exp_pc_ppp,
            school_enrol = (primschoolenrol + upsecschoolenrol) / 2, 
            primschoolenrol, upsecschoolenrol, preprim_enr_rate, educ_xpnd_pctGDP, 
            pct_priv_school = (pct_priv_primschool + pct_priv_secschool) / 2, pct_priv_preprim, 
            mat_leave_generosity = mat_leave_weeks *  mat_rep_rate, mat_leave_weeks, 
            mat_rep_rate, leave_gender_bias) %>%
  arrange(ccode, year)

vis_miss(regime_input)
count(regime_input, ccode, year) %>%
  filter(n>1)

missing_by_country_year <- regime_input %>%
  mutate(na_count = rowSums(is.na(.))) %>%
  select(ccode, year, na_count) %>%
  filter(year<2020)

missing_by_country <- missing_by_country_year %>%
  group_by(ccode) %>%
  summarize(missing = sum(na_count))

## Consolidate data from several years to reduce missingness:

# Consolidating every 5 years yields better results:

consolidated_year_2002 <- consolidate_5_years(regime_input, 2002)
consolidated_year_2007 <- consolidate_5_years(regime_input, 2007)
consolidated_year_2012 <- consolidate_5_years(regime_input, 2012)
consolidated_year_2017 <- consolidate_5_years(regime_input, 2017)

consolidated_score_data <- consolidated_year_2002 %>%
  rbind(consolidated_year_2007, consolidated_year_2012, 
        consolidated_year_2017) %>%
  ungroup()

consolidated_score_data2 <- consolidated_score_data

vis_miss(consolidated_score_data2)

# I will later use this "consolidated_score_data2" as the base (non-imputed) data

na_count_by_country_year <- consolidated_score_data2 %>%
  mutate(na_count = rowSums(is.na(.))) %>%
  select(ccode, year, na_count) 

na_count_by_country <- na_count_by_country_year %>%
  group_by(ccode) %>%
  summarize(missing = sum(na_count))

###############################################################
## Imputation of missing values using cold-deck imputation ####
###############################################################

# Code to get the earliest available value for each variable/country: I'll use
# this to bulk-fill missing values up to 2010. 

first_value <- regime_input %>%
  group_by(ccode) %>%
  arrange(year) %>%
  fill(c(1:ncol(regime_input)), .direction = "up") %>%
  filter(year == 2000)

vis_miss(first_value)  

# Now I will fill all missing values from consolidated year 2002:
consolidated_year_2002_filled <- consolidated_year_2002 %>%
  rbind(first_value) %>%
  group_by(ccode) %>%
  arrange(year) %>%
  fill(c(1:ncol(consolidated_year_2002)), .direction = "down") %>%
  filter(year == 2002)

# I want to check if NAs in 2007 are all for countries/vars that don't have any earlier value
nas2007 <- consolidated_year_2007 %>%
  filter_all(any_vars(is.na(.))) 
# Yes, they are, except for SLV primschoolenrol, but there's practically no difference. Will fill same way
consolidated_year_2007_filled <- consolidated_year_2007 %>%
  rbind(first_value) %>%
  group_by(ccode) %>%
  arrange(year) %>%
  fill(c(1:ncol(consolidated_year_2007)), .direction = "down") %>%
  filter(year == 2007)

# Now I will do the same using the last non-missing value for 2017:
last_value <- regime_input %>%
  group_by(ccode) %>%
  arrange(year) %>%
  fill(c(1:ncol(regime_input)), .direction = "down") %>%
  filter(year == 2020)

consolidated_year_2017_filled <- consolidated_year_2017 %>%
  rbind(last_value) %>%
  group_by(ccode) %>%
  arrange(year) %>%
  fill(c(1:ncol(consolidated_year_2017)), .direction = "up") %>%
  filter(year == 2017) %>%
  # whatever is left missing, I will fill with data from 2012
  rbind(consolidated_year_2012) %>%
  group_by(ccode) %>%
  arrange(year) %>%
  fill(c(1:ncol(consolidated_year_2017)), .direction = "down") %>%
  filter(year == 2017) 

# This function fills with the closest available value within 8 years: 
consolidated_year_2012_filled <- fill_with_closest_value_within_8_yrs(consolidated_year_2012, regime_input, 2012)

consolidated_score_filled <- consolidated_year_2002_filled %>%
  rbind(consolidated_year_2007_filled, consolidated_year_2012_filled, 
        consolidated_year_2017_filled) %>%
  ungroup()

vis_miss(consolidated_score_filled)

# write_csv(consolidated_score_filled,  "/Users/juan/Dropbox (Princeton)/Dissertation/DATA/my data sets/consolidated_score_filled_no_family.csv")  
# consolidated_score_filled <- read_csv("/Users/juan/Dropbox (Princeton)/Dissertation/DATA/my data sets/consolidated_score_filled_no_family.csv") 

# code to standardize scores: it takes the mean of each var and converts all values 
# into SD from the mean. I.e., 0 = mean. 1 = 1*SD + mean

standardized_score <- consolidated_score_filled %>%
  ungroup() %>%
  mutate(cnum = as.character(cnum),
         year = as.Date(as.character(year), format = "%Y")) %>%
  # so that I can standardize all numeric vars
  mutate_if(is.numeric, ~(scale(.) %>% as.vector)) %>%
  select(-leave_gender_bias) %>%
  mutate(year = year(year)) # turning year back to numeric to eliminate the spurious month:day.

# I want to cap the values for each variable, so that there are no values above 2.5 (to avoid that some countries score too high based on biased data)

capped_df_selection <- replace(standardized_score[6:ncol(standardized_score)], standardized_score[6:ncol(standardized_score)]> 2.5, 2.5)
capped_df_selection2 <- replace(capped_df_selection, capped_df_selection < -2.5, -2.5)

capped_standardized_score <- standardized_score
capped_standardized_score[6:ncol(standardized_score)] <- capped_df_selection2 

# write_csv(capped_standardized_score, "/Users/juan/Dropbox (Princeton)/Dissertation/DATA/my data sets/capped_standardized_score_no_family.csv")
# capped_standardized_score <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/capped_standardized_score_no_family.csv")
        ##########################################
        ##   CALCULATING THE SCORE ITSELF     ####
        ##########################################

# Final model
# Cold-deck imputation, capped at 2.5 SD, separate scores, segm worth half
## Fifth model: cold_deck imputation, inclusion + generosity + equity = decommodification
scores <- make_score(capped_standardized_score)

ven_year_2x_vector <- scores %>%
  select(country, ccode, year) %>%
  filter(ccode == "VEN", year == 2017)
scores2 <- scores %>%
  filter(!(ccode == "VEN" & year == 2017)) %>%
  bind_rows(ven_year_2x_vector)

# scores2 has all scores for Venezuela in 2017 as NA. 
# This table should be used to create the tile plots. 

ven_2012 <- scores %>%
  filter(ccode == "VEN", year == 2012)
ven_2012_marked_as_2017 <- ven_2012 %>%
  mutate(year = 2017)
scores3 <- scores2 %>%
  filter(!(ccode == "VEN" & year == 2017)) %>%
  rbind(ven_2012_marked_as_2017)

# scores3 has all scores for Venezuela in 2017 filled with 2012 data
# This table should be used for the cross-section and progress tables

# write_csv(scores3, "/Users/juan/Dropbox (Princeton)/Dissertation/DATA/my data sets/scores3_24.csv")
# scores3 <- read_csv ("/Users/juan/Dropbox (Princeton)/Dissertation/DATA/my data sets/scores3.csv")

title_label_scores <- "No family model: inclusion + generosity + equity = decommodification"
plot_scores(scores2)
score_plots <- plot_scores(scores2)
setwd("/Users/juan/Documents/Research/Welfare in Latin America - book/manuscript/Figures--too small/")
 ggsave(plot = score_plots[[1]], width = 2.5, height = 4, dpi = 320, filename = "inclusion_score.png", device = "png", bg = "white")
 ggsave(plot = score_plots[[2]], width = 2.5, height = 4, dpi = 320, filename = "generosity_score.png", device = "png", bg = "white")
 ggsave(plot = score_plots[[3]], width = 2.5, height = 4, dpi = 320, filename = "equity_score.png", device = "png", bg = "white")
 ggsave(plot = score_plots[[4]], width = 2.5, height = 4, dpi = 320, filename = "decommodification_score.png", device = "png", bg = "white")

plot_area_scores10(scores2)
area_score_plots <- plot_area_scores10(scores2)
 ggsave(plot = area_score_plots[[1]], width = 2.5, height = 4, dpi = 320, filename = "nc_transf_score.png", device = "png", bg = "white")
 ggsave(plot = area_score_plots[[2]], width = 2.5, height = 4, dpi = 320, filename = "cont_transf_score.png", device = "png", bg = "white")
 ggsave(plot = area_score_plots[[3]], width = 2.5, height = 4, dpi = 320, filename = "health_score.png", device = "png", bg = "white")
 ggsave(plot = area_score_plots[[4]], width = 2.5, height = 4, dpi = 320, filename = "education_score.png", device = "png", bg = "white")


# Create progress/evolution tables: 
# Function: clean table

clean_table <- function(table){
  mutate(table, across(ends_with("score"), ~ round(., digits = 1))) %>%
    rename(Inclusion = coverage_score, Generosity = generosity_score, Equity = equity_score,
           Decommodification = decom_score) %>%
    mutate(country = ifelse(str_detect(country, "Ven"), "Venezuela", 
                            ifelse(str_detect(country, "Bolivia"), "Bolivia", country)))
}
cut_2002 <- scores3 %>%
  filter(year == 2002) %>%
  select(country, coverage_score, generosity_score, equity_score, decom_score) %>%
  arrange(desc(decom_score)) %>%
  clean_table()
  
# write_csv(cut_2002, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch4/cut_2002_24.csv")

cut_2012 <- scores3 %>%
  filter(year == 2012) %>%
  select(country, coverage_score, generosity_score, equity_score, decom_score) %>%
  arrange(desc(decom_score))%>%
  clean_table()
# write_csv(cut_2012, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch4/cut_2012_24.csv")

cut_2017 <- scores3 %>%
  filter(year == 2017) %>%
  select(country, coverage_score, generosity_score, equity_score, decom_score) %>%
  arrange(desc(decom_score)) %>%
  clean_table()
# write_csv(cut_2017, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch4/cut_2017_24.csv")

progress_table <- get_progress(scores3) %>%
  left_join(codebook_countries_short) %>%
  select(country, coverage, generosity, equity, decommodification) %>%
  arrange(desc(decommodification)) %>%
  mutate_at(c(2:5), ~ round(., digits = 1))
write_clip(progress_table)
# write_csv(progress_table, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch4/progress_table_24.csv")

summary_progress_LA <- progress_table %>%
  summarise(across(2:5, ~ mean (.x))) %>%
  mutate(country = "Average 17 countries") %>%
  select(country, 1:4)

progress_all_w_avg <- progress_table %>%
  arrange(desc(decommodification)) %>%
  rbind(summary_progress_LA) %>%
  select(country, inclusion = coverage, generosity, equity, decommodification) %>%
  mutate(across(c(inclusion, generosity, equity, decommodification), ~ round(., digits = 1)))
# write_csv(progress_all_w_avg, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch4/progress_all_scores_24.csv")


## Plot country-specific progress in dimensions by policy area:
 
 
tidy_data <- capped_standardized_score %>%
   mutate(coverage_score = cct_incl + elderly_covered_any_pension +
            pen_rate_occup_all + UHC_index + school_enrol + preprim_enr_rate, 
          generosity_score = cct_vis_med_income_wageearner + pen_repl_rate_median_income
          + health_spending + educ_xpnd_pctGDP + mat_leave_weeks + mat_rep_rate, 
          equity_score = - ncp_social_deficit - pen_gap - priv_pens_pct_gdp -
            priv_health_exp_pct - pct_priv_school - pct_priv_preprim,
          decom_score =  coverage_score + generosity_score + equity_score, 
          transfers_nc_score = cct_incl + elderly_covered_any_pension 
          + cct_vis_med_income_wageearner - ncp_social_deficit, 
          transfers_cont_score = pen_rate_occup_all + pen_repl_rate_median_income 
          - pen_gap - priv_pens_pct_gdp, 
          health_score = health_spending + UHC_index - priv_health_exp_pct, 
          educ_score = school_enrol + educ_xpnd_pctGDP - pct_priv_school, 
          family_score = preprim_enr_rate + mat_leave_weeks + mat_rep_rate - pct_priv_preprim) %>%
   select(country, ccode, year, coverage_score, cct_incl, elderly_covered_any_pension ,
          pen_rate_occup_all , UHC_index , school_enrol , preprim_enr_rate, 
          generosity_score , cct_vis_med_income_wageearner , pen_repl_rate_median_income
          , health_spending , educ_xpnd_pctGDP , mat_leave_weeks , mat_rep_rate, 
          equity_score , ncp_social_deficit , pen_gap , priv_pens_pct_gdp ,
          priv_health_exp_pct , pct_priv_school , pct_priv_preprim,
          decom_score ,  coverage_score , generosity_score , equity_score, 
          transfers_nc_score, transfers_cont_score, health_score, 
          educ_score, family_score) %>%
   mutate(ncp_social_deficit = - ncp_social_deficit, 
          pen_gap = - pen_gap,
          priv_pens_pct_gdp = - priv_pens_pct_gdp,
          priv_health_exp_pct = - priv_health_exp_pct,
          pct_priv_school = - pct_priv_school,
          pct_priv_preprim = - pct_priv_preprim) %>%
   pivot_longer(cols = 4:30, names_to = "indicator", values_to = "value") %>%
   mutate(dimension = case_when(indicator %in% c("coverage_score", "cct_incl", "elderly_covered_any_pension" ,
                                                 "pen_rate_occup_all" , "UHC_index" , "school_enrol" , "preprim_enr_rate") ~ "Coverage", 
                                indicator %in% c("generosity_score", "cct_vis_med_income_wageearner" , "pen_repl_rate_median_income"
                                                 , "health_spending" , "educ_xpnd_pctGDP" , "mat_leave_weeks" , "mat_rep_rate") ~ "Generosity", 
                                indicator %in% c("equity_score", "ncp_social_deficit" , "pen_gap" , "priv_pens_pct_gdp" ,
                                                 "priv_health_exp_pct" , "pct_priv_school" , "pct_priv_preprim") ~ "Equity"), 
          area = case_when(indicator %in% c("transfers_nc_score", "cct_incl", "elderly_covered_any_pension", 
                                            "cct_vis_med_income_wageearner", "ncp_social_deficit") ~ "Transfers",  # I could give this the label " Non-Contributory Transfers"
                           indicator %in% c("transfers_cont_score", "pen_rate_occup_all", "pen_repl_rate_median_income", 
                                            "pen_gap" , "priv_pens_pct_gdp") ~ "Transfers", # I could give this the label "Contributory Transfers"
                           indicator %in% c("health_score", "UHC_index", "health_spending", "priv_health_exp_pct") ~ "Health", 
                           indicator %in% c("educ_score", "school_enrol", "educ_xpnd_pctGDP", "pct_priv_school") ~ "Education", 
                           indicator %in% c("family_score", "preprim_enr_rate", "mat_leave_weeks", "mat_rep_rate", "pct_priv_preprim") ~ "Family policies"))
 
 plot_progress_dimensions_by_area(tidy_data, "ARG")
 plot_progress_dimensions_by_area(tidy_data, "CHL")
 plot_progress_dimensions_by_area(tidy_data, "URY")
 plot_progress_dimensions_by_area(tidy_data, "BRA")
 plot_progress_dimensions_by_area(tidy_data, "COL")
 plot_progress_dimensions_by_area(tidy_data, "CRI")
 plot_progress_dimensions_by_area(tidy_data, "MEX")
 plot_progress_dimensions_by_area(tidy_data, "ECU")
 plot_progress_dimensions_by_area(tidy_data, "VEN")
 plot_progress_dimensions_by_area(tidy_data, "GTM")
 plot_progress_dimensions_by_area(tidy_data, "PER")
 plot_progress_dimensions_by_area(tidy_data, "NIC")
 plot_progress_dimensions_by_area(tidy_data, "SLV")
 
 plot_variables_progress_simple_score(capped_standardized_score)
 