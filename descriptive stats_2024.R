# Exploratory analysis / descriptive statistics for Dissertation

rm(list = ls())

# Packages
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(ggplot2)
library(stringr)

# DATA
codebook_countries <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries.csv", drop = 1)
codebook_countries_short <- read_csv ("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries_short.csv")
codebook_years <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_years.csv", drop = 1)
codebook_quintiles <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_quintiles.csv")
master_data3 <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/master_data3.csv")
explanatory_data <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data.csv") 
country_selection_ccodes <- c("ARG", "CHL", "COL", "URY")

# Constants 
LA18 <- c("216","221","222","224","225","226","249","229","230",
          "235","239","233","240","241","242","244","258","259")
LA17 <- c("216","221","222","224","225","226","229","230",
          "235","239","233","240","241","242","244","258","259")
LA11 <- c("216","221","222","224","225","226","229","233","244","258","259")

LA9 <- c("216", "222","224","225","226","229","233","258","259")

# functions
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

line_plot <- function(data, x_var, y_var, group_var = NULL){
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}}, linetype = {{group_var}})) +
    geom_line()+
    theme_minimal()
  
}

#### 

pcGDP_2002_2017 <- explanatory_data %>%
  filter(year %in% c(2002, 2017), ccode %in% country_selection) %>%
  select(country, year, pcgdp, pop) %>%
  pivot_wider(names_from = year, values_from = c(pcgdp, pop)) 


CCT_coverage_ppl <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/CCT_coverage_ppl.csv")

CCT_coverage_ppl %>%
  group_by(year) %>%
  filter(year<2020) %>%
  summarize(cct_benef_LA = sum(cct_cov_n_people, na.rm = T)) %>%
  ggplot(aes(year, cct_benef_LA)) +
  geom_col(color = "#61D04F", fill = "#61D04F") +
  geom_text(aes(label = round(cct_benef_LA/1000000, 1))) +
  theme_minimal()

CCT_coverage_ppl %>%
  group_by(year) %>%
  filter(year<2020, cnum %in% LA17) %>%
  summarize(cct_benef_LA = sum(cct_cov_n_people, na.rm = T)) %>%
  ggplot(aes(year, cct_benef_LA)) +
  geom_col(color = "grey", fill = "grey") +
  geom_text(aes(label = round(cct_benef_LA/1000000, 1)), nudge_y = 5000000) +
  theme_minimal() +
  labs(y = "Millions of beneficiaries")

arg_cct_amount_yrs <- master_data3 %>%
  filter(ccode == "ARG") %>%
  select(country, year, cct_program, cct_USD_max_ph, cct_vis_med_income_wageearner)


# For appendix: table with largest CCT program for each country/year: 
top_cct_table <- consolidated_score_filled %>% 
  select(country, year, cct_program) %>%
  mutate(country = ifelse(ccode == "BOL", "Bolivia", 
                          ifelse (ccode == "VEN", "Venezuela", country))) %>%
  pivot_wider(names_from = year, values_from = cct_program)

write_csv(top_cct_table, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/top_cct_table.csv")

# pension coverage passive pop
elderly_covered <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/elderly_covered.csv")

elderly_covered %>%
  group_by(year) %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            ncp = 100*mean(elderly_covered_ncp, na.rm =T)) %>%
  pivot_longer(cols = c(2:3), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, color = type))+
  geom_line() +
  geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal()

elderly_covered %>%
  group_by(year) %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
  rename(`all pensions` = all_pensions) %>%
  pivot_longer(cols = c(2:4), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, linetype = type))+
  geom_line() +
  labs(y = "Pension converage (%)") +
  geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, nudge_y = 3, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal()

elderly_covered_corrected <- elderly_covered %>%
  mutate(elderly_covered_ncp_correct = elderly_covered_any_pension - elderly_covered_cont) 

elderly_covered_corrected %>%
  group_by(year) %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp_correct, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
  rename(`all pensions` = all_pensions) %>%
  pivot_longer(cols = c(2:4), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, linetype = type))+
  geom_line() +
  labs(y = "Pension converage (%)") +
  geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, nudge_y = 3, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal()

# Comparison NCP vs cont pension benefit:
ncp_benef_comp <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/ncp_benef_comp.csv") %>%
  select(ccode, ncp_vis_cont)

table_pension_comp <- ncp_benef_comp %>%
  left_join(codebook_countries_short) %>%
  select(country, `NCP vs cont. pension benefit` = ncp_vis_cont)
write_csv(table_pension_comp, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch3/table_pension_comp.csv")

consolidated_score_filled <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/consolidated_score_filled_no_family.csv") 
# pension coverage active pop
consolidated_score_filled %>%
  mutate(year = as.factor(year), 
         pen_rate_occup_all = 100*pen_rate_occup_all) %>%
  filter(year == 2002 | year == 2017) %>%
  ggplot(aes(ccode, pen_rate_occup_all, fill = year)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pen_rate_occup_all, 1)), position = position_dodge(1), color = "black")+
  scale_fill_grey(start=0.6, end=0.8) +
  labs(y = "Pension contribution/enrolment rate (%)", x = "Country")+
  theme_minimal()

# pension differentiaal by quintiles
pensions_pruned_tidy <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/pensions_pruned_tidy.csv") %>%
  select(-c(cnum, country)) %>%
  left_join(codebook_countries_short)

pensions_pruned_tidy %>%
  filter(q == 1 | q == 5, cnum %in% LA17, ccode !=  "VEN") %>%
  mutate(quintile = q) %>%
  ggplot(aes(year, pen_rate_occup, linetype = quintile)) +
  geom_line() +
  facet_wrap("country") +
  labs(y = "Pension contribution/enrolment rate (%)") +
  theme_minimal()

Ury <- pensions_pruned_tidy %>%
  filter(ccode == "URY", q %in% c(1,5)) %>%
  select(ccode, year, pen_rate_occup, q) %>%
  pivot_wider(names_from = "q", values_from = pen_rate_occup, names_prefix = "q_") %>%
  mutate(diff = q_5 - q_1)

# private pensions: 
priv_pens_cont <- read_csv("/Users/juan/Documents/Dissertation/DATA/pensions/priv_pens_cont.csv") %>%
  filter(!is.na(ccode)) %>%
  pivot_longer(cols = 2:18, names_to = "year", values_to = "pct_enrolled_private") %>%
  mutate(year = as.numeric(year))

ggplot(priv_pens_cont, aes(year, pct_enrolled_private, linetype = ccode)) +
  geom_line() +
  geom_point(aes(shape=ccode)) +
  labs(y = "Contributors to a private pension system (%)", linetype = "Country", shape = "Country") +
  theme_minimal()

# For comparative analysis: ARG, CHL, URY

# gdp_growth
explanatory_cepal_wide <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/explanatory_data_cepal_wide.csv")

explanatory_cepal_wide %>%
  select(ccode, year, gdpgrowth) %>%
  filter(ccode %in% c("CHL", "URY", "ARG"), year < 2021) %>%
  ggplot(aes(year, gdpgrowth, linetype = ccode)) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(ylim = c(-15, +15))+
  labs(title = "GDP Growth (%). Chile, Uruguay and Argentina 1995-2020",
       y = "GPD growth (%)",  linetype = "Country") 

gdp_growth_table <- explanatory_cepal_wide %>%
  select(ccode, year, gdpgrowth) %>%
  filter(ccode %in% c("CHL", "URY", "ARG"), year < 2021) %>%
  pivot_wider(names_from = ccode, values_from = gdpgrowth)

# 3 countries, percentage of elders receiving any kind of pension
elderly_covered %>%
  group_by(year) %>%
  filter(ccode %in% c("CHL", "URY", "ARG")) %>%
#  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
#            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
#            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
#  pivot_longer(cols = c(2:4), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, 100*elderly_covered_any_pension, color = ccode))+
  geom_line() +
  geom_text(aes(label = 100* elderly_covered_any_pension), check_overlap = T, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal()

# Chile, Uruguay, Argentina: elders covered in each country by pension type
elderly_covered %>%
  filter(ccode %in% c("CHL", "URY", "ARG")) %>%
  group_by(year, ccode) %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
  pivot_longer(cols = c(3:5), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, linetype = ccode, shape = type))+
  geom_line() +
  geom_point(aes(shape = type)) +
  #geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  scale_y_continuous(breaks = seq(0,100, 10))+
  theme_minimal() +
  labs(y = "Percent", linetype = "Country", shape = "Pension type") 

pensions_by_type_3countries <- elderly_covered %>%
  filter(ccode %in% c("CHL", "URY", "ARG")) %>%
  group_by(year, ccode) %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T))
pensions_by_type_3countries_wide <- pensions_by_type_3countries %>%
  pivot_wider(names_from = "ccode", values_from = c(3:5))

# Chile: elders covered, by pension type
elderly_covered %>%
  group_by(year) %>%
  filter(ccode =="CHL") %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
  pivot_longer(cols = c(2:4), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, linetype = type))+
  geom_line() +
  geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, color = "black", vjust = -0.5)+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal() +
  labs(       y = "Percent", color = "Pension type") 
  
# Uruguay: elders covered, by pension type
elderly_covered %>%
  group_by(year) %>%
  filter(ccode =="URY") %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
  pivot_longer(cols = c(2:4), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, color = type))+
  geom_line() +
  geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal() +
  labs(title = "Percentage of elders receiving a pension. Uruguay 2000-2019",
       y = "Percent", color = "Pension type") 

# Argentina: elders covered, by pension type
elderly_covered %>%
  group_by(year) %>%
  filter(ccode =="ARG") %>%
  summarize(all_pensions = 100*mean(elderly_covered_any_pension, na.rm =T), 
            `non-contributory` = 100*mean(elderly_covered_ncp, na.rm =T), 
            `contributory` = 100*mean(elderly_covered_cont, na.rm = T)) %>%
  pivot_longer(cols = c(2:4), names_to = "type", values_to = "pension_coverage") %>%
  ggplot(aes(year, pension_coverage, color = type))+
  geom_line() +
  geom_text(aes(label = round(pension_coverage, 1)), check_overlap = T, color = "black")+
  coord_cartesian(ylim = c(0, 100))+
  theme_minimal() +
  labs(title = "Percentage of elders receiving a pension. Argentina 2000-2019",
       y = "Percent", color = "Pension type") 

# Private pensions
priv_pens_pct_gdp <- read_csv("/Users/juan/Documents/Dissertation/DATA/pensions/priv_pens_pct_gdp.csv") %>%
  filter(!is.na(ccode)) %>%
  pivot_longer(cols = 2:18, names_to = "year", values_to = "priv_pens_pct_gdp") %>%
  mutate(year = as.numeric(year)) %>%
  filter(ccode %in% c("URY", "ARG", "CHL"))

ggplot(priv_pens_pct_gdp, aes(year, priv_pens_pct_gdp, linetype = ccode, color = ccode)) +
  geom_line() +
  labs(title = "Private pension funds as % of the GDP", y = "Percentage of the GDP", linetype = "Country", color = "Country") +
  theme_minimal()

ggplot(filter(priv_pens_cont, ccode %in% c("URY", "ARG", "CHL")), aes(year, pct_enrolled_private, linetype = ccode)) +
  geom_line() +
  labs(y = "Contributors to a private pension system (%)", linetype = "Country", color = "black") +
  theme_minimal()

# pension coverage active pop
consolidated_score_filled %>%
  mutate(pen_rate_occup_all = 100*pen_rate_occup_all) %>%
  filter(ccode %in% c("CHL", "URY", "ARG")) %>%
  ggplot(aes(year, pen_rate_occup_all, color = ccode)) +
  geom_line ()+
  coord_cartesian(ylim = c(0, 100))+
  labs(y = "Pension contribution/enrolment rate (%)", color = "Country")+
  theme_minimal()

# Electoral competition in Arg, Chl, Ury

elect_compet_Cl_Ar_Uy <- C_index %>%
  filter(ccode %in% c("CHL", "URY", "ARG"), year> 1989)

ggplot(elect_compet_Cl_Ar_Uy, aes(x = year, y = elect_comp, linetype = ccode)) +
geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Electoral Competition Index", linetype = "Country")+
  theme_minimal()

elect_compet_several <- C_index %>%
  filter(ccode %in% c("CHL", "URY", "ARG", "MEX", "COL"), year> 1989)

ggplot(elect_compet_several, aes(x = year, y = elect_comp, linetype = ccode)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Electoral Competition Index", linetype = "Country")+
  
  theme_minimal()

# Union density

union_density_3countries <- explanatory_data %>%
  filter(ccode %in% c("CHL", "URY", "ARG")) %>%
  select(year, ccode, union_density) %>%
  filter(!is.na(union_density)) %>%
  pivot_wider(names_from = "ccode", values_from = "union_density")


# Education: plain enrollment 

school_enrolment <- master_data3 %>%
  select(ccode, year, primschoolenrol, upsecschoolenrol, preprim_enr_rate) %>%
  filter(year %in% c(2001, 2005, 2009, 2013, 2017)) %>%
  arrange(year, ccode) %>%
  mutate(across(c(primschoolenrol, upsecschoolenrol, preprim_enr_rate), ~ round(., digits = 2))) %>%
  pivot_wider(names_from = year, values_from = c(primschoolenrol, upsecschoolenrol, preprim_enr_rate)) 

write_csv(school_enrolment,"/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch3/school_enrolment.csv" )

# Education spending: 

ggplot(consolidated_score_data2, aes(ccode, educ_xpnd_pctGDP, fill = factor(year))) +
  geom_col(position = "dodge")+
  theme_minimal() +
  scale_fill_brewer(palette = 8) +
  labs(y = "% of GDP", x = "Country", fill = "Year")

# Expenditures: 


  
master_data3_LA11 <- master_data3 %>%
  filter(cnum %in% LA11)

# line_plot(master_data3_LA11, year, housing_xpnd_pctGDP, ccode) +
#   geom_smooth(method = "lm", se = F) 
# 
# line_plot(master_data3_LA11, year, educ_xpnd_pctGDP, ccode) +
#   geom_smooth(method = "lm", se = F) 
#   
# line_plot(master_data3_LA11, year, health_pub_xpnd_pctGDP, ccode) +
#   geom_smooth(method = "lm", se = F) 
# 
# Education: private enrolment: 

priv_ed_primLA11 <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/priv_ed_primLA11.csv") %>%
  left_join(codebook_countries, by = "country")

 priv_ed_primLA8 <- priv_ed_primLA11 %>%
  filter(ccode !="MEX", ccode !="CRI", ccode !="BOL")
 
ggplot(priv_ed_primLA11, aes(x = year, y = priv_ed_prim)) + # Note that the data for this (and the geom_smooth)
                              # is different than the data for the individual lines for the countries)
  geom_line(data = priv_ed_primLA8[!is.na(priv_ed_primLA8$priv_ed_prim),], 
            aes(linetype = ccode)) + # this code fills the missing points to make a continuous line
    geom_smooth(se = FALSE) +
  labs(title = "Enrolment in private education for primary school, Latin America
       1990-2020", y = "Percentage", linetype = "Country", color = "Country") +
  theme_minimal()

master_data3_LA11 <- master_data3 %>% filter(cnum %in% LA11)

ggplot(master_data3_LA11, aes(x = year, y = pct_priv_secschool)) + # Note that the data for this (and the geom_smooth)
  # is different than the data for the individual lines for the countries)
  geom_line(data = master_data3_LA11[!is.na(master_data3_LA11$pct_priv_secschool),], 
            aes(linetype = ccode, color = ccode)) + # this code fills the missing points to make a continuous line
  geom_smooth(se = FALSE) +
  labs(title = "Enrolment in private education for secondary school, Latin America
       2000-2020, selected countries", y = "Percentage", linetype = "Country", color = "Country") +
  theme_minimal()

# line_plot(master_data3, year, pct_priv_preprim) + 
#   geom_smooth(method = "lm")

line_plot(master_data3, year, preprim_enr_rate, ccode) 
table_ecc <- master_data3 %>%
  select(country, year, preprim_enr_rate) %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2004, 2008, 2012, 2016)) %>%
  mutate(preprim_enr_rate = round(preprim_enr_rate, digits = 2)) %>%
  pivot_wider(names_from = year, values_from = preprim_enr_rate) %>%
  arrange(country) 
write_csv(table_ecc, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/Ch3/table_ecc.csv")

line_plot(master_data3, year, pct_priv_preprim, ccode)
table_ecc_priv <- master_data3 %>%
  select(country, year, pct_priv_preprim) %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2004, 2008, 2012, 2016)) %>%
  mutate(pct_priv_preprim = round(pct_priv_preprim, digits = 2)) %>%
  pivot_wider(names_from = year, values_from = pct_priv_preprim) %>%
  arrange(country)

ggplot(master_data3_LA11, aes(x = year, y = pct_priv_preprim)) + # Note that the data for this (and the geom_smooth)
  # is different than the data for the individual lines for the countries)
  geom_line(data = master_data3_LA11[!is.na(master_data3_LA11$pct_priv_preprim),], 
            aes(linetype = ccode)) + # this code fills the missing points to make a continuous line
  geom_smooth(se = FALSE, color = "grey", size = 2) +
  labs(#title = "Enrolment in private ECC (preprimary), Latin America
       #2000-2020, selected countries", 
       y = "Percentage", linetype = "Country") +
  theme_minimal()


# Health: UHC


ggplot(master_data3, aes(x = year, y = UHC_index)) + 
  geom_line(data = master_data3[!is.na(master_data3$UHC_index),], 
            aes(linetype = ccode)) + 
  geom_smooth(se = FALSE, color = "grey", size = 2) +
  labs(linetype = "Country")+
  theme_minimal()

# Selecting countries bc there aren't enough "linetypes" for 17 countries: 
# And fixing the units of UHC (it's 1-100)
master_data3_LA9 <- master_data3 %>%
  filter(cnum %in% LA9)

ggplot(master_data3_LA9, aes(x = year, y = UHC_index)) + 
  geom_line(data = master_data3_LA9[!is.na(master_data3_LA9$UHC_index),], 
            aes(linetype = ccode)) + 
  geom_smooth(se = FALSE, color = "grey", size = 2) +
  labs(linetype = "Country")+
  theme_minimal()

ggplot(data = master_data3_LA11[!is.na(master_data3_LA11$UHC_index),], 
            aes(linetype = ccode)) + # this code fills the missing points to make a continuous line
  geom_line()+
  theme_minimal()


# only URY, ARG, CHL
selected_countries <- master_data3 %>%
  filter(ccode %in% c("URY", "ARG", "CHL")) %>%
  mutate(UHC_index = UHC_index * 100)


line_plot(selected_countries, year, cat_hexp_risk, ccode)

# Public spending in health: 
line_plot(master_data3, year, gov_hexp_ppp_WB_cnstnt, ccode)

consolidated_master_data3_2002 <- consolidate_5_years(master_data3, 2002)
consolidated_master_data3_2007 <- consolidate_5_years(master_data3, 2007)
consolidated_master_data3_2012 <- consolidate_5_years(master_data3, 2012)
consolidated_master_data3_2017 <- consolidate_5_years(master_data3, 2017)

consolidated_master_data3 <- consolidated_master_data3_2002 %>%
  rbind(consolidated_master_data3_2007, consolidated_master_data3_2012, 
        consolidated_master_data3_2017) %>%
  ungroup()

ggplot(consolidated_master_data3, aes(ccode, govt_hexp_pctGDP_WB, fill = factor(year))) +
  geom_col(position = "dodge")+
  theme_minimal() +
  scale_fill_grey() +
  labs(y = "Percentage of GDP", x = "Country", fill = "Year")

ggplot(consolidated_master_data3, aes(ccode, gov_hexp_ppp_WB, fill = factor(year))) +
  geom_col(position = "dodge")+
  theme_minimal() +
  scale_fill_grey() +
  labs(y = "USD (PPP)", x = "Country", fill = "Year")

# private spending: 

WB_health_exp_LA_clean <- fread("/Users/juan/Documents/Dissertation/DATA/my data sets/WB_health_exp_LA_2023.csv")

health_exp_LA <- WB_health_exp_LA_clean %>%
  filter(cnum %in% LA17, year>1999) %>%
  mutate(indicator_abrev = case_when(indicator == "Domestic private health expenditure (% of current health expenditure)" ~ "Private Health Exp as % CHE", 
                                     indicator == "Domestic private health expenditure per capita (current US$)" ~ "Private Health Exp pc in USD", 
                                     indicator == "Domestic private health expenditure per capita, PPP (current international $)" ~ "Private Health Exp pc in PPP", 
                                     indicator == "Out-of-pocket expenditure (% of current health expenditure)" ~ "Out-of-pocket Exp as % CHE",
                                     indicator == "Out-of-pocket expenditure per capita (current US$)" ~ "Out-of-pocket Exp pc in USD", 
                                     indicator == "Out-of-pocket expenditure per capita, PPP (current international $)" ~ "Out-of-pocket Exp pc in PPP"))

hexp_LA_non_pct <- health_exp_LA %>%
  filter(!str_detect(indicator_abrev, "%"))

hexp_LA_pct <- health_exp_LA %>%
  filter(str_detect(indicator_abrev, "%"))

ggplot(hexp_LA_non_pct, aes(year, value, shape = indicator_abrev, linetype = indicator_abrev)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(data = hexp_LA_non_pct, aes(year, value), se = F, color = "black") +
  theme_minimal()+
  theme(legend.position="right")+
  labs(shape = "Indicator", linetype = "Indicator", y = "Dollars per capita")

ggplot(hexp_LA_pct, aes(year, value, shape = indicator_abrev, linetype = indicator_abrev)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(data = hexp_LA_pct, aes(year, value), se = F, color = "black") +
  theme_minimal()+
  theme(legend.position="bottom") +
  labs(y = "Percentage", shape = "Indicator", linetype = "Indicator", )

health_exp_LA17_wide <- health_exp_LA %>%
  mutate(country = ifelse(ccode == "BOL", "Bolivia", 
                          ifelse (ccode == "VEN", "Venezuela", country))) %>%
  select(country, year, indicator, value) %>%
  pivot_wider(id_cols = c(1,2), names_from = indicator, values_from = value)


ggplot(health_exp_LA11_wide, aes(year))+
  geom_line(aes(y = `Domestic private health expenditure (% of current health expenditure)`*10, linetype = "as % CHE")) +
  geom_line(aes(y = `Domestic private health expenditure per capita (current US$)`, linetype = "per capita in USD"))+
  geom_line(aes(y = `Out-of-pocket expenditure per capita, PPP (current international $)`, linetype = "out-of-pocket in PPP"))+
  scale_y_continuous(name = "Private xpenditure in USD or PPP", sec.axis = sec_axis(trans = ~. /10, name = "OOP expenditure as % CHE"))+
  facet_wrap("country") +
  theme(legend.position="bottom")+
  theme_minimal()

ggplot(health_exp_LA17_wide, aes(year))+
  geom_line(aes(y = `Domestic private health expenditure (% of current health expenditure)`*10, linetype = "as % CHE")) +
  geom_line(aes(y = `Domestic private health expenditure per capita (current US$)`, linetype = "per capita in USD"))+
  geom_line(aes(y = `Out-of-pocket expenditure per capita, PPP (current international $)`, linetype = "out-of-pocket in PPP"))+
  scale_y_continuous(name = "Private xpenditure in current or PPP USD", sec.axis = sec_axis(trans = ~. /10, name = "OOP expenditure as % CHE"))+
  scale_x_continuous(breaks = c(2001, 2010, 2019))+
  facet_wrap("country") +
  theme(axis.x.text = element_text(angle = 90))+
  labs(linetype = "Private spending measure")+
  theme_minimal()

## Health numbers for comparative analysis:

master_data3_Ar_Cl_Uy <- master_data3 %>%
  filter(ccode %in% c("ARG", "CHL", "URY"))

ggplot(master_data3_Ar_Cl_Uy, aes(year, gov_hexp_ppp_WB_cnstnt, linetype = country)) +
  geom_line() +
  theme_minimal()+
  labs(y = "US Dollars")

ggplot(master_data3_Ar_Cl_Uy, aes(year, govt_hexp_pctGDP_WB, linetype = country)) +
  geom_line() +
  theme_minimal()+
  labs(y = "Percentage of the GDP")  +
  coord_cartesian(ylim = c(0, 8))

line_plot(master_data3_Ar_Cl_Uy, year, gov_hexp_ppp_WB_cnstnt, ccode)
line_plot(master_data3_Ar_Cl_Uy, year, govt_hexp_pctGDP_WB, ccode)

ggplot(master_data3_Ar_Cl_Uy, aes(x = year, y = UHC_index)) + # Note that the data for this (and the geom_smooth)
  # is different than the data for the individual lines for the countries)
  geom_line(data = selected_countries[!is.na(selected_countries$UHC_index),], 
            aes(linetype = ccode)) + # this code fills the missing points to make a continuous line
  theme_minimal() +
  labs(linetype = "Country")

WB_health_exp_LA_clean <- fread("/Users/juan/Documents/Dissertation/DATA/my data sets/WB_health_exp_LA.csv")


health_exp_Ar_Cl_Uy <- WB_health_exp_LA_clean %>%
  filter(ccode %in% c("ARG", "CHL", "URY"), year>1999) %>%
  mutate(indicator_abrev = case_when(indicator == "Domestic private health expenditure (% of current health expenditure)" ~ "Private Health Exp as % CHE", 
                                     indicator == "Domestic private health expenditure per capita (current US$)" ~ "Private Health Exp pc in USD", 
                                     indicator == "Domestic private health expenditure per capita, PPP (current international $)" ~ "Private Health Exp pc in PPP", 
                                     indicator == "Out-of-pocket expenditure (% of current health expenditure)" ~ "Out-of-pocket Exp as % CHE",
                                     indicator == "Out-of-pocket expenditure per capita (current US$)" ~ "Out-of-pocket Exp pc in USD", 
                                     indicator == "Out-of-pocket expenditure per capita, PPP (current international $)" ~ "Out-of-pocket Exp pc in PPP"))

hexp_comp_non_pct <- health_exp_Ar_Cl_Uy %>%
  filter(!str_detect(indicator_abrev, "%"))

hexp_comp_pct <- health_exp_Ar_Cl_Uy %>%
  filter(str_detect(indicator_abrev, "%"))

ggplot(hexp_comp_non_pct, aes(year, value, color = indicator_abrev)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(data = hexp_comp_non_pct, aes(year, value), se = T) +
  theme_minimal()+
  theme(legend.position="right")+
  labs(color = "Indicator", y = "Dollars per capita")

ggplot(hexp_comp_pct, aes(year, value, color = indicator_abrev)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth(data = hexp_comp_pct, aes(year, value)) +
  theme_minimal()+
  theme(legend.position="bottom") +
  labs(y = "Percentage", color = "Indicator")

ggplot(hexp_comp_pct, aes(year, value, color = indicator_abrev)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth(data = hexp_comp_pct, aes(year, value)) +
  theme_minimal()+
  theme(legend.position="bottom") +
  facet_wrap(~country) +
  labs(y = "Percentage", color = "Indicator")

ggplot(hexp_comp_pct, aes(year, value, color = indicator_abrev)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth(data = hexp_comp_pct, aes(year, value)) +
  theme_minimal()+
  theme(legend.position="bottom") +
  labs(y = "Percentage", color = "Indicator")

health_exp_Ar_Cl_Uy_wide <- health_exp_Ar_Cl_Uy %>%
  select(country, year, indicator, value) %>%
  pivot_wider(id_cols = c(1,2), names_from = indicator, values_from = value)


oop_costs_table <- health_exp_Ar_Cl_Uy %>%
  filter(str_detect(indicator, "Out-"), str_detect(indicator_abrev, "PPP")) %>%
  select(country, year, value)

ggplot(oop_costs_table, aes(year, value, color = country)) +
  theme_minimal()+
  geom_line()

oop_pct_table <- health_exp_Ar_Cl_Uy %>%
  filter(str_detect(indicator, "Out-"), str_detect(indicator_abrev, "%")) %>%
  select(country, year, value)

ggplot(oop_pct_table, aes(year, value, linetype = country)) +
  theme_minimal()+
  geom_line() +
  coord_cartesian(ylim = c(0, 50))

ggplot(health_exp_Ar_Cl_Uy_wide, aes(year))+
  geom_line(aes(y = `Domestic private health expenditure (% of current health expenditure)`*10, linetype = "as % CHE")) +
  geom_line(aes(y = `Domestic private health expenditure per capita (current US$)`, linetype = "per capita in USD"))+
  geom_line(aes(y = `Out-of-pocket expenditure per capita, PPP (current international $)`, linetype = "out-of-pocket in PPP"))+
  scale_y_continuous(name = "Private xpenditure in USD or PPP", sec.axis = sec_axis(trans = ~. /10, name = "OOP expenditure as % CHE"))+
  facet_wrap("country") +
  theme(legend.position="bottom")+
  theme_minimal()

ggplot(health_exp_Ar_Cl_Uy_wide, aes(year))+
  geom_line(aes(y = `Domestic private health expenditure (% of current health expenditure)`*10, linetype = "as % CHE")) +
  geom_line(aes(y = `Domestic private health expenditure per capita (current US$)`, linetype = "per capita in USD"))+
  geom_line(aes(y = `Out-of-pocket expenditure per capita, PPP (current international $)`, linetype = "out-of-pocket in PPP"))+
  scale_y_continuous(name = "Private xpenditure in current or PPP USD", sec.axis = sec_axis(trans = ~. /10, name = "OOP expenditure as % CHE"))+
  scale_x_continuous(breaks = c(2001, 2010, 2019))+
  facet_wrap("country") +
  theme(axis.x.text = element_text(angle = 90))+
  labs(linetype = "Private spending measure")+
  theme_minimal()


# Parental leave
ggplot(consolidated_score_data2, aes(ccode, mat_leave_weeks, fill = factor(year))) +
  geom_col(position = "dodge")+
  scale_fill_brewer(palette = 2) +
  theme_minimal()+
  labs(y = "Maternity leave in weeks", x = "Country", fill = "Year")

parental_leave_table <- master_data3 %>% 
  select(country, year, mat_leave_weeks, pat_leave_days) %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2019)) %>%
  arrange(year) %>%
  pivot_wider(names_from = year, values_from = c(mat_leave_weeks, pat_leave_days)) %>%
  arrange(country)

write_csv(parental_leave_table, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/parental_leave_table.csv")


pen_repl_rate <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/pen_repl_rate.csv") %>%
  left_join(codebook_countries) %>%
  filter(cnum %in% LA17)
ncp_benef_comp <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/ncp_benef_comp.csv") %>%
  select(ccode, ncp_vis_cont)

table_ncp_appendix <- ncp_benef_comp %>%
  full_join(pen_repl_rate, by = "ccode") %>%
  select(country, pen_repl_rate_median_income, ncp_vis_cont) %>%
  arrange(country)

write_csv(table_ncp_appendix, "/Users/juan/Dropbox (Princeton)/Dissertation/write-ups/tables/pen_repl_rate_and_ncp_comp.csv")

female_part <- master_data3 %>%
  select(country, year, partic_femenina) %>%
  pivot_wider(names_from = year, values_from = partic_femenina)

female_part_gap <- master_data3 %>%
  mutate(female_part_gap = partic_femenina - partic_masculina) %>%
  select(country, year, female_part_gap) %>%
  pivot_wider(names_from = year, values_from = female_part_gap)

data_2001_2018 <- master_data3 %>%
  filter(year %in% c(2000, 2020))
ggplot(data_2001_2018, aes(ccode, mat_leave_weeks, fill = as.factor(year))) +
  geom_col(position = "dodge") +
  theme_light() +
  geom_hline(yintercept = 14)+
  labs(title = "Maternity leave length in weeks, Latin America 2001, 2020", 
       y = "Weeks", x = "Country", fill = "Year")

ggplot(data_2001_2018, aes(ccode, mat_rep_rate, fill = as.factor(year))) +
  geom_col(position = "dodge") +
  theme_light() +
  scale_fill_grey()+
  labs(    y = "Percentage", x = "Country", fill = "Year")
#++=+=++=++++++==+=++


# Regression for Comparative politics article: 
explanatory_data2 <- explanatory_data %>%
  # here I'm adding missing data on unemployment: 
  mutate(unemployment = case_when(ccode == "ECU" & year == 2002 ~ 8.47, # this is Ecuador's value for 2001
                                  ccode == "GTM" & year == 2002 ~ 2.2, # from datosmacro https://datosmacro.expansion.com/paro-epa/guatemala?sc=UEPAR-&anio=2002
                                  ccode == "GTM" & year == 2007 ~ 2.3, # from datosmacro
                                  ccode == "PER" & year == 2002 ~ 7.9, # from datosmacro.com https://datosmacro.expansion.com/paro-epa/peru?sc=UEPAR-&anio=2002
                                  ccode == "URY" & year == 2002 ~ 18.6, 
                                  TRUE ~ unemployment)) %>%# data for GTM 2006, from ILO's report (folder: data/unemployment)
  filter(year > 1999, year <2021)


fmla1 <- elderly_covered_any_pension ~ pcgdp_in_k + last3yrs_pcgdp_growth_avg + pinktide_yrs + year + lag_curr_acct_blz  + trade_openness + unemployment + pinktide_yrs*last3yrs_pcgdp_growth_avg
model1 <- lm(fmla1, explanatory_data)
summary(model1)
summary(model1, robust = T)

# Same with Prais-Winsten:
fmla1 <- elderly_covered_any_pension ~ pcgdp_in_k + last3yrs_pcgdp_growth_avg + pinktide_yrs + lag_curr_acct_blz  + trade_openness 
prais_model1 <- prais_winsten(elderly_covered_any_pension ~ pcgdp_in_k + last3yrs_pcgdp_growth_avg + pinktide_yrs + lag_curr_acct_blz  + trade_openness,
                              data = explanatory_data, 
                              index = "year")

fmla2 <- delta_decom_score ~ pcgdp_in_k +  pinktide_yrs + lag_curr_acct_blz + year
model2 <- lm(fmla2, explanatory_data2)
summary(model2)
summary(model2, robust = T)

fmla3 <- delta_decom_score ~ pcgdp_in_k +  pinktide_yrs + trade_openness + year
model3 <-  lm(fmla3, explanatory_data2)
summary(model3)
summary(model3, robust = T)

fmla4 <- delta_decom_score ~ pcgdp_in_k + unemployment +  pinktide_yrs + year
model4 <- lm(fmla4, explanatory_data2) 
summary(model4)
summary(model4, robust = T)

fmla5 <- delta_decom_score ~ pcgdp_in_k + pinktide_yrs*last3yrs_pcgdp_growth_avg+ trade_openness  +  year
model5 <- lm(fmla5,  explanatory_data2)
summary(model5)
summary(model5, robust = T)

##---3-3--3-3-#_#_#_##3-3

ggplot(progress_univ, aes(year_2002, year_2017)) +
  geom_point() +
  geom_text(aes(label = ccode), nudge_y = 2) +
  geom_abline(slope = 1)


# Check differences between my data and Arenas de Mesa's 

pen_cont_AdM <- fread("/Users/juan/Documents/Dissertation/DATA/pensions/pen_cont_AdM.csv", header = T) %>%
  pivot_longer(cols = 2:19, names_to = "year") %>%
  mutate(year = as.numeric(year), 
         pencont_adm = as.numeric(gsub(",", ".", gsub("\\.", "",value)))/100, # This will give a warning, but it's ok
         ccode = toupper(ccode)) %>%
  select(-value)

comparison <- pensions_complete %>%
  select(ccode, year, pen_rate_occup_all) %>%
    full_join(pen_cont_AdM, by = c("ccode", "year"))
  
pensions_all<- fread("/Users/juan/Documents/Dissertation/DATA/my data sets/cepal_pensions_all.csv")
pensions_all_LA17 <- pensions_all %>%
  filter(cnum %in% LA17)