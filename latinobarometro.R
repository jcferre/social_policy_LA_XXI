# Latinobarometro

library(haven)
library(readxl)
library(dplyr)
library(readr)
library(tidyverse)
library(clipr)
# Constants 
LA17 <- c("216","221","222","224","225","226","229","230",
          "235","239","233","240","241","242","244","258","259")
# Data: 
codebook_countries_short <- read_csv ("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries_short.csv")
ccodes <- read_xlsx("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/lat_bar_country_codes.xlsx")

lat_bar95 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004297-Latinobarometro_1995_sav/Latinobarometro_1995_data_english_spss_v2014_06_27.sav")
lat_bar97 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004300-Latinobarometro_1997_sav/Latinobarometro_1997_datos_english_spss_v2014_06_27.sav")
lat_bar98 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004303-Latinobarometro_1998_sav/Latinobarometro_1998_datos_english_v2014_06_27.sav")
lat_bar02 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004312-Latinobarometro_2002_sav/Latinobarometro_2002_datos_eng_v2014_06_27.sav")
lat_bar03 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004315-Latinobarometro_2003_sav/Latinobarometro_2003_datos_eng_v2014_06_27.sav")
lat_bar04 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004318-Latinobarometro_2004_sav/Latinobarometro_2004_datos_eng_v2014_06_27.sav")
lat_bar05 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004322-Latinobarometro_2005_sav/Latinobarometro_2005_datos_eng_v2014_06_27.sav")
lat_bar06 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004325-Latinobarometro_2006_sav/Latinobarometro_2006_datos_eng_v2014_06_27.sav")
lat_bar07 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004328-Latinobarometro_2007_sav/Latinobarometro_2007_datos_eng_v2014_06_27.sav")
lat_bar08 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00004331-Latinobarometro_2008_sav/Latinobarometro_2008_datos_eng_v2014_06_27.sav")

load("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00005906-Latinobarometro2016_r/Latinobarometro2016Eng_v20170205.rdata")
lat_bar16 <- Latinobarometro2016Eng_v20170205 %>%
  left_join(ccodes)
rm(Latinobarometro2016Eng_v20170205)
latbar_18 <- read_sav("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/Latinobarometro_2018_Esp_Spss_v20190303.sav")

health_sat03 <- lat_bar03 %>%
  select(idenpa, year = numinves, health_sat = p25na)

health_sat04 <- lat_bar04 %>%
  select(idenpa, year = numinves, health_sat = p39sta)

health_sat05 <- lat_bar05 %>%
  select(idenpa, year = numinves, health_sat = p91sta)

health_sat06 <- lat_bar06 %>%
  select(idenpa, year = numinves, health_sat = p80st.a)

health_sat07 <- lat_bar07 %>%
  select(idenpa, year = numinves, health_sat = p57st.a)

health_sat16 <- lat_bar16 %>%
  select(idenpa, year = numinves, health_sat = P34STA)

health_satisf <- health_sat03 %>%
  full_join(health_sat04) %>%
  full_join(health_sat05) %>%
  full_join(health_sat06) %>%
  full_join(health_sat07) %>%
  full_join(health_sat16)  %>%
  left_join(ccodes) %>%
  left_join(codebook_countries_short)

health_satisf_table <- health_satisf %>%
  filter(cnum %in% (LA17)) %>%
  group_by(year, ccode) %>%
  count(health_sat) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) 

health_satisf_table_wide <- health_satisf_table %>%
  pivot_wider(names_from = year, values_from = pct)
  
# Muy satisfecho	1
# Bastante satisfecho	2
# No muy satisfecho	3
# Para nada satisfecho	4

health_sat_ury_arg_chl <- health_satisf_table %>%
  filter(ccode %in% c("ARG", "CHL", "URY"), !is.na(health_sat), 
         health_sat %in% c(1, 2, 3, 4)) %>%
  mutate(health_sat = factor(health_sat))

levels(health_sat_ury_arg_chl$health_sat) <-   c("Very satisfied", "Fairly satisfied", "Not very satisfied", "Not at all satisfied")

ggplot(health_sat_ury_arg_chl, aes(year, pct, color = ccode)) +
  geom_line() +
  facet_grid(vars(health_sat)) +
  theme_light()

health_sat_wide_ury_arg_chl <- health_satisf_table_wide %>%
  filter(ccode %in% c("ARG", "CHL", "URY"), !is.na(health_sat)) 

write_clip(health_sat_ury_arg_chl)

lat_bar95_filtered <- lat_bar95 %>%
  left_join(ccodes, by = c("pais" = "idenpa")) %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

support_priv_pen95 <- lat_bar95_filtered %>%
  group_by(country) %>%
#  mutate(p60g = ifelse(p60g < 0, NA, p60g)) %>%
  count(p60g) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(preference = p60g)


lat_bar98_filtered <- lat_bar98 %>%
  left_join(ccodes, by = "idenpa") %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

support_priv_pen98 <- lat_bar98_filtered %>%
  group_by(country) %>%
  mutate(sp12g = ifelse(sp12g < 0, NA, sp12g)) %>%
  count(sp12g) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(preference = sp12g)


lat_bar08_filtered <- lat_bar08 %>%
  left_join(ccodes, by = "idenpa") %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

support_priv_pen08 <- lat_bar08_filtered %>%
  group_by(country) %>%
  count(p93st.f) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(preference = p93st.f)

support_priv_pen_95_98_08 <- support_priv_pen95 %>%
  left_join(support_priv_pen98, by = c("country", "preference"), suffix = c("_95", "_98")) %>%
  left_join(support_priv_pen08, by = c("country", "preference"), suffix = c("", "_08")) 
  
clipr::write_clip(support_priv_pen_95_98_08)

load(file = "/Users/juan/Documents/Dissertation/DATA/Latinobarometro/F00006501-Latinobarometro2017_r/Latinobarometro2017Eng_v20180117.rdata")
lat_bar17_filtered <- Latinobarometro2017Eng_v20180117 %>%
  left_join(ccodes, by = "idenpa") %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

support_priv_pen17 <- lat_bar17_filtered %>%
  group_by(country) %>%
  count(P28N.E)


lat_bar07_filtered <- lat_bar07 %>%
  left_join(ccodes, by = "idenpa") %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

satisf_pensiones07 <- lat_bar07_filtered %>%
  group_by(country) %>%
  count(p57ni)  %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(satisf = p57ni) %>%
  pivot_wider(names_from = "satisf", values_from = "pct")


lab_bar02_filtered <- lat_bar02 %>%
  left_join(ccodes, by = "idenpa") %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))
  
trust_in_gov <- lab_bar02_filtered %>%
  group_by(country) %>%
  count(p34std) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(trust_in_gov = p34std)

# [1] A lot
# [2] Some
# [3] A little
# [4] No confidence
# [8] Don´t know
# [0] No answer

# Trust in political parties 
trust_in_parties02 <- lab_bar02_filtered %>%
  group_by(country) %>%
  count(p34stf) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(trust_in_parties = p34stf)



lab_bar03_filtered <- lat_bar03 %>%
  left_join(ccodes, by = "idenpa") %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

trust_in_parties03 <- lab_bar03_filtered %>%
  group_by(country) %>%
  count(p21std) %>%
  mutate(pct = round(100 * prop.table(n), 1)) %>%
  select(-n) %>%
  rename(trust_in_parties = p21std)


# Trust in congress
trust_in_congress <- readxl::read_excel("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/Confianza_en_el_Congreso.xlsx", skip = 7) %>%
  mutate(country = ifelse(is.na(country), "Latin America", country), 
         avg = as.numeric(avg)) %>%
  filter(!is.na(avg)) %>%
  left_join(codebook_countries_short) %>%
  filter(cnum %in% LA17) %>%
  mutate(across(c(`1995`:`2003`), ~ as.numeric(.))) %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay"))

trust_in_congress_wide <- trust_in_congress %>%
  pivot_wider(names_from = c("country"), values_from = c(`1995`:`2003`))

no_trust_congress <- lat_bar %>%
  filter(category == "Ninguna confianza") %>%
  select(-c(country, cnum, category)) %>%
  pivot_longer(!ccode, names_to = "year", values_to = "no_trust_congress") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year), !is.na(no_trust_congress))

pre_2000_no_trust <- no_trust_congress %>%
  filter(year < 2000) %>%
  group_by(ccode) %>%
  summarize(pre_2000_no_trust = mean(no_trust_congress, na.rm = T))

trust_in_govt <- readxl::read_excel("/Users/juan/Documents/Dissertation/DATA/Latinobarometro/Confianza_en_el_Gobierno.xlsx", skip = 7) %>%
  mutate(country = ifelse(is.na(country), "Latin America", country), 
         avg = as.numeric(avg)) %>%
  filter(!is.na(avg)) %>%
  left_join(codebook_countries_short) %>%
  mutate(across(c(`1995`:`2020\r\n`), ~ as.numeric(.))) %>%
  filter(country %in% c("Argentina", "Chile", "Uruguay")) %>%
  select(-avg) %>%
  pivot_longer(cols = c(`1995`:`2020\r\n`), names_to = "year", values_to = "pct")

trust_in_govt %>%
  mutate(year = as.numeric(year), 
         pct = 100 * pct) %>%
  filter(category == "Ninguna confianza") %>%
  ggplot(aes(year, pct, linetype = ccode)) +
  geom_line() +
  labs(y = "Percentage",  
       linetype = "Country", x = "Year") +
  scale_x_continuous(breaks=c(seq(1996, 2020, 2)))+
  theme_minimal()

ggsave("figure 4.2.png", path = "/Users/juan/Documents/Research/Welfare in Latin America - book/manuscript/final set of files to submit/figures", 
       width = 8, height = 5, device='png', dpi=320, bg = "white")


trust_in_govt %>%
  mutate(year = as.numeric(year), 
         pct = 100 * pct) %>%
  filter(category == "Ninguna confianza", 
         year < 2016) %>%
  ggplot(aes(year, pct, linetype = ccode)) +
  geom_line() +
  labs(y = "Percentage", title = "No trust in government, % of respondents by year", 
       linetype = "Country", x = "Year") +
  scale_x_continuous(breaks=c(seq(1996, 2014, 2)))+
  theme_minimal()


View(Latinobarometro_2018_Eng_Stata_v20190303)
