### CLUSTER ANALYSIS
library(data.table)
library(readr)
library(dendextend)
library(tibble)
library(purrr)
library(stringr)
library(cluster)
library(dplyr)


# codebooks:
codebook_countries <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries.csv", drop = 1)
codebook_countries_short <- read_csv ("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_countries_short.csv")
codebook_years <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_years.csv", drop = 1)
codebook_quintiles <- fread("/Users/juan/Documents/Dissertation/DATA/codebooks/codebook_quintiles.csv")

# Constants: 
LA17 <- c("216","221","222","224","225","226","229","230",
          "235","239","233","240","241","242","244","258","259")
# Input data:
cut_2002 <- read_csv("/Users/juan/documents/Dissertation/write-ups/tables/Ch4/cut_2002_24.csv")
cut_2017 <- read_csv( "/Users/juan/documents/Dissertation/write-ups/tables/Ch4/cut_2017_24.csv")


##++++++=++=+++
##########################################
##          CLUSTER ANALYSIS          ####
##########################################

clust_02 <- cut_2002 %>%
  mutate(country = ifelse(str_detect(country, "Bolivia"), "Bolivia", ifelse(
    str_detect(country, "Ven"), "Venezuela", country))) %>%
  column_to_rownames(var = "country") %>%
  as.data.frame()


clust_17 <- cut_2017 %>%
  mutate(country = ifelse(str_detect(country, "Bolivia"), "Bolivia", ifelse(
    str_detect(country, "Ven"), "Venezuela", country))) %>%
  column_to_rownames(var = "country") %>%
  as.data.frame()

# cluster using everything 

dist_02_all <- dist(clust_02)
hc_02_all <- hclust(dist_02_all)

setwd("/Users/juan/Documents/Research/Welfare in Latin America - book/manuscript/final set of files to submit/figures")
png("figure 3.3.png", units="in", width=12, height=8, res=320)
plot(hc_02_all, main = "Cluster Dendrogram 2002", 
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, cex = 1.8)
dev.off()

dist_17_all <- dist(clust_17)
hc_17_all <- hclust(dist_17_all)

png("figure 3.4.png", units="in", width=12, height=8, res=320)
plot(hc_17_all, main = "Cluster Dendrogram 2017", 
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, cex = 1.8)
dev.off()


dendrog_1 <- plot(hc_02_all, main = "Cluster Dendrogram 2002")
dendrog_2 <- plot(hc_17_all, main = "Cluster Dendrogram 2017")
 ggsave(plot = dendrog_1, width = 4, height = 4, dpi = 300, filename = "/Users/juan/Documents/Dissertation/write-ups/book manuscript/charts/Ch4/dendrogram_02.jpeg")
 ggsave(plot = dendrog_2, width = 4, height = 4, dpi = 300, filename = "/Users/juan/Documents/Dissertation/write-ups/book manuscript/charts/Ch4/dendrogram_17.jpeg")

# color-tree dendrograms:
dend_all_02 <- as.dendrogram(hc_02_all)
color_dend_02 <- color_branches(dend_all_02, h = 8)
plot(color_dend_02, main = "Cluster Dendrogram 2002")

dend_all_17 <- as.dendrogram(hc_17_all)
color_dend_17 <- color_branches(dend_all_17, h = 10)
plot(color_dend_17, main = "Cluster Dendrogram 2017")

# clusters using inclusion, generosity and equity: 
clust_gen_eq_02 <- clust_02 %>%
  select(Inclusion, Generosity, Equity)
dist_gen_eq_02 <- dist(clust_gen_eq_02)
hc_gen_eq_02 <- hclust(dist_gen_eq_02, "complete")
plot(hc_gen_eq_02, main = "Cluster Dendrogram 2002 - all but decommodification")

dend_gen_eq_02 <- as.dendrogram(hc_gen_eq_02)
color_dend_02 <- color_branches(dend_gen_eq_02, h = 6)
plot(color_dend_02, main = "Cluster Dendrogram 2002 - all but decommodification")

clust_gen_eq_17 <- clust_17 %>%
  select(Inclusion, Generosity, Equity)
dist_gen_eq_17 <- dist(clust_gen_eq_17)
hc_gen_eq_17 <- hclust(dist_gen_eq_17, "complete")
plot(hc_gen_eq_17, main = "Cluster Dendrogram 2017 - all but decommodification")

dend_gen_eq_17 <- as.dendrogram(hc_gen_eq_17)
color_dend_17 <- color_branches(dend_gen_eq_17, h = 8)
plot(color_dend_17, main = "Cluster Dendrogram 2017 - all but decommodification")

# code below is not working
dendrog_1 <- plot(hc_gen_eq_02, main = "Cluster Dendrogram 2002")
dendrog_2 <- plot(hc_gen_eq_17, main = "Cluster Dendrogram 2017")
# ggsave(plot = dendrog_1, width = 4, height = 4, dpi = 300, filename = "/Users/juan/Documents/Dissertation/write-ups/Chapter 3 - quanti scores and more/charts/dendrogram_02.jpeg")
# ggsave(plot = dendrog_2, width = 4, height = 4, dpi = 300, filename = "/Users/juan/Documents/Dissertation/write-ups/Chapter 3 - quanti scores and more/charts/dendrogram_17.jpeg")


# Analysis of clusters 2002 and 2017: 

cut_2002_with_clusters <- cut_2002 %>%
  left_join(codebook_countries_short) %>%
  mutate(cluster = case_when(ccode %in% c("CRI", "URY", "BRA", "ARG") ~ "1",
                             ccode %in% c("ECU", "MEX", "VEN", "PER", "BOL", "COL","PAN") ~ "3",
                             ccode == "CHL" ~ "2", 
                             ccode %in% c("NIC", "HND", "GTM", "SLV", "PRY") ~ "4"))
clusters_2002_avgs <- cut_2002_with_clusters %>%
  group_by(cluster) %>%
  summarize(Inclusion = mean(Inclusion),
            Generosity = mean(Generosity),
            Equity = mean(Equity), 
            Decommodification = mean(Decommodification)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("variable") 

write_csv(clusters_2002_avgs, "/Users/juan/Documents/Dissertation/write-ups/tables/Ch4/clusters_2002_avgs_2024.csv")

cut_2017_with_clusters <- cut_2017 %>%
  left_join(codebook_countries_short) %>%
  mutate(cluster = case_when(ccode %in% c("CRI", "URY", "BRA", "ARG") ~ "1",
                             ccode %in% c("NIC", "ECU", "MEX", "VEN", "PER", "BOL", "COL","PAN") ~ "3",
                             ccode == "CHL" ~ "2", 
                             ccode %in% c("HND", "GTM", "SLV", "PRY") ~ "4"))
clusters_2017_avgs <- cut_2017_with_clusters %>%
  group_by(cluster) %>%
  summarize(Inclusion = mean(Inclusion),
            Generosity = mean(Generosity),
            Equity = mean(Equity), 
            Decommodification = mean(Decommodification)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(V5 = V1, V6 = V2, V7 = V3, V8 = V4)

write_csv(clusters_2017_avgs, "/Users/juan/Documents/Dissertation/write-ups/tables/Ch4/clusters_2017_avgs_2024.csv")

# Now I'll compare clusters using real values of indicators
all_clusters <- cut_2002_with_clusters %>%
  select(ccode, cluster_2002 = cluster) %>%
  left_join(cut_2017_with_clusters) %>%
  select(ccode, cluster_2002, cluster_2017 = cluster)

consolidated_score_filled <- read_csv("/Users/juan/Documents/Dissertation/DATA/my data sets/consolidated_score_filled_no_family.csv") 

master_filled_with_clusters <- consolidated_score_filled %>%
  left_join(all_clusters)

cluster_comp_table <- master_filled_with_clusters %>%
  filter(year %in% c(2002, 2017)) %>%
  select(year, cluster_2002, cluster_2017, cct_incl, cct_vis_med_income_wageearner,
         elderly_covered_any_pension, ncp_vis_cont,  pen_rate_occup_all,
         pen_gap, pen_repl_rate_median_income, priv_pens_pct_gdp,
         govt_hexp_pctGDP_WB, gov_hexp_ppp_WB_cnstnt, 
         UHC_index,  oop_health_exp_pc_ppp, priv_health_exp_pct,
         educ_xpnd_pctGDP, primschoolenrol, upsecschoolenrol, 
         pct_priv_school, 
         mat_leave_weeks, mat_rep_rate, 
         preprim_enr_rate) %>%
  mutate(cluster = ifelse(year == 2002, cluster_2002,
                          ifelse(year ==2017, cluster_2017, NA))) %>%
  select(-c(cluster_2002, cluster_2017)) %>%
  group_by(year, cluster) %>%
  summarize(across(c(cct_incl, cct_vis_med_income_wageearner,
                     elderly_covered_any_pension, ncp_vis_cont,  pen_rate_occup_all,
                     pen_gap, pen_repl_rate_median_income, priv_pens_pct_gdp,
                     govt_hexp_pctGDP_WB, gov_hexp_ppp_WB_cnstnt, 
                     UHC_index,  oop_health_exp_pc_ppp, priv_health_exp_pct,
                     educ_xpnd_pctGDP, primschoolenrol, upsecschoolenrol, 
                     pct_priv_school, 
                     mat_leave_weeks, mat_rep_rate, 
                     preprim_enr_rate), ~ mean(.))) %>%
  mutate(across(c(cct_incl, cct_vis_med_income_wageearner,
                  elderly_covered_any_pension, pen_rate_occup_all,
                  pen_gap,  UHC_index, primschoolenrol, upsecschoolenrol, 
                  mat_rep_rate, 
                  preprim_enr_rate), ~ (.*100))) %>%
  mutate(across(c(cct_incl, cct_vis_med_income_wageearner,
                  elderly_covered_any_pension, ncp_vis_cont,  pen_rate_occup_all,
                  pen_gap, pen_repl_rate_median_income, priv_pens_pct_gdp,
                  govt_hexp_pctGDP_WB, gov_hexp_ppp_WB_cnstnt, 
                  UHC_index,  oop_health_exp_pc_ppp, priv_health_exp_pct,
                  educ_xpnd_pctGDP, primschoolenrol, upsecschoolenrol, 
                  pct_priv_school, 
                  mat_leave_weeks, mat_rep_rate, 
                  preprim_enr_rate), ~ round(., digits = 1))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("variable")

write_csv(cluster_comp_table, "/Users/juan/Documents/Dissertation/write-ups/tables/Ch4/cluster_comp_table_2024.csv")

cluster_scores_table <- clusters_2002_avgs %>%
  #rename(V1 = `1`, V2 = `2`, V3 = `3`, V4 = `4`) #%>%
  cbind(clusters_2017_avgs[2:5]) %>%
  #rename(V6 = V1.1, V7 = V2.1, V8 = V3.1, V9 = V4.1) %>%
  mutate_at(c(2:9), as.numeric ) # %>%
 # mutate_at(c(2:9), round(., digits = 1))

full_comp_table <- cluster_scores_table %>%
  rbind(cluster_comp_table)

write_csv(full_comp_table, "/Users/juan/Documents/Dissertation/write-ups/tables/Ch4/full_cluster_comp_table_2024.csv")

#####
##    I DON'T THINK I NEED ANYTHING BELOW THIS
#####


##  elbow_plot <- function(clust_table, title){
##    ## Assessing the number of clusters that is most efficient
##    tot_withinss <- map_dbl(1:10,  function(k){
##      model <- kmeans(x = clust_table, centers = k)
##      model$tot.withinss
##    })
##    
##    # Generate a data frame containing both k and tot_withinss
##    elbow_df <- data.frame(
##      k = 1:10,
##      tot_withinss = tot_withinss
##    )
##    
##    # Plot the elbow plot
##    ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
##      geom_line() +
##      scale_x_continuous(breaks = 1:10)+
##      labs(title = title)
##  }
##  
##  elbow_plot(clust_02, "Elbow plot for cluster analysis 2002, all scores")
##  elbow_plot(clust_17, "Elbow plot for cluster analysis 2017, all scores")
##  
##  silhouette_plot <- function(clust_table, title){
##    ## Now the same with sillhouette method: 
##    
##    sil_width <- map_dbl(2:10,  function(k){
##      model <- pam(x = clust_table, k = k)
##      model$silinfo$avg.width
##    })
##    
##    # Generate a data frame containing both k and sil_width
##    sil_df <- data.frame(
##      k = 2:10,
##      sil_width = sil_width
##    )
##    
##    # Plot the relationship between k and sil_width
##    ggplot(sil_df, aes(x = k , y = sil_width)) +
##      geom_line() +
##      scale_x_continuous(breaks = 2:10)+
##      labs(title = title)
##  }
##  
##  silhouette_plot(clust_02, "sillhouette plot for cluster analysis 2002, all scores")
##  silhouette_plot(clust_17, "sillhouette plot for cluster analysis 2017, all scores")
##  
##  
##  
##  model_2002 <- kmeans(clust_02, centers = 4)
##  cl_2002 <- model_2002$cluster
##  clust_02_complete <- mutate(clust_02, cluster = cl_2002)
##  
##  model_2017 <- kmeans(clust_17, centers = 3)
##  cl_2017 <- model_2017$cluster
##  clust_17_complete <- mutate(clust_17, cluster = cl_2017)
##  
##  
##  
##  # cluster using only universalism score for 2002
##  clust_univ_02 <- clust_02 %>%
##    select(Decommodification)
##  dist_02_univ <- dist(clust_univ_02)
##  hc_02_univ <- hclust(dist_02_univ, "complete")
##  plot(hc_02_univ, main = "Cluster 2002 using only decommodification")
##  
##  # cluster using only universalism score for 2017
##  clust_univ_17 <- clust_17 %>%
##    select(Decommodification)
##  dist_17_univ <- dist(clust_univ_17)
##  hc_17_univ <- hclust(dist_17_univ, "complete")
##  plot(hc_17_univ, main = "Cluster 2017 using only decommodification")
##  
##  
##  elbow_plot <- function(clust_table, title){
##    ## Assessing the number of clusters that is most efficient
##    tot_withinss <- map_dbl(1:10,  function(k){
##      model <- kmeans(x = clust_table, centers = k)
##      model$tot.withinss
##    })
##    
##    # Generate a data frame containing both k and tot_withinss
##    elbow_df <- data.frame(
##      k = 1:10,
##      tot_withinss = tot_withinss
##    )
##    
##    # Plot the elbow plot
##    ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
##      geom_line() +
##      scale_x_continuous(breaks = 1:10)+
##      labs(title = title)
##  }
##  
##  elbow_plot(clust_univ_02, "Elbow plot for cluster analysis 2002, all scores")
##  elbow_plot(clust_univ_17, "Elbow plot for cluster analysis 2017, all scores")
##  
##  silhouette_plot <- function(clust_table, title){
##    ## Now the same with sillhouette method: 
##    
##    sil_width <- map_dbl(2:10,  function(k){
##      model <- pam(x = clust_table, k = k)
##      model$silinfo$avg.width
##    })
##    
##    # Generate a data frame containing both k and sil_width
##    sil_df <- data.frame(
##      k = 2:10,
##      sil_width = sil_width
##    )
##    
##    # Plot the relationship between k and sil_width
##    ggplot(sil_df, aes(x = k , y = sil_width)) +
##      geom_line() +
##      scale_x_continuous(breaks = 2:10)+
##      labs(title = title)
##  }
##  
##  silhouette_plot(clust_univ_02, "sillhouette plot for cluster analysis 2002, all scores")
##  silhouette_plot(clust_univ_17, "sillhouette plot for cluster analysis 2017, all scores")
##  
##  
##  
##  dend_gen_eq_17 <- as.dendrogram(hc_gen_eq_17)
##  color_dend_17 <- color_branches(dend_gen_eq_17, h = 7)
##  plot(color_dend_17)
##  
##  country_cluster02 <- cutree(hc_gen_eq_02, h = 1)
##  cut_02_plus <- cut_2002 %>%
##    mutate(cluster = country_cluster02)
##  cluster_means_02 <- cut_02_plus %>%
##    group_by(cluster) %>%
##    summarize(generosity = mean(generosity_score), 
##              equity = mean(equity_score))
##  country_cluster17 <- cutree(hc_gen_eq_17, h = 1)
##  cut_17_plus <- cut_2017 %>%
##    mutate(cluster = country_cluster17)
##  cluster_means_17 <- cut_17_plus %>%
##    group_by(cluster) %>%
##    summarize(generosity = mean(generosity_score), 
##              equity = mean(equity_score))
##  avg_cut_2017 <- cut_2017 %>%
##    summarize(across(.cols = everything(), fns = mean(.)))
##  
##  # cluster using everything BUT universalism 
##  clust_no_univ_02 <- clust_02 %>%
##    select(-decom_score)
##  dist_02_no_univ <- dist(clust_no_univ_02)
##  hc_02_no_univ <- hclust(dist_02_no_univ)
##  plot(hc_02_no_univ, main = "Cluster Dendrogram 2002 - all but decomm")
##  
##  clust_no_univ_17 <- clust_17 %>%
##    select(-decom_score)
##  dist_17_no_univ <- dist(clust_no_univ_17)
##  hc_17_no_univ <- hclust(dist_17_no_univ)
##  plot(hc_17_no_univ, main = "Cluster Dendrogram 2017 - all but decomm")
##  
##  ##+++=++=+++=++++=+
##  