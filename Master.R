# Political Economy of Welfare in Latin America: Universalism Deferred
# Master script

# Join all data: 
source('/Users/juan/Documents/Dissertation/DATA/codes/joining all data7.R', echo = T)
# Keep only master_data3
rm(list=ls()[! ls() %in% c("master_data3")])

# Descriptive statistics:
# -> /Users/juan/Documents/Dissertation/DATA/codes/descriptive stats_2025.R

# Constructing the scores: 
source('/Users/juan/Documents/Dissertation/DATA/codes/score construction_without family policy.R', echo = T)
# Keep only objects I need
rm(list=ls()[! ls() %in% c("master_data3", "scores3", "cut_2002", "cut_2017", "progress_all_w_avg")])

# Cluster analysis: 
source('/Users/juan/Documents/Dissertation/DATA/codes/Cluster analysis2.R', echo = T)

# Constructing explanatory data set:
source('/Users/juan/Documents/Dissertation/DATA/codes/constructing explanatory data set_2025.R', echo = T)

# Explanatory analysis: 
source('/Users/juan/Documents/Dissertation/DATA/codes/Explanatory analysis_2024_manuscript.R', echo = T)
