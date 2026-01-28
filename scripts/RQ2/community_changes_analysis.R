# RQ1
# COMMUNITY CHANGES DATA ANALYSIS
# 17/12/2025


# LOAD LIBRARIES----
library(tidyverse)
library(vegan)
library(ggplot2)
library(lme4)
library(dplyr)


# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")

# CLEAN AND TIDY DATA----
# remove nas from rootmass bulk density 
QHI_combined_cleaned <- QHI_combined_data %>%
  filter(!is.na(rootmass_bulkdensity))  %>%
  filter(rootmass_bulkdensity != "N/A")

# make year a factor
QHI_combined_cleaned$year <- as.factor(as.character(QHI_combined_cleaned$year))   

# make root biomass bulk density numeric
QHI_combined_cleaned$rootmass_bulkdensity <- as.numeric(QHI_combined_cleaned$rootmass_bulkdensity)

# HISTOGRAM----
# visualise the frequency of distribution of rootmass bulk density/response variable 
(root_histogram<-ggplot(QHI_combined_cleaned, aes(x = rootmass_bulkdensity, fill = factor(year))) +
   geom_histogram(binwidth = 0.5, color = "black", alpha = 0.6) +
   labs(
     title = "Histogram of Root Mass Bulk Density",
     x = "Root Mass Bulk Density",
     y = "Frequency",
     fill = "Year"
   ) +
   theme_minimal())
 
# looks right skewed 

# VISUALISE MEANS WITH A BOXPLOT----
(root_boxplot<-ggplot(QHI_combined_cleaned, aes(x = year, y = rootmass_bulkdensity, fill = year)) +
   geom_boxplot() +
   labs(
     title = "Boxplot of Root Mass Bulk Density by Year",
     x = "Year",
     y = "Root Mass Bulk Density"
   ) +
   theme_minimal())