# RQ1
# community comparison between years
# 09/12/2025

# LOAD LIBRARIES----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(lme4)

# LOAD DATA----
QHI_combined_data <- read_excel("dissertation_2026_bestington/datasets/QHI_roots_data/QHI_combined_data.xlsx")

# FIGURE -----
(community_biomass_year<-QHI_combined_data %>%
  filter(year %in% c(2023, 2024)) %>%
  ggplot(aes(x = community, y = root_dry_biomass_total, fill = as.factor(year))) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(
       x = "Community",
       y = "Dry Root Biomass (g)",
       fill = "Year") +
  scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3"))+
  theme_classic() 
)

# ANALYSIS----
# is there a significant difference between each year by functional group? 

# histogram
hist(QHI_combined_data$root_dry_biomass_total) # looks right skewed/non-normal
QHI_combined_data$root_dry_biomass_total <- scale(QHI_combined_data$root_dry_biomass_total, center = TRUE, scale = TRUE) # scaling and standardising data
hist(QHI_combined_data$root_dry_biomass_total) # still right skewed

# test assumptions
shapiro.test(QHI_combined_data$root_dry_biomass_total) # p-value < 0.05, data not normal


# basic linear model - root biomass & year
basic_lm<- lm(root_dry_biomass_total ~ year, data = QHI_combined_data)
summary(basic_lm) 
plot(basic_lm) # not normal, heteroscedasticity




# FIGURE 2 IDEA----
(year_biomass_community<-QHI_combined_data %>%
    filter(year %in% c(2023, 2024)) %>%
    ggplot(aes(x = as.factor(year), y = root_dry_biomass_total, fill = community)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
    labs(
      x = "Year",
      y = "Dry Root Biomass (g)",
      fill = "Community") +
    scale_fill_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
    theme_classic() 
)
