# RQ1
# community comparison between years
# 09/12/2025

# LOAD LIBRARIES----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(lme4)

setwd("C:/Users/ellie/OneDrive/Documents/dissertation_2026_bestington")

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")


# CLEAN DATA----
QHI_combined_data <- QHI_combined_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
  # remove row 32
  filter(row_number() != 32)
  
# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation 

# FILTER FOR JUST P3----
QHI_combined_data_P3 <- QHI_combined_data %>%
  filter(core_ID == "P3")

# FIGURE P3s only-----
(community_biomass_year <- QHI_combined_data_P3 %>%
  filter(year %in% c(2023, 2024)) %>%
  ggplot(aes(x = community, y = rootmass_bulkdensity, fill = as.factor(year))) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(
    x = "Community",
    y = "Root Biomass Density (g cm⁻³)",
    fill = "Year") +
  scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme_classic()
)


# FIGURE ORIGINAL----
(community_biomass_year_all <- QHI_combined_data %>%
   filter(year %in% c(2023, 2024)) %>%
   ggplot(aes(x = community, y = rootmass_bulkdensity, fill = as.factor(year))) +
   geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
   labs(
     x = "Community",
     y = "Root Biomass Density (g cm⁻³)",
     fill = "Year") +
   scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3")) +
   scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
   theme_classic()
)

# FIGURE 2 IDEA----
(year_biomass_community<-QHI_combined_data_P3 %>%
    filter(year %in% c(2023, 2024)) %>%
    ggplot(aes(x = as.factor(year), y = rootmass_bulkdensity, fill = community)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
    labs(
      x = "Year",
      y = "Root Biomass Density (g cm⁻³)",
      fill = "Community") +
    scale_fill_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    theme_classic() 
)
