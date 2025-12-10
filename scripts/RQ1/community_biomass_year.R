# RQ1
# community comparison between years
# 09/12/2025

# LOAD LIBRARIES----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")


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
