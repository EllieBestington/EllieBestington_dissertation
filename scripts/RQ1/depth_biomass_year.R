# RQ1
# DEPTH BIOMASS BY YEAR 
# 10/12/2025

# LOAD LIBRARIES----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")


# FIGURE----
(depth_biomass_year<-QHI_combined_data %>%
   filter(year %in% c(2023, 2024)) %>%
   filter(!is.na(max_depth_increment), !is.na(root_dry_biomass_total)) %>%
   filter(max_depth_increment != "N/A") %>%
   mutate(max_depth_increment = factor(max_depth_increment, 
                                       levels = c("5", "10", "15", "20", "25", "30"))) %>%
   ggplot(aes(x = max_depth_increment, y = root_dry_biomass_total, fill = as.factor(year))) +
   geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
   labs(
     x = "Depth (cm)",
     y = "Dry Root Biomass (g)",
     fill = "Year") +
   scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3"))+
   theme_classic() 
)

# FIGURE WITH LINE OF BEST FIT ----
(depth_biomass_year_lobf <- QHI_combined_data %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(!is.na(max_depth_increment), !is.na(root_dry_biomass_total)) %>%
  filter(max_depth_increment != "N/A") %>%
  mutate(max_depth_increment = as.numeric(max_depth_increment)) %>%
  ggplot(aes(x = max_depth_increment, y = root_dry_biomass_total, color = as.factor(year))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depth (cm)",
    y = "Dry Root Biomass (g)",
    color = "Year") +
  scale_color_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3")) +
  theme_classic()
)
# 2023 not really any change, slight increase with decreasing depth
# but 2024 more of an increase with decreasing depth- potential for graminoids to be taking advantage of deeper rooting as more space available!
# would need to compare to community biomass year graph to see if this is reflected there too
# if results not significant in analysis, can just say exposure of heatwave not long enough to cause significant change, but over time with more heatwaves could be the case 
# futher research analysing multiple years, tracking heatwaves would be required 
# but then need to be careful if its due to heatwaves or just general increases in temp due too CC, hence doing short term year to year analysis better for understanding short term heatwaves impact? So this stuyd is important

