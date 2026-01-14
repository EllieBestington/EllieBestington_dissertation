# ACTIVE LAYER DEPTH COMPARISON
# 14/01/2026

# LOAD LIBRARIES----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# LOAD DATA----
ald_data <- read_excel("datasets/QHI_roots_data/active_layer_depths_data.xlsx")

# DATA CLEANING----
ald_data$year <- as.factor(ald_data$year)

# CREATE FIGURE ----
(ald_plot<-ald_data %>%
    filter(year %in% c(2023, 2024)) %>%
    ggplot(aes(x = year, y = depth, fill = as.factor(year))) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
    labs(
      x = "Year",
      y = "Active Layer Depth (cm)",
      fill = "Year") +
    scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3"))+
    theme_classic() 
)

# WTF is active layer depth decreasing post heatwave- surely over the years active layer is getting bigger from permafrost thawing 
# some explanations 
# 1. Measurement error or inconsistencies in data collection methods between years or mixed up two years when putting data folder together 
# 2. Unusual weather patterns in 2024 that led to shallower active layers, such as increased precipitation or cooler temperatures (though doesn't correlate with temperature data)
# 3. Changes in vegetation cover or soil composition that could affect insulation and heat transfer to the permafrost layer- i.e. increase in vegetation cover, more mosses under warmer temperatures, create blanket and insulate?