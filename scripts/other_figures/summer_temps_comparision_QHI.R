# RQ0 
# TEMPERATURE TRENDS BETWEEN SUMMERS AT QHI 
# I.E. HEATWAVES OCCURING IN 2024 
# 12/12/2025

# LOAD LIBRARIES----
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

# LOAD DATA----
QHI_climate_combined <- read_excel("dissertation_2026_bestington/datasets/climate_data/QHI_climate_combined.xlsx")

# FIGURE----
(QHI_climate_combined %>%
   filter(year %in% c(2023, 2024)) %>%
   mutate(year = factor(year)) %>%  # Convert year to factor for proper grouping
   ggplot(aes(x = date_time, y = temp, color = year, group = year)) +
   geom_line(linewidth = 0.5, alpha= 0.4) +  # Changed from 'size' to 'linewidth'
   geom_point(alpha = 0.3) +
   geom_hline(yintercept = 26, linetype = "dashed", color = "black", linewidth = 0.8) +
   scale_color_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3")) +  # Changed from scale_fill_manual
   labs(title = "Summer Temperature Trends at QHI (2023-2024)",
        x = "Time",
        y = "Surface Temperature (°C)",
        color = "Year") +  # Added legend title
   theme_classic() +
   theme(legend.position = "top")
)

# need to fix time on excel file 