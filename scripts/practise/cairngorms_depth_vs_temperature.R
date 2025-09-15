# MOCK FIGURE CREATION
# CAIRNGORMS DEPTH VS TEMPERATURE VS TYPE
# 21/08/2025
# BY ELLIE BESTINGTON

# LOAD LIBRARIES----
library(tidyverse)
library(readxl)
library(ggplot2)

# LOAD DATA----
combined_data <- read_excel("C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/accom_exp/cairngorms/clean_data/combined_data.xlsx")

# CREATE FIGURE----
# Without data points
(ggplot(combined_data, aes(x = temperature, y = depth, color = type)) +
   geom_smooth(method = "lm", se = FALSE) +
   labs(x = "Temperature (°C)",
        y = "Depth (cm)") +
   scale_color_manual(values = c("Control" = "navy", "Experimental" = "maroon")) +
   theme_classic() +
   theme(legend.title = element_blank())
   
)

# With data points
(ggplot(combined_data, aes(x = temperature, y = depth, color = type)) +
    geom_point(alpha=0.25) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Temperature (°C)",
         y = "Depth (cm)") +
    scale_color_manual(values = c("Control" = "navy", "Experimental" = "maroon")) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
)

# ANALYSIS----
