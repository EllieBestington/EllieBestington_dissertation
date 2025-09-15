# MOCK FIGURES
# PILOT DEPTH VS TEMPERATURE 
# LAST UPDATED: 15/09/2025
# ELLIE BESTINGTON 

# LOAD LIBRARIES
library(tidyverse)
library(readxl)
library(ggplot2)

# LOAD DATA----
combined_data_pilot <- read_excel("datasets/accompanying_experiment/pilot/clean_data/combined_data_pilot.xlsx")

# CREATE FIGURE----
# Without data points
(ggplot(combined_data_pilot, aes(x = temperature, y = depth, color = type)) +
   geom_smooth(method = "lm", se = FALSE) +
   labs(x = "Temperature (°C)",
        y = "Depth (cm)") +
   scale_color_manual(values = c("Control" = "navy", "Experimental" = "maroon")) +
   theme_classic() +
   theme(legend.title = element_blank())
 
)

# With data points
(ggplot(combined_data_pilot, aes(x = temperature, y = depth, color = type)) +
    geom_point(alpha=0.25) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Temperature (°C)",
         y = "Depth (cm)") +
    scale_color_manual(values = c("Control" = "navy", "Experimental" = "maroon")) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
)