# MOCK FIGURE CREATION
# CAIRNGORMS
# 21/08/2025
# BY ELLIE BESTINGTON

# LOAD LIBRARIES----
library(tidyverse)
library(readxl)
library(ggplot2)

# LOAD DATA----
combined_data <- read_excel("C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/accom_exp/cairngorms/clean_data/combined_data.xlsx")

# CREATE FIGURE----

(ggplot(combined_data, aes(x = temperature, y = depth, color = type)) +
   geom_point(size = 2, alpha= 0.2) +
   geom_smooth(method = "lm", se = FALSE) +
   labs(x = "Temperature (°C)",
        y = "Depth (cm)") +
   scale_color_manual(values = c("Control" = "navy", "Experimental" = "maroon")) +
   theme_minimal() +
   theme(legend.title = element_blank())+
   scale_x_continuous(sec.axis = dup_axis())
   
)
