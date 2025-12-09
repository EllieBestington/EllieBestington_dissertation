# RQ2
# SOIL MOISTURE OVER TIME
# 09/12/2025 


# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

# LOAD DATA----
clean_control_exp_data <- read_excel("datasets/lab_experiment/clean_control_exp_data.xlsx")

# CLEAN DATA---
clean_control_exp_data <- clean_control_exp_data %>%
  mutate(hour = as.numeric(as.character(hour)))

# FIGURE----
(rq2_soil_moisture <- clean_control_exp_data %>%
   filter(!is.na(soil_moisture), !is.na(hour)) %>%
   ggplot(aes(x = hour, y = soil_moisture, color = type, group = type)) +
   geom_line()+
   geom_point()+
   labs(
     x = "Hour",
     y = "Surface Soil Moisture (%)",
     color = "Treatment"
   ) +
   scale_color_manual(labels = c("con"="Control", "exp"= "Experimental") ,values = c("con" = "deepskyblue2", "exp" = "brown1"))+
   theme(legend.text = element_text(size = 12)) +
   theme_classic()
)
