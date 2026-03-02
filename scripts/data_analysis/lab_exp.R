# RQ2: LAB EXPERIMENT DATA ANALYSIS 
# Is there a significant difference between changes in surface temp and changes in 30 cm depth across time?

# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)
library(mgcv) 


# LOAD DATA----
clean_control_exp_data <- read_excel("datasets/lab_experiment/clean_control_exp_data.xlsx")


# VISUALISATION ----
(change_temp_time <- clean_control_exp_data %>%
   filter(!is.na(change_in_temp), !is.na(hour)) %>%
   filter(hour >= 24) %>% # filter to remove first day as initial thawing period where temp changes are due limited thawing from control
   ggplot(aes(x = hour, y = change_in_temp, color = depth, group = depth)) +
   geom_line() +
   geom_point(size = 2) +
   scale_color_gradient(high = "#132B43", low = "#56B1F7") +  # dark to light
   labs(
     x = "Time (hours)",
     y = "Difference in Temperature (°C)",
     color = "Depth (cm)"
   ) +
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
     axis.title.x = element_text(size = 14),
     axis.title.y = element_text(size = 14),
     legend.title = element_text(size = 12),
     legend.text = element_text(size = 10)
   )
)

# plot smooth line to plot trends for each depth
(change_temp_time_smooth <- clean_control_exp_data %>%
   filter(!is.na(change_in_temp), !is.na(hour)) %>%
   filter(hour >= 24) %>% # filter to remove first day as initial thawing period where temp changes are due limited thawing from control
   ggplot(aes(x = hour, y = change_in_temp, color = depth, group = depth)) +
   geom_smooth(method = "loess", se = FALSE) +
   scale_color_gradient(high = "#132B43", low = "#56B1F7") +  # dark to light
   labs(
     x = "Time (hours)",
     y = "Difference in Temperature (°C)",
     color = "Depth (cm)"
   ) +
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
     axis.title.x = element_text(size = 14),
     axis.title.y = element_text(size = 14),
     legend.title = element_text(size = 12),
     legend.text = element_text(size = 10)
   )
)

# ANALYSIS----
# cannot use a linear model as our pattern isn't linear!
# instead we should use: Generalized Additive Mixed Model (GAMM) to deal with this 

#  stacked diurnal time series repeated over 4 days 

# create cyclic hour 
clean_control_exp_data$hour_mod <- clean_control_exp_data$hour %% 24

# create day variable
clean_control_exp_data$day <- floor(clean_control_exp_data$hour / 24)

# fit GAM

mod2 <- gam(log_change_in_temp ~factor(depth) + s(hour_mod, by = factor(depth), bs = "cc", k = 5) +
    s(day, bs = "re"),
  data = clean_control_exp_data,
  method = "REML"
)
summary(mod2)
gam.check(mod2)
