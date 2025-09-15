# MOCK FIGURE CREATION 
# PILOT TEMP OVER TIME BY DEPTH?
# 15/09/2025
# Author: Ellie Bestington

# Libraries----
library(tidyverse)
library(ggplot2)

# FIGURE----

# With depth split up by facet wrap
library(ggplot2)

(ggplot(combined_data_pilot) +
    aes(x = date_time, y = temperature, colour = type, group = type) +
    geom_line() +
    theme_minimal() + 
    facet_wrap(~depth)
)

# Combined 
(ggplot(combined_data_pilot) +
    aes(x = date_time, y = temperature, fill = depth, colour = type, group = type) +
    geom_col() +
    scale_fill_gradient() +
    scale_color_hue(direction = 1) +
    theme_minimal() 
)
