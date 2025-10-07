# MOCK FIGURE CREATION 
# CAIRNGORMS TEMP OVER TIME BY DEPTH?
# 27/08/2025
# Author: Ellie Bestington

# Libraries----
library(tidyverse)
library(ggplot2)

# FIGURE----

# With depth split up by facet wrap
library(ggplot2)

(ggplot(combined_data) +
 aes(x = date_time, y = temperature, colour = type, group = type) +
 geom_line() +
 theme_minimal() + 
 facet_wrap(~depth)
)

# Combined 
(ggplot(combined_data) +
    aes(x = date_time, y = temperature, fill = depth, colour = type, group = type) +
    geom_col() +
    labs(x = "Time", y = "Temperature (°C)" )+
    scale_fill_gradient() +
    scale_color_hue(direction = 1) +
    theme_classic()+
    theme(axis.text.x=element_blank())
)

