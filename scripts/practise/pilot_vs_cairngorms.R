# MOCK FIGURE 
# COMPARE PILOT VS CAIRNGORMS 
# 15/09/2025
# ELLIE BESTINGTON 

# LOAD PACKAGES----
library(tidyverse)
library(ggplot2)


# LOAD DATA----
combine_experimental <- read_excel("datasets/accompanying_experiment/combine_experimental.xlsx")


# FIGURE 1 ----

library(ggplot2)

(ggplot(combine_experimental) +
 aes(x = temperature_average, y = depth, colour = location) +
 geom_point() +
 geom_smooth(method = "lm", se = FALSE) +
 scale_color_manual(values = c(Cairngorms = "#52A8F7", Pilot = "#F8766D")) +
 labs(x = "Temperature (C)", 
 y = "Depth (cm)", color = "Experiment") +
 theme_classic()
  
)

# Without data points
(ggplot(combine_experimental, aes(x = temperature_average, y = depth, color = location)) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Temperature (°C)",
         y = "Depth (cm)") +
    scale_color_manual(values = c("Cairngorms" = "#52A8F7", "Pilot" = "#F8766D")) +
    theme_classic() +
    theme(legend.title = element_blank())
  
)

