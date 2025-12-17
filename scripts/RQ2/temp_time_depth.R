# RQ2
# TEMPERATURE, TIME BY DEPTH 
# 09/12/2025


# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

# LOAD DATA----
clean_control_exp_data <- read_excel("dissertation_2026_bestington/datasets/lab_experiment/clean_control_exp_data.xlsx")


# FIGURE AS FACET WRAP ----
(rq2_fac5et_temp_time<-ggplot(clean_control_exp_data, aes(x = hour, y = temp, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Time (hours)",
    y = "Temperature (°C)",
    color = "Treatment"
  ) +
  facet_wrap(~ depth, ncol = 1)+
   scale_color_manual(labels = c("con"="Control", "exp"= "Experimental") ,values = c("con" = "deepskyblue2", "exp" = "brown1"))+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
)
# worry with this figure is that too squashed so hard to see differences between depths of control and experimental 
# better way to represent? 


# FIGURE AS DIFFERENCE IN TEMPERATURE----
(change_temp_time<-clean_control_exp_data %>%
   filter(!is.na(change_in_temp), !is.na(hour)) %>%
   ggplot(aes(x = hour, y = change_in_temp, color = depth, group = depth)) +
    geom_line() +
    geom_point(size = 2) +
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
# difference greatest in first 24 hours- correlates with rapid thawing of experimental but retention of cold/frozenness in control 
# due to polystrene that is insulating control 
# then settles into more cyclic pattern after initial thawing period

# FIGURE JUST SURFACE TEMP ----
(surface_temp_time<-clean_control_exp_data %>%
   filter(depth == 0) %>%
   ggplot(aes(x = hour, y = temp, color = type, group = type)) +
   geom_line() +
   geom_point(size = 2) +
   labs(
     x = "Time (hours)",
     y = "Surface Temperature (°C)",
     color = "Treatment"
   ) +
   scale_color_manual(labels = c("con"="Control", "exp"= "Experimental") ,values = c("con" = "deepskyblue2", "exp" = "brown1"))+
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
     axis.title.x = element_text(size = 14),
     axis.title.y = element_text(size = 14),
     legend.title = element_text(size = 12),
     legend.text = element_text(size = 10)
   )
)
# surface temps show clear divergence between control and experimental treatments with experimental cores heating up much faster and to higher temps overall
# seems to establish a cyclic pattern after initial rapid increase in temp over first 10 hours or so (because of rapid onset of heat/thawing of cores)
# control cores heat up much more slowly and dont reach as high temps overall