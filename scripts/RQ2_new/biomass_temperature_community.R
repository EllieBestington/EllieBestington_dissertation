# RQ3
# DEPTH AND ROOT BIOMASS FOR 2024 
# 17/12/2025


# LOAD LIBRARIES----
library(tidyverse)
library(readxl)
library(ggplot2)


# LOAD DATA----
QHI_combined_data <- read_excel("dissertation_2026_bestington/datasets/QHI_roots_data/QHI_combined_data.xlsx")
clean_control_exp_data <- read_excel("datasets/lab_experiment/clean_control_exp_data.xlsx")
# FILTER DATA FOR 2024----
QHI_2024 <- QHI_combined_data %>%
  filter(year == 2024)

# PLOT DEPTH VS ROOT BIOMASS FOR 2024 by community----
(QHI_combined_data %>%
  filter(year %in% c(2024)) %>%
  filter(!is.na(max_depth_increment), !is.na(root_dry_biomass_total)) %>%
  filter(max_depth_increment != "N/A") %>%
  mutate(max_depth_increment_numeric = as.numeric(as.character(max_depth_increment))) %>%
  ggplot(aes(x = max_depth_increment_numeric, y = root_dry_biomass_total, colour = community)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")+
  labs(title = "Root Biomass vs Depth (2024)",
       x = "Depth (cm)",
       y = "Root Biomass (g/m²)",
       colour = "Community") +
  theme_classic() +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30))
)
# mix community (possible with the graminoids) driving the relationship we are seeing i.e. higher biomass deeper post heatwave due to thawing 
# but this doens't match up with the thawing depth data we have seen -> data might be wrong though, one to ask Elise 


# PLOT ROOT BIOMASS BY TEMP FOR 2024 BY COMMUNITY  ----
  library(tidyverse)
  
  # Prepare root biomass data
  root_data <- QHI_combined_data %>%
    filter(year %in% c(2024)) %>%
    filter(!is.na(max_depth_increment), !is.na(root_dry_biomass_total)) %>%
    filter(max_depth_increment != "N/A") %>%
    mutate(depth_numeric = as.numeric(as.character(max_depth_increment)))
  
  # Prepare temperature data - filter for experimental and aggregate by depth
  temp_data <- clean_control_exp_data %>%
    filter(type == "exp") %>%  # filter for experimental
    group_by(depth) %>%
    summarise(
      mean_temp = mean(temp, na.rm = TRUE),
      max_temp = max(temp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(depth_numeric = depth)
  
  # Merge datasets
  combined <- root_data %>%
    left_join(temp_data, by = "depth_numeric") %>%
    filter(!is.na(mean_temp))  # remove depths without temperature data
  
  # Create correlation plot
  (ggplot(combined, aes(x = max_temp, y = root_dry_biomass_total, 
                       color = community, shape = max_depth_increment)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_smooth(aes(group = community), method = "lm", se = TRUE, alpha = 0.2) +
    labs(title = "Root Biomass vs Temperature by Plant Community",
         x = "Max Temperature for 5 day heatwave(°C)",
         y = "Root Biomass (g/m²)",
         color = "Plant Community", 
         shape = "Depth Increment (cm)") +
    scale_shape_manual(values = c(16, 17, 15, 18, 8, 11)) +
    scale_color_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
    theme_classic() +
    theme(legend.position = "right")
  )
  
  
# Alternative: Faceted version for clearer view of each community----
  (ggplot(combined, aes(x = max_temp, y = root_dry_biomass_total, 
                       color = community, shape = max_depth_increment)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    scale_shape_manual(values = c(16, 17, 15, 18, 8, 11)) +
    facet_wrap(~community, scales = "free_y") +
    labs(title = "Root Biomass vs Temperature by Plant Community",
         x = "Max Temperature (°C)",
         y = "Root Biomass (g/m²)",
         color = "Plant Community", 
         shape = "Depth Increment") +
     scale_color_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
    theme_classic() +
    theme(legend.position = "bottom")
  )
  
  
  
  
  
# Add correlation statistics----
  library(ggpubr)
  
  ggplot(combined, aes(x = mean_temp, y = root_dry_biomass_total, 
                       color = community)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             method = "pearson") +
    facet_wrap(~community, scales = "free_y") +
    labs(title = "Root Biomass vs Temperature by Plant Community",
         x = "Mean Temperature (°C)",
         y = "Root Biomass (g/m²)",
         color = "Plant Community") +
    theme_classic() +
    theme(legend.position = "none")
  
  