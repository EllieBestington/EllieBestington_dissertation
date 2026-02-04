# JUL/AUG TEMPS 2023/2024
# 28/01/2026 

# This script analyses TOMST logger temperature data for July and August of 2023 and 2024.
# Averaged out daily temperatures are calculated and plotted for comparison, taken from ground QHI 


# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)



# LOAD DATA----
TOMST_summer_temps <- read_excel("~/dissertation_2026_bestington/datasets/QHI_roots_data/TOMST_summer_temps.xlsx")


# AVERAGE DAILY TEMPS----
daily_air_temps <- TOMST_summer_temps %>%
  group_by(year, doy) %>%
  summarise(daily_air_temp = mean(air_temp, na.rm = TRUE))

daily_surface_temps<- TOMST_summer_temps %>%
  group_by(year, doy) %>%
  summarise(daily_surface_temp = mean(surface_temp, na.rm = TRUE))

daily_soil_temps<- TOMST_summer_temps %>%
  group_by(year, doy) %>%
  summarise(daily_soil_temp = mean(below_temp, na.rm = TRUE))


# PLOT DAILY AIR TEMPS----
(air_temps<-ggplot(daily_air_temps, aes(x = doy, y = daily_air_temp, color = as.factor(year))) +
  geom_line(size = 1) +
  labs(title = "Air") +
   scale_color_manual(values = c("2023" = "brown3", "2024" = "steelblue")) +
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
)


# PLOT DAILY SURFACE TEMPS----
(surface_temps<-ggplot(daily_surface_temps, aes(x = doy, y = daily_surface_temp, color = as.factor(year))) +
   geom_line(size = 1) +
   labs(title = "Surface",
        y = "Average Temperature (°C)")+
   scale_color_manual(values = c("2023" = "brown3", "2024" = "steelblue")) +
   theme_classic()+
   theme(axis.title.x = element_blank(),
         plot.title = element_text(hjust = 0.5),
         legend.position = "none")
)


# PLOT DAILY SOIL TEMPS----
(soil_temps<-ggplot(daily_soil_temps, aes(x = doy, y = daily_soil_temp, color = as.factor(year))) +
   geom_line(size = 1) +
   labs(title = "Soil",
        x = "Day of Year",
        color= "Year") +
   scale_color_manual(values = c("2023" = "brown3", "2024" = "steelblue")) +
   theme_classic()+
   theme(legend.position = "bottom",
         plot.title = element_text(hjust = 0.5),
         axis.title.y = element_blank())
)

# ALL FIGURES ON ONE----
library(gridExtra)
TOMST_summer_temps<-grid.arrange(air_temps, surface_temps, soil_temps, ncol = 1)


# SAVE FIGURE----
ggsave("~/dissertation_2026_bestington/figures/RQ1/TOMST_summer_temps.png", TOMST_summer_temps, width = 6, height = 8, dpi = 300)

