# MET STATION DATA FROM QHI OVER COMPARISON----
# 29/01/26

# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)


# LOAD DATA----
met_station_combined <- read_excel("datasets/climate_data/met_station_combined.xlsx")


# CLEAN DATA----
# average daily temps for each doy 
met_station_avg <- met_station_combined %>%
  group_by(year, doy) %>%
  summarise(
    avg_temp = mean(air_temp, na.rm = TRUE),
    sd_temp = sd(air_temp, na.rm = TRUE)
  )

# PLOT DATA----
(ggplot(met_station_combined, aes(x = doy, y = air_temp, color = as.factor(year))) +
    geom_point(alpha = 0.1) +
    geom_line(stat = "summary", fun = "max") +
    geom_hline(yintercept = 22, linetype = "dashed", color = "black", linewidth = 1)+
    labs(title = "Max Daily Air Temperature by Day of Year",
         x = "Day of Year",
         y = "Air Temperature (°C)",
         color= "Year") +
    scale_colour_manual(values = c("2023" = "skyblue2", "2024" = "brown3")) +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# each line of data represents one day
# line at 22C, Canadian government classification of heatwave in QHI provience for more than 2 days 
# in 2024 especially at end in august 6 days! 
# note the line represents the MAX temperatures reached that day, but points show all the temperature values for that day 



