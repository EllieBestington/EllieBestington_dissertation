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

# max daily temps for each doy
met_station_max <- met_station_combined %>%
  group_by(year, doy) %>%
  summarise(
    max_temp = max(air_temp, na.rm = TRUE)
  )

# PLOT 2023 and 2024 only----
# plot 2023 and 2024 data only
max_2023_2024<-ggplot(filter(met_station_combined, year %in% c(2023, 2024)), aes(x = doy, y = air_temp, color = factor(year))) +
  geom_point(alpha = 0.1) +
  geom_line(stat = "summary", fun = "max") + 
  geom_hline(yintercept = 22, linetype = "dashed",
             color = "black", linewidth = 1)+
  labs(x = "Day of Year",
       y = "Air Temperature (°C)",
       color = "Year") +
  scale_color_manual(values = c("brown3", "steelblue")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# each line of data represents one day
# line at 22C, Canadian government classification of heatwave in QHI provience for more than 2 days 
# in 2024 especially at end in august 6 days! 
# note the line represents the MAX temperatures reached that day, but points show all the temperature values for that day 

# quite a messy graph above, but trying to show comparison of years and that 2023 was on average hotter 
# but do i already show this from ERA5 anomaly map?

# PLOT JUST 2023 MAX TEMP DATA----
(temp_2023<-ggplot(filter(met_station_combined, year == 2023), aes(x = doy, y = air_temp)) +
    geom_point(alpha = 0.1, color = "brown3") +
    geom_line(stat = "summary", fun = "max" , color = "brown3") + 
    geom_hline(yintercept = 22, linetype = "dashed",
               color = "black", linewidth = 1)+
    labs(x = "Day of Year",
         y = "Air Temperature (°C)") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# just 2023 data to show how long heatwaves were for that summer 
# Canada met office states over 22C for more than 2 consecutive days 
# in diss doc circle/annotate peaks 


# PLOT JUST 2024 MAX TEMP DATA----
(temp_2024<-ggplot(filter(met_station_combined, year == 2024), aes(x = doy, y = air_temp)) +
   geom_point(alpha = 0.1, color = "steelblue") +
   geom_line(stat = "summary", fun = "max" , color = "steelblue") + 
   geom_hline(yintercept = 22, linetype = "dashed",
              color = "black", linewidth = 1)+
   labs(x = "Day of Year",
        y = "Air Temperature (°C)") +
   theme_classic()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
)


# PLOT JUST 2023 AV TEMP DATA----
(ggplot(filter(met_station_avg, year == 2023), aes(x = doy, y = avg_temp)) +
   geom_point(color = "brown3", size = 2, alpha=0.5) +
    geom_line(color = "brown3", size = 1) +
    geom_hline(yintercept = 22, linetype = "dashed",
               color = "black", linewidth = 1)+
    labs(x = "Day of Year",
         y = "Average Air Temperature (°C)") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
