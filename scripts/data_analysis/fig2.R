# RQ1 ANALYSIS
# FIGURE 2: WERE TEMPERATURES IN 2023 SIGNIFICANTLY HIGHER THAN 2024? 

# Load libraries----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)



# LOAD DATA----
met_station_combined <- read_excel("datasets/climate_data/met_station_combined.xlsx")


# DATA PREPARATION----
# Filter data for 2023 and 2024
met_station_filtered <- met_station_combined %>%
  filter(year %in% c(2023, 2024))

# Calculate average temperature for each day of each year 
daily_avg_temp <- met_station_filtered %>%
  group_by(year, doy) %>%
  summarise(avg_temp = mean(air_temp, na.rm = TRUE))

# calculate max temp for each day of each year 
daily_max_temp <- met_station_filtered %>%
  group_by(year, doy) %>%
  summarise(max_temp = max(air_temp, na.rm = TRUE))

# DATA ANALYSIS 1.1----
# were temperature trends across summers of 2023 and 2024 significantly different?
daily_max_temp$year_factor <- factor(daily_max_temp$year)
model1 <- lm(max_temp ~ doy * year_factor, data = daily_max_temp)
summary(model1)

# 2023: actually relatively stable throughout the summer
# slope = -0.022 per day decrease in temp throughout season, essential flat/no trend
# p= 0.7888, not significant (but showing that temps remained high throughout the season)

# 2024: increasing summer temp trend at 0.232C per day-> significant p=0.0251
# evidence for gradual increasing trend in temps 

# significant interaction (p=0.0325)-> temp trend differs significantly between years

# extract R2 for 2023 only 
# Fit model for 2023 only
lm_2023_only <- lm(max_temp ~ doy, data = subset(daily_max_temp, year_factor == "2023"))
summary(lm_2023_only)

lm_2024_only <- lm(max_temp ~ doy, data = subset(daily_max_temp, year_factor == "2024"))
summary(lm_2024_only)

# DATA ANALYSIS 1.2----
# do average summer temps differ between 2023 and 2024, i.e. was 2023 significantly warmer?
# simple t-test 
t.test(max_temp ~ year_factor, data = daily_max_temp)

# 2023: mean max temp = 20.34C
# 2024: mean max temp = 17.75C
# difference of 2.59C but only marginally significant p=0.065 (not quite below 0.05 threshold)
# trending towards 2023 summer being warmer than 2024
# 2023 summer averaged 2.6°C warmer than 2024, though this difference was marginally non-significant (p = 0.065)
# CHECK ASSUMPTIONS----
# only required for 1.1

# Check normality of residuals
residuals <- model1$residuals
shapiro.test(residuals)
# NORMAL p=0.2094

# Check homogeneity of variances
library(car)
leveneTest(max_temp ~ year_factor, data = daily_max_temp)
# Welch test actually good to use here as variances are unequal - use welch test

bartlett.test(max_temp ~ year_factor, data = daily_max_temp)

plot(model1)# Residuals vs Fitted
hist(residuals,main = "Histogram of Residuals", xlab = "Residuals")
