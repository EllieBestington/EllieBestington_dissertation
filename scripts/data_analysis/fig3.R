# RQ1: DATA ANALYSIS 
# FIGURE 3: WERE TEMPS SIGNIFICANTLY WARMER IN 2023 FOR TOMST LOGGER MEASUREMENTS 
# i.e does this match the trend with macro temps 


# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)

# LOAD DATA----
TOMST_summer_temps <- read_excel("datasets/QHI_roots_data/TOMST_summer_temps.xlsx")

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

# DATA ANALYSIS: AIR TEMPS----
# were air temperature trends across summers of 2023 and 2024 significantly different?
daily_air_temps$year_factor <- factor(daily_air_temps$year)
model2 <- lm(daily_air_temp ~ doy * year_factor, data = daily_air_temps)
summary(model2)

# check assumptions of the model
plot(model2)
hist(residuals(model2))
shapiro.test(residuals(model2))
# normal p=0.35

bartlett.test(daily_air_temps$daily_air_temp ~ daily_air_temps$year_factor)
# unequal variance

# do average air summer temps differ between 2023 and 2024, i.e. was 2023 significantly warmer?
# simple t-test 
t.test(daily_air_temp ~ year_factor, data = daily_air_temps)

# 2023: mean= 14.21C
# 2024: 10.65C
# difference of 3.56C warmer in 2023
# very significant p = 0.000064
# 95% confidence interval is 1.89 to 5.23C)

# DATA ANALYSIS: SURFACE TEMPS----
# were surface temperature trends across summers of 2023 and 2024 significantly different?
daily_surface_temps$year_factor <- factor(daily_surface_temps$year)
model3 <- lm(daily_surface_temp ~ doy * year_factor, data = daily_surface_temps)
summary(model3)

# check assumptions of the model
plot(model3)
hist(residuals(model3))
shapiro.test(residuals(model3))
# normal p=0.5184

bartlett.test(daily_surface_temps$daily_surface_temp ~ daily_surface_temps$year_factor)
# unequal variance

# do average surface summer temps differ between 2023 and 2024, i.e. was 2023 significantly warmer?
# simple t-test
t.test(daily_surface_temp ~ year_factor, data = daily_surface_temps)


# DATA ANALYSIS: SOIL TEMPS----
# were soil temperature trends across summers of 2023 and 2024 significantly different?
daily_soil_temps$year_factor <- factor(daily_soil_temps$year)
model4 <- lm(daily_soil_temp ~ doy * year_factor, data = daily_soil_temps)
summary(model4)

# check assumptions of the model
plot(model4)
hist(residuals(model4))
shapiro.test(residuals(model4))
# normal p=0.6

bartlett.test(daily_soil_temps$daily_soil_temp ~ daily_soil_temps$year_factor)
# equal variance -> p =0.016

# do average soil summer temps differ between 2023 and 2024, i.e. was 2023 significantly warmer?
# simple t-test
t.test(daily_soil_temp ~ year_factor, data = daily_soil_temps)
