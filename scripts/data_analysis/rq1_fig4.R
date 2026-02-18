# RQ1: DATA ANALYSIS
# FIGURE 4: IS THERE A SIGNIFICANT RELATIONSJIP BETWEEN YEAR/TIME AND ACTIVE LAYER 

# Comparison between 4a (up to 2023) and 4b (including 2024/25)


# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)


# LOAD DATA----
ald_all_years<-read_excel("datasets/QHI_roots_data/active_layer_thaw_combined.xlsx")


# DATA CLEANING ----
ald_all_years <- ald_all_years %>%
  filter(!is.na(thaw_av)) 


# new data frame just up to 2023
ald_pre2023 <- ald_all_years %>%
  filter(year %in% c(1986:2023))


# DATA ANALYSIS UP TO 2023----
mod1<-lm(thaw_av ~ year, data = ald_pre2023)
summary(mod1)

# highly significant relationship between year and thaw_av up to 2023, with a positive slope of 0.019.
# about a +0.68 cm increase in active layer per year 
# p value < 0.001
# R2= 0.2366 -> good for natural systems with lots of other variables influencing the outcome.

# DATA ANALYSIS INCLUDING 2024/25----
mod2<-lm(thaw_av ~ year, data = ald_all_years)
summary(mod2)

# still significant but barely - p value = 0.049, just below 0.05 threshold 
# but point being, including 2024/25 changes the significance/relationship 
# now decreasing active of -0.10 cm per year 
# r2= 0.004348, very weak relationship now 
# essentially this is showing that active layers in 2024 much shallower than 2023