# RQ2: DATA ANALYSIS
# FIGURE 5: Root biomass, community between years 
# FIGURE 6: Root biomass, community, year, depth 

# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)



# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")


# CLEAN DATA----
QHI_combined_data <- QHI_combined_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
  # remove row 32
  filter(row_number() != 32)

# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation


# FILTER FOR JUST P3----
QHI_combined_data_P3 <- QHI_combined_data %>%
  filter(core_ID == "P3")

# DATA ANALYSIS FIGURE 5----
# Is there a significant difference in root biomass between 2023 and 2024 by community type?#

mod1<-aov(rootmass_bulkdensity ~ as.character(year) + community, data = QHI_combined_data_P3)
summary(mod1)

# No significant difference between 2023 and 2024 (p = 0.608)
# No significant difference among communities (p = 0.296)
# Root mass bulk density is similar across years and community types

# check assumptions 
shapiro.test(mod1$residuals)
hist(mod1$residuals) 
# not normal 

# CUBE ROOT DATA
QHI_combined_data_P3 <- QHI_combined_data_P3 %>%
  mutate(rootmass_bulkdensity_cuberoot = rootmass_bulkdensity^(1/3))
mod1_cuberoot <- aov(rootmass_bulkdensity_cuberoot ~ as.character(year) + community, data = QHI_combined_data_P3)
summary(mod1_cuberoot)

shapiro.test(mod1_cuberoot$residuals)
hist(mod1_cuberoot$residuals)
# now normal 

bartlett.test(rootmass_bulkdensity_cuberoot ~ as.factor(year), data = QHI_combined_data_P3)

# USE THIS LINEAR MIXED MODEL INSTEAD to account for subplot!!
model3 <- lmerTest::lmer(rootmass_bulkdensity_cuberoot ~ as.character(year) + community + (1 | subplot), data = QHI_combined_data_P3)
summary(model3)


# Now let's split it up by community type- using cube root data to meet assumptions and lmer to add subplot as random effect   
# Shrub: is there a significant difference in shrub root biomass between 2023 and 2024?
QHI_shrub_data <- QHI_combined_data_P3 %>%
  filter(community == "Shrub")
# use lmer to account for subplot as random effect
mod_shrub_lmer <- lmer(rootmass_bulkdensity_cuberoot ~ as.character(year) + (1 | subplot), data = QHI_shrub_data)
summary(mod_shrub_lmer)
# No significant difference in shrub root biomass between 2023 and 2024 (p = 0.397)
# no significant change (i.e. decrease) but most decrease out of all communties, so may be a trend to watch for in future years


# Mix: is there a significant difference in mix root biomass between 2023 and 2024?
QHI_mix_data <- QHI_combined_data_P3 %>%
  filter(community == "Mix")
mod_mix_lmer <- lmer(rootmass_bulkdensity_cuberoot ~ as.character(year) + (1 | subplot), data = QHI_mix_data)
summary(mod_mix_lmer)
# No significant difference in mix root biomass between 2023 and 2024 (p = 0.966)

# Graminoid: is there a significant difference in graminoid root biomass between 2023 and 2024?
# Can't do yet as no 2024 data but will need to do lm as only two plots 
QHI_graminoid_data <- QHI_combined_data_P3 %>%
  filter(community == "Graminoid")
mod_graminoid_lm <- lm(rootmass_bulkdensity_cuberoot ~ as.character(year), data = QHI_graminoid_data)
summary(mod_graminoid_lm)
# No significant difference in graminoid root biomass between 2023 and 2024 

# DATA ANALYSIS FIGURE 6- including 30cm ----
# Is there a significant relationship between depth and root biomass in 2023 vs 2024 by community type?
# i.e. comparison of biomass:depth relationship for shrubs, mix, and graminoid communities in 2023 vs 2024

# all one model-> including 30 cm
# Convert to numeric
QHI_combined_data_P3 <- QHI_combined_data_P3 %>%
  mutate(max_depth_numeric = as.numeric(as.character(max_depth_increment)))

# refit model with numeric depth
mod_slopes <- lmer(rootmass_bulkdensity_cuberoot ~ as.character(year) * community * max_depth_numeric + 
                     (1 | subplot), 
                   data = QHI_combined_data_P3)

summary(mod_slopes)

# Test if slopes differ between years within each community
emtrends(mod_slopes, pairwise ~ year | community, var = "max_depth_numeric")

# Graminoid
# 2023 slope: -0.011 (decreasing slightly with depth)
# 2024 slope: no data yet 

# Mix
# 2023 slope: -0.0028 (essentially flat, very slight decrease 
# 2024 slope: -0.0006 (also essentially flat, very very tiny increase)
# Difference: p= 0.8877 -> no significant change in slope/relationship between years

# Shrub
# 2023 slope: +0.0014 (very tiny increase)
# 2024 slope: +0.0058 (very tiny decrease) 
# Difference: p = 0.89 -> no significant change in slope/relationship between years 

# Overall 
# none of the communities show significant chanegs in the depth-biomass relationship between 2023 and 2024
# relationships are very consistent between years 



# DATA ANALYSIS FIGURE 6- excluding 30 cm----
# Exclude 30 cm depth increment
QHI_combined_data_P3_no30 <- QHI_combined_data_P3 %>%
  filter(max_depth_increment != "30")

QHI_combined_data_P3_no30 <- QHI_combined_data_P3_no30 %>%
  mutate(max_depth_numerical = as.numeric(as.character(max_depth_increment)))

# Refit model without 30 cm depth increment
mod_slopes_no30 <- lmer(rootmass_bulkdensity_cuberoot ~ as.character(year) * community * max_depth_numerical + 
                          (1 | subplot), 
                        data = QHI_combined_data_P3_no30)
summary(mod_slopes_no30)

# Test if slopes differ between years within each community
emtrends(mod_slopes_no30, pairwise ~ year | community, var = "max_depth_numerical")



# DATA ANANLYSIS FIGURE 6- including 30cm EACH SLOPE----
# Q1: Is there a significant depth-biomass relationship in 2023 shrubs?
# Q2: Is there a significant depth-biomass relationship in 2024 shrubs?
# Q3: Is there a significant depth-biomass relationship in 2023 mix?
# Q4: Is there a significant depth-biomass relationship in 2024 mix?
# Q5: Is there a significant depth-biomass relationship in 2023 graminoid?
# Q6: Is there a significant depth-biomass relationship in 2024 graminoid?

# Convert to numeric
QHI_combined_data_P3 <- QHI_combined_data_P3 %>%
  mutate(max_depth_numeric = as.numeric(as.character(max_depth_increment)))

# Q1
shrub_2023 <- filter(QHI_combined_data_P3, community == "Shrub", year == 2023)
mod_shrub_2023 <- lmer(rootmass_bulkdensity_cuberoot ~ max_depth_numeric + (1 | subplot), 
                       data = shrub_2023)
summary(mod_shrub_2023)  

# Q2
shrub_2024 <- filter(QHI_combined_data_P3, community == "Shrub", year == 2024)
mod_shrub_2024 <- lm(rootmass_bulkdensity_cuberoot ~ max_depth_numeric, 
                       data = shrub_2024)
summary(mod_shrub_2024)

# Q3
mix_2023 <- filter(QHI_combined_data_P3, community == "Mix", year == 2023)
mod_mix_2023 <- lmer(rootmass_bulkdensity_cuberoot ~ max_depth_numeric + (1 | subplot), 
                       data = mix_2023)
summary(mod_mix_2023)

# Q4
mix_2024 <- filter(QHI_combined_data_P3, community == "Mix", year == 2024)
mod_mix_2024 <- lm(rootmass_bulkdensity_cuberoot ~ max_depth_numeric, 
                       data = mix_2024)
summary(mod_mix_2024)

# Q5
graminoid_2023 <- filter(QHI_combined_data_P3, community == "Graminoid", year == 2023)
mod_graminoid_2023 <- lmer(rootmass_bulkdensity_cuberoot ~ max_depth_numeric + (1 | subplot), 
                       data = graminoid_2023)
summary(mod_graminoid_2023)

# Q6
graminoid_2024 <- filter(QHI_combined_data_P3, community == "Graminoid", year == 2024)
mod_graminoid_2024 <- lm(rootmass_bulkdensity_cuberoot ~ max_depth_numeric, 
                       data = graminoid_2024)
summary(mod_graminoid_2024)

# DATA ANALYSIS FIGURE 6- excluding 30cm EACH SLOPE----
# Repeat the above analyses but excluding 30 cm depth increment

#Q1
shrub_2023_no30 <- filter(QHI_combined_data_P3_no30, community == "Shrub", year == 2023)
mod_shrub_2023_no30 <- lmer(rootmass_bulkdensity_cuberoot ~ max_depth_numerical + (1 | subplot), 
                       data = shrub_2023_no30)
summary(mod_shrub_2023_no30)

# Q2
shrub_2024_no30 <- filter(QHI_combined_data_P3_no30, community == "Shrub", year == 2024)
mod_shrub_2024_no30 <- lm(rootmass_bulkdensity_cuberoot ~ max_depth_numerical, 
                       data = shrub_2024_no30)
summary(mod_shrub_2024_no30)

# Q3
mix_2023_no30 <- filter(QHI_combined_data_P3_no30, community == "Mix", year == 2023)
mod_mix_2023_no30 <- lmer(rootmass_bulkdensity_cuberoot ~ max_depth_numerical + (1 | subplot), 
                       data = mix_2023_no30)
summary(mod_mix_2023_no30)

# Q4
mix_2024_no30 <- filter(QHI_combined_data_P3_no30, community == "Mix", year == 2024)
mod_mix_2024_no30 <- lm(rootmass_bulkdensity_cuberoot ~ max_depth_numerical, 
                       data = mix_2024_no30)
summary(mod_mix_2024_no30)

# Q5
graminoid_2023_no30 <- filter(QHI_combined_data_P3_no30 , community == "Graminoid", year == 2023)
mod_graminoid_2023_no30 <- lmer(rootmass_bulkdensity_cuberoot ~ max_depth_numerical + (1 | subplot), 
                       data = graminoid_2023_no30)
summary(mod_graminoid_2023_no30)

# Q6
graminoid_2024_no30 <- filter(QHI_combined_data_P3_no30, community == "Graminoid", year == 2024)
mod_graminoid_2024_no30 <- lm(rootmass_bulkdensity_cuberoot ~ max_depth_numerical, 
                       data = graminoid_2024_no30)
summary(mod_graminoid_2024_no30)
