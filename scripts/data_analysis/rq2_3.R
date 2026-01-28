# RQ1 PART 3: SIGNIFICANT DIFFERENCE BETWEEN 2023 AND 2024 ROOT BIOMASS BY DEPTH 
# 16/01/2026

# LOAD LIBRARIES----
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(emmeans)

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")

# CLEAN DATA ----
QHI_combined_data$year <- as.factor(QHI_combined_data$year)
QHI_combined_data <- QHI_combined_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
  # remove row 32
  filter(row_number() != 32)
QHI_combined_data$max_depth_increment <- as.factor(QHI_combined_data$max_depth_increment)

# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation 


# QUESTION ----
# Is there a significant difference in root biomass between 2023 and 2024 across different soil depths?
# i.e. what is the relationship between root biomass and depth? How does this vary between years and if so is it significant?

# TWO WAY ANOVA----
# because have two explanatory variables of year and depth (depth treated as categorical here)
anova_depth<- aov(rootmass_bulkdensity ~ max_depth_increment * year, data = QHI_combined_data)
summary(anova_depth)

# effect of depth on root biomass density does not differ between years (p= 0.203)
# biomass density and depth also do not have a significant relationship (positive, p = 0.571)
  # higher biomass at deeper depths, could be due to thawing and more available space for roots to grow
  # see rq1_4 for biomass, depths by community type analysis, could be that more graminoids deeper down? 


# what's the relationship between depth and biomass for 2024 
anova_2024<- aov(rootmass_bulkdensity ~ max_depth_increment, data = filter(QHI_combined_data, year == "2024"))
summary(anova_2024)

# positive, but not significant (p=0.656)

# what's the relationship between depth and biomass for 2023
anova_2023<- aov(rootmass_bulkdensity ~ max_depth_increment, data = filter(QHI_combined_data, year == "2023"))
summary(anova_2023)
# positive and not significant (p=0.231)

# BUT WE NEED TO NOTE!!
  # sample size imbalance- could be a significant difference in 2024 but not enough data compared to 2023 to determine this (107 vs 27)
 

# CHECK ASSUMPTIONS----
# normality of residuals
plot(anova_depth)
shapiro.test(residuals(anova_depth))
hist(residuals(anova_depth))
# not normal distribution

# homogeneity of variance
bartlett.test(rootmass_bulkdensity ~ interaction(max_depth_increment, year), data = QHI_combined_data)

plot(anova_depth)

# TRANSFORMING DATA----
# log transform
QHI_combined_data <- QHI_combined_data %>% 
  mutate(log_root_biomass = log(root_dry_biomass_total + 1)) # log transform to deal with zeros
anova_depth_log<- aov(log_root_biomass ~ max_depth_increment * year, data = QHI_combined_data)
summary(anova_depth_log)

shapiro.test(residuals(anova_depth_log))
hist(residuals(anova_depth_log))
# no better 

# cube root
QHI_combined_data <- QHI_combined_data %>% 
  mutate(cube_root_biomass = (rootmass_bulkdensity)^(1/3)) 
anova_depth_cuberoot<- aov(cube_root_biomass ~ max_depth_increment * year, data = QHI_combined_data)
summary(anova_depth_cuberoot)

shapiro.test(residuals(anova_depth_cuberoot))
hist(residuals(anova_depth_cuberoot))
# now more normally distrubuted, use this in analysis

# FINAL ANOVA RESULTS AFTER TRANSFORMATION----
anova_depth_cuberoot<- aov(cube_root_biomass ~ max_depth_increment * year, data = QHI_combined_data)
summary(anova_depth_cuberoot)

# only significant effect with biomass and depth (p= 0.0327) When introduce year, no longer significant, 
# so no significant difference/changes in root biomass and depth  between 2023 and 2024, similar relationships/trends in biomass
# see 1.4 for investigating by community type

# what's the relationship between depth and biomass for 2024 
anova_2024_cuberoot<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2024"))
summary(anova_2024_cuberoot)
# not significant (0.869)

# what's the relationship between depth and biomass for 2023
anova_2023_cuberoot<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2023"))
summary(anova_2023_cuberoot)
# 2023 is significant (p= 0.0269)

# BUT WE NEED TO NOTE (as before)!!
  # sample size imbalance- could be a significant difference in 2024 but not enough data compared to 2023 to determine this (107 vs 27)
 