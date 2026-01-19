# RQ1 PART 4: SIGNIFICANT DIFFERENCE BETWEEN BIOMASS AND DEPTH BY COMMUNITY TYPE
# 19/01/2026 

# LOAD LIBRARIES----
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(emmeans)

# LOAD DATA----
# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")

# MAKE YEAR FACTOR VARIABLE ----
QHI_combined_data$year <- as.factor(QHI_combined_data$year)
QHI_combined_data$max_depth_increment <- as.factor(QHI_combined_data$max_depth_increment)

# QUESTION----
# Is there a significant difference in biomass and depth by community type?
# i.e. let's test for an interaction between biomass and depth in graminoids, shrubs, and mix within each year and between years?

# DATA TRANSFORM TO CUBE ROOT - SEE RQ1_3----
# cube root
QHI_combined_data <- QHI_combined_data %>% 
  mutate(cube_root_biomass = (root_dry_biomass_total)^(1/3)) 
anova_community_cuberoot<- aov(cube_root_biomass ~ max_depth_increment * community, data = QHI_combined_data)
summary(anova_depth_cuberoot)

shapiro.test(residuals(anova_community_cuberoot))
hist(residuals(anova_community_cuberoot))
# now more normally distrubuted, use this in analysis

# TWO WAY ANOVA 2023 ONLY----
anova_2023_community <- aov(cube_root_biomass ~ max_depth_increment * community, data = filter(QHI_combined_data, year == "2023"))
summary(anova_2023_community)
# root biomass and depth have significant interaction in 2023, but not by community type
# Depth matters for biomass (it changes significantly with depth)
# Community type doesn't affect overall biomass levels
# The way biomass changes with depth is consistent across all three community types (no significant differences) - they all follow the same trend

# split up to get significance of each line in 2023

# graminoids 
anova_2023_graminoids<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2023", community == "Graminoid"))
summary(anova_2023_graminoids)
# significant difference in biomass by depth in graminoid community in 2023 (p= 0.0168)

# shrubs
anova_2023_shrubs<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2023", community == "Shrub"))
summary(anova_2023_shrubs)
# also significant difference in biomass by depth in shrub community in 2023 (p= 0.0121)

# mix
anova_2023_mix<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2023", community == "Mix"))
summary(anova_2023_mix)
# also significant difference in biomass by depth in mix community in 2023 (p= 0.000418)

# each community type shows significant differences in biomass by depth in 2023 but when comparing between them, community type is not significant
# Despite these significant effects within each community, overall biomass levels do not differ between community types (p = 0.363), 
# suggesting that graminoids, shrubs, and mixed communities produce similar total root biomass
# Would R2 values be neccessary? 

# Post-hoc test to see which depths are different
TukeyHSD(anova_2023_community, which = "max_depth_increment")
# Deepest samples (30cm) had significantly lower biomass than mid-depth samples:
        # 30cm vs. 15cm: 0.15 units lower (p = 0.002)
        # 30cm vs. 25cm: 0.17 units lower (p = 0.004)
# Shallow to mid-depth range (5-25cm) showed no significant differences, indicating relatively stable biomass in the upper soil profile
# In 2023, root biomass appears to peak in the mid-depth range (15-25cm) and decline at the deepest depth (30cm) across all community types. 
# suggests that regardless of plant community composition, roots are concentrated in the upper-to-middle soil profile, with less investment in the deepest soil layer sampled.
        # possible due to soil conditions/permafrost layer??
# TWO WAY ANOVA 2024 ONLY----
anova_2024_community <- aov(cube_root_biomass ~ max_depth_increment * community, data = filter(QHI_combined_data, year == "2024"))
summary(anova_2024_community)
# no significant interaction between biomass and depth by community type in 2024
      # possible due to leverage of that point and fewer data points (ongoing lab analysis)

# split up to get significance of each line in 2024
# mix
anova_2024_mix<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2024", community == "Mix"))
summary(anova_2024_mix)
# no significant interaction between biomass and depth for mix community
# again possible due to fewer data points (ongoing lab analysis) and leverage/such massive variation 

# shrub
anova_2024_shrubs<- aov(cube_root_biomass ~ max_depth_increment, data = filter(QHI_combined_data, year == "2024", community == "Shrub"))
summary(anova_2024_shrubs)
# no significant interaction between biomass and depth for shrub community either 
# again possible due to fewer data points (ongoing lab analysis) - only one for each depth increment 