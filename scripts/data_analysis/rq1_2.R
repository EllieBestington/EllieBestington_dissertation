# RQ1 ANALYSIS PART 2: SIGNIFICANT DIFFERENCE BETWEEN 2023 AND 2024 ROOT BIOMASS BY COMMUNITY TYPE?
# 16/01/2026 

# LOAD LIBRARIES -----
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

# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation 

# QUESTION----
# Is there a significant difference in root biomass between 2023 and 2024 across different community types?
# i.e. does root biomass vary significantly in different community types between the years? 
# e.g. there is a significant difference in shrub root biomass between 2023 and 2024 
# VISUALISE WITH HISTOGRAM----
ggplot(QHI_combined_data, aes(x = rootmass_bulkdensity, fill= community)) +
  geom_histogram(binwidth = 0.05, position = "dodge", color = "black") +
  labs(title = "Distribution of Total Dry Root Biomass by Year",
       x = "Root Biomass Density (g cm⁻³)",
       y = "Count") +
  theme_minimal()

# STATISTICAL TEST: TWO-WAY ANOVA ----
# two way anova needed as two explanatory variables (year and community)
roots_community_anova <- aov(rootmass_bulkdensity ~ year * community, data = QHI_combined_data)
summary(roots_community_anova)

# the effect of community on root biomass differs between years (p= 0.0419)
# The communities don't differ consistently across years, and years don't differ consistently across communities—the pattern changes depending on the combination.

# EMMEANS POST HOC TEST---
# Effect of community at each level of year
emmeans(roots_community_anova, pairwise ~ community | year, adjust = "tukey")

# Effect of year at each level of community
emmeans(roots_community_anova, pairwise ~ year | community, adjust = "tukey")

# Why use emmeans and not tukey HSD?
# Tukey asked: Do any year×community combinations differ from each other?
# Emmeans asks: Do communities differ within each year? Does each community change over time/between the years?

# Results from post hoc analysis
# no significant differece among communities in 2023
# in 2024, mix communities have significantly higher root biomass than shrub (no graminoid yet as lab analysis not completed)- p = 0.0122
# mix community significant increase between years (p=0.0331)- root biomass increased from 0.046 (2023) to 0.086 (2024)
# shrub community, so significant change (p=0.1014) but trend towards decrease- root biomass decreased from 0.057 (2023) to 0.017 (2024)
# no data on 2024 graminoid yet


# CONCLUSIONS----
# Root biomass of different community types varies significantly between 2023 and 2024
# Specifically, mixed communities show a significant increase in root biomass from 2023 to 2024, while shrub communities show a non-significant trend towards a decrease.
# divergence in communities that didn't exist in 2023/pre heatwave exposure 
# possible explanations: 
  # driven by graminoids in mix plots ability to grow deeper from increased thaw? Have to wait to get graminoid plots back from lab 


# CHECKING ASSUMPTIONS----
# Normality of resuiduals
plot(roots_community_anova)
# Shapiro-Wilk test 
shapiro.test(residuals(roots_community_anova))
# Q-Q plot (most useful visual)
qqnorm(residuals(roots_community_anova))
qqline(residuals(roots_community_anova))
# Histogram
hist(residuals(roots_community_anova), breaks = 20)
# not normal 

# Homogeneity of variances
bartlett.test(root_dry_biomass_total ~ interaction(year, community), data = QHI_combined_data)

# TRANSFORMATION OF DATA----
# Log transformation 
QHI_combined_data$log_biomass <- log(QHI_combined_data$root_dry_biomass_total + 0.001)
anova_log <- aov(log_biomass ~ year * community, data = QHI_combined_data)
shapiro.test(residuals(anova_log))
plot(anova_log, which = 2)  # Q-Q plot
hist(residuals(anova_log), breaks = 20)

# Square root transformation
QHI_combined_data$sqrt_biomass <- sqrt(QHI_combined_data$root_dry_biomass_total)
anova_sqrt <- aov(sqrt_biomass ~ year * community, data = QHI_combined_data)
shapiro.test(residuals(anova_sqrt))
plot(anova_sqrt, which = 2)
hist(residuals(anova_sqrt), breaks = 20)

# Cube root 
QHI_combined_data$cbrt_biomass <- sign(QHI_combined_data$rootmass_bulkdensity) * 
  abs(QHI_combined_data$rootmass_bulkdensity)^(1/3)
anova_cbrt <- aov(cbrt_biomass ~ year * community, data = QHI_combined_data)
shapiro.test(residuals(anova_cbrt))
plot(anova_cbrt, which = 2)
hist(residuals(anova_cbrt), breaks = 20)

# cube root deals with it best? Okay to use given smaller sample size for 2024- just need to address this when discussing.
# Check with Elise - histogram looks good, can do comparison before and after in appendix 

# ALTERING ANOVA TO USE CUBE ROOT DATA----
anova_cbrt <- aov(cbrt_biomass ~ year * community, data = QHI_combined_data)
summary(anova_cbrt)

# no longer significant 
# post hoc tests again
emmeans(anova_cbrt, pairwise ~ community | year, adjust = "tukey")
emmeans(anova_cbrt, pairwise ~ year | community, adjust = "tukey")
# no significant differences found anymore 

# NEW CONCLUSIONS ----
# After applying a cube root transformation to meet ANOVA assumptions, the previously observed significant differences in root biomass between community types and years are no longer significant.
# This suggests that the initial findings may have been influenced by non-normality in the data.
# Further investigation with larger sample sizes, especially for 2024, is recommended to validate these BUT can't do due to lab delays- so need to address in discussion limitations