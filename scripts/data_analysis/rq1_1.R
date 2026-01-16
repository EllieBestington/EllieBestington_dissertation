# RQ1 ANALYSIS PART 1 : SIGNIFICANT DIFFERENCE BETWEEN 2023 AND 2024 ROOT BIOMASS?
# 14/01/2026 

# LOAD LIBRARIES -----
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")

# MAKE YEAR FACTOR VARIABLE ----
QHI_combined_data$year <- as.factor(QHI_combined_data$year)

# VISUALISE WITH HISTOGRAM----
ggplot(QHI_combined_data, aes(x = root_dry_biomass_total, fill= year)) +
  geom_histogram(binwidth = 0.05, position = "dodge", color = "black") +
  labs(title = "Distribution of Total Dry Root Biomass by Year",
       x = "Total Root Biomass",
       y = "Count") +
  theme_minimal()

# VISUALISING MEANS WITH BOXPLOT----
(root_boxplot<-ggplot(QHI_combined_data, aes(x = year, y = root_dry_biomass_total, fill = year)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Boxplot of Total Dry Root Biomass by Year",
         x = "Year",
         y = "Total Root Biomass") +
    scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3")) +
    theme_classic()+
   theme(legend.position = "none")
)
# likely not significant as overlap of boxes and whiskers but let's check anyway

# SIMPLE ANOVA----
roots_anova <- aov(root_dry_biomass_total ~ year, data = QHI_combined_data)
summary(roots_anova)

# p= 0.478, no significant difference in overall root biomass between years 

# TRY AS LINEAR MODEL ----
roots_lm <- lm(root_dry_biomass_total ~ year, data = QHI_combined_data)
summary(roots_lm)

# CHECKING ASSUMPTIONS ----
# checking normality
hist(roots_anova$residuals)
plot(roots_anova, which = 2)
shapiro.test(roots_anova$residuals)
# not normally distributed 

# checking homoscedasticity
plot(roots_anova, which = 1)
bartlett.test(root_dry_biomass_total ~ year, data = QHI_combined_data)


# NON-PARAMETRIC TESTS?? -----
# Wilcox Test 
wilcox.test(root_dry_biomass_total ~ year, data = QHI_combined_data)
# p=0.06195 -> no statistically significant difference 




# CONCLUSION----
# No significant difference in root biomass overall between 2023 and 2024 based on ANOVA and Wilcox test results.