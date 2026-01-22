# RQ1 ANALYSIS PART 1 : SIGNIFICANT DIFFERENCE BETWEEN 2023 AND 2024 ROOT BIOMASS?
# 14/01/2026 

# LOAD LIBRARIES -----
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

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

# VISUALISE WITH HISTOGRAM----
ggplot(QHI_combined_data, aes(x = rootmass_bulkdensity, fill= year)) +
  geom_histogram(binwidth = 0.05, position = "dodge", color = "black") +
  labs(title = "Distribution of Root Biomass Density by Year",
       x = "Root Biomass Density (g cm⁻³)",
       y = "Count") +
  theme_minimal()

# VISUALISING MEANS WITH BOXPLOT----
(root_boxplot<-ggplot(QHI_combined_data, aes(x = year, y = rootmass_bulkdensity, fill = year)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Boxplot of Root Biomass Density by Year",
         x = "Year",
         y = "Root Biomass Density (g cm⁻³)") +
    scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3")) +
    theme_classic()+
   theme(legend.position = "none")
)
# likely not significant as overlap of boxes and whiskers but let's check anyway

# SIMPLE ANOVA----
roots_anova <- aov(rootmass_bulkdensity ~ year, data = QHI_combined_data)
summary(roots_anova)

# p= 0.691, no significant difference in overall root biomass between years 

# TRY AS LINEAR MODEL ----
roots_lm <- lm(rootmass_bulkdensity ~ year, data = QHI_combined_data)
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

# CUBE ROOT TRANSFORMATION?----
QHI_combined_data <- QHI_combined_data %>% 
  mutate(cube_root_biomass = (rootmass_bulkdensity)^(1/3))
hist(QHI_combined_data$cube_root_biomass)
roots_cuberoot_anova <- aov(cube_root_biomass ~ year, data = QHI_combined_data)
summary(roots_cuberoot_anova)
shapiro.test(residuals(roots_cuberoot_anova))

# normally distributed now 
# still not significant p= 0.178


# CONCLUSION----
# No significant difference in root biomass density overall between 2023 and 2024 based on ANOVA 