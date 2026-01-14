# RQ1 ANALYSIS PART 1 
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

# log transform root biomass data
QHI_combined_data <- QHI_combined_data %>%
  mutate(log_root_dry_biomass_total = log(root_dry_biomass_total + 1)) # add 1 to avoid log(0)
# VISUALISE LOG TRANSFORMED DATA WITH HISTOGRAM----
ggplot(QHI_combined_data, aes(x = log_root_dry_biomass_total , fill= year)) +
  geom_histogram(binwidth = 0.05, position = "dodge", color = "black") +
  labs(title = "Distribution of Log Transformed Total Dry Root Biomass by Year ",
       x = "Log Transformed Total Root Biomass",
       y = "Count") +
  theme_minimal() 
# still not normal - non parametric tests required?

# NON-PARAMETRIC TESTS?? -----
# Kruskal-Wallis test to compare root biomass across years
kruskal.test(root_dry_biomass_total ~ year, data = QHI_combined_data) 


pairwise.wilcox.test
