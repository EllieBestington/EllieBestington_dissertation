# SLOPE VS FLAT AND ROOT BIOMASS 


# LOAD LIBRARIES----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)


# LOAD DATA----
QHI_root_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")
subplots_slope_flat <- read_excel("datasets/location_data/subplots_slope_flat.xlsx")


# DATA CLEANING AND MANIPULATION----
# Join slope/flat info to root data
QHI_root_data <- QHI_root_data %>%
  left_join(subplots_slope_flat, by = c("subplot"))
# remove QHI12 as no slope/flat info
QHI_root_data <- QHI_root_data %>%
  filter(subplot != "QHI12")

QHI_root_data <- QHI_root_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
 filter(row_number() != 32)



# PLOT FACET WRAP EACH YEAR----
(ggplot(QHI_root_data, aes(x = type, y = rootmass_bulkdensity)) +
  geom_boxplot() +
  facet_wrap(~year) +
  labs(title = "Root Biomass by Slope/Flat Condition Across Years",
       x = "Slope/Flat Condition",
       y = "Root Biomass Density (g/m²)") +
  theme_minimal()
)

# its really 2024 we are most bothered about 