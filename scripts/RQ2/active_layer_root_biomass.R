# ACTIVE LAYER DEPTH VS ROOT BIOMASS BY YEAR/BY COMMUNITY TYPE 
# 14/01/2026

# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)

# LOAD DATA----
ald_data <- read_excel("datasets/QHI_roots_data/active_layer_depths_data.xlsx")
QHI_root_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")

# note: active layer depths not taken from same place as root biomass samples- working on trying to get data on this 

# CLEAN DATA----
QHI_root_data <- QHI_root_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
  # remove row 32
  filter(row_number() != 32)

# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation 


# NEW DATAFRAME FOR PLOTTING----
# Create the new combined dataframe

# Rename 'subplot' to 'site' if they represent the same thing
# If subplot corresponds to site, you may need to join on both year and site/subplot
# Here's an alternative if site and subplot should match:

merged_data <- merge(
  QHI_root_data[, c("year", "subplot", "max_depth_increment", "community", "rootmass_bulkdensity")],
  ald_data[, c("year", "site", "depth")],
  by.x = c("year", "subplot"),
  by.y = c("year", "site"),
  all = FALSE
)

# Select and rename columns to match your requirements
merged_data <- merged_data[, c("year", "subplot", "max_depth_increment", 
                                   "community", "rootmass_bulkdensity", "depth")]
colnames(merged_data)[2] <- "site"
colnames(merged_data)[6] <- "active_layer_depth"

# View the result
head(merged_data)

# FIGURE 1----
# Scatter plot of Active Layer Depth vs Root Biomass by Year
ggplot(merged_data, aes(x = active_layer_depth, y = as.numeric(rootmass_bulkdensity), color = as.factor(year))) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Active Layer Depth vs Root Biomass by Year",
    x = "Active Layer Depth (cm)",
    y = "Root Biomass Density (g cm⁻³)",
    color = "Year"
  ) +
  scale_colour_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3"))+
  theme_classic()



# FIGURE 2----
# Scatter plot of Active Layer Depth vs Root Biomass by Community Type
ggplot(merged_data, aes(x = active_layer_depth, y = rootmass_bulkdensity, color = community)) +
  geom_point(alpha= 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs( title = "Active Layer Depth vs Root Biomass by Community Type",
    x = "Active Layer Depth (cm)",
    y = "Root Biomass Density (g cm⁻³))",
    color = "Community Type")+
  scale_colour_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
  theme_classic()

# FIGURE 3----
# Scatter plot of Active Layer Depth vs Root Biomass by Year and Community Type AS FACET WRAP
ggplot(merged_data, aes(x = active_layer_depth, y = rootmass_bulkdensity, color = community)) +
  geom_point(alpha= 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs( title = "Active Layer Depth vs Root Biomass by Year and Community Type",
        x = "Active Layer Depth (cm)",
        y = "Root Biomass Density (g cm⁻³)",
        color = "Community Type")+
  scale_colour_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
  theme_classic()+
  facet_wrap(~ year)
