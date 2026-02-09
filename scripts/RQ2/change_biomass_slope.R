# CHANGE IN BIOMASS, DIFFERENT SLOPE DEGREES 


# LOAD LIBRARIES ----
library(tidyverse)
library(ggplot2)
library(readxl)
library(raster)
library(terra)


setwd("C:/Users/ellie/OneDrive/Documents/dissertation_2026_bestington")

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")
phenocam_ald_gps_coordinates <- read_excel("datasets/location_data/phenocam_ald_gps_coordinates.xlsx")

# PLOT SLOPE DATA----

phenocam_only<- phenocam_ald_gps_coordinates[phenocam_ald_gps_coordinates$type == "phenocam", ]
# convert lat/lon to UTM Zone 7N

coordinates(phenocam_only) <- ~longitude + latitude
proj4string(phenocam_only) <- CRS("+proj=longlat +datum=WGS84")

# Transform to UTM Zone 7N to match your raster
sites_utm <- spTransform(phenocam_only, crs(slope))


# load raster data
setwd("C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/location_data/QHI_landscape_files")
slope <- raster("slope.tif")
slope_terra <- rast("slope.tif")

plot(slope)
# Adjust plot size
par(mar = c(3, 3, 3, 3))
plot(slope, 
     col = terrain.colors(100), 
     main = "Slope")
points(sites_utm, pch = 4, col = "red2", cex = 1.5)


# EXTRACT SLOPE VALUES AT PHENOCAM POINTS----
slope_values <- terra::extract(slope_terra, vect(sites_utm))
slope_values_df <- as.data.frame(slope_values)
slope_values_df

# add subplot information to the slope values dataframe
slope_values_df$subplot <- phenocam_only$subplot 

# categorise slope values into bins
slope_values_df <- slope_values_df %>% 
  mutate(slope_category = case_when(
    slope < 4 ~ "Flat",
    slope >= 4 & slope < 7 ~ "Moderate",
    slope >= 7  ~ "Steep"
  ))


# CALCULATE CHANGE IN BIOMASS AT EACH SITE BETWEEN 2023 AND 2024----
# First, we need to calculate the change in biomass for each site. Assuming you have biomass data for both years in your QHI_combined_data, we can do this as follows:
# Fix the data type first
biomass_2024 <- biomass_2024 %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity))

biomass_2023 <- biomass_2023 %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity))

biomass_change <- biomass_2024 %>%
  group_by(subplot, community) %>%
  summarise(biomass_2024 = sum(rootmass_bulkdensity, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    biomass_2023 %>%
      group_by(subplot) %>%
      summarise(biomass_2023 = sum(rootmass_bulkdensity, na.rm = TRUE), .groups = "drop"),
    by = c("subplot")
  ) %>%
  mutate(change_in_biomass = biomass_2024 - biomass_2023)

# convert change to % change
biomass_change <- biomass_change %>%
  mutate(percent_change_in_biomass = (change_in_biomass / biomass_2023) * 100)

# MERGE THE SLOPE VALUES WITH THE BIOMASS CHANGE DATA----
# Assuming you have a common identifier to merge on, such as 'subplot' or 'community
# For example, if 'subplot' is the common identifier:
final_data <- biomass_change %>% 
  left_join(slope_values_df, by = "subplot") 

# BAR CHART CHANGE IN BIOMASS BY COMMUNITY AND SLOPE CATEGORY----
# Create the bar plot
ggplot(final_data, aes(x = slope_category, y = percent_change_in_biomass, fill = community)) +
  geom_col(position = "dodge") +  # dodge puts bars side-by-side
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black") +  # line at zero
  labs(
    x = "Slope Category",
    y = "Change in Biomass (%)",
    fill = "Community Type"
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

# have bar in figure above for each slope category
# Aggregate biomass across all subplots for each slope category and community
biomass_summary <- final_data %>%
  group_by(slope_category, community) %>%
  summarise(
    total_biomass_2024 = sum(biomass_2024, na.rm = TRUE),
    total_biomass_2023 = sum(biomass_2023, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percent_change = ((total_biomass_2024 - total_biomass_2023) / total_biomass_2023) * 100)

# Create the plot
ggplot(biomass_summary, aes(x = slope_category, y = percent_change, fill = community)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black") +
  labs(
    x = "Slope Category",
    y = "Change in Biomass (%)",
    fill = "Community Type"
  ) +
  scale_fill_manual(values = c("Graminoid" = "#E69F00", "Shrub" = "#009E73", "Mix"="#CC79A7"))+
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

# this is what i was going for but unfortunately we don't have that much variation in plots and slope categories
# this only represents 3 subplots as that's all that's been analysed in the lab so far
# still need to add QHI11 and QHI12 
# ALSO, note that the slope categories/values are based off .tiff data from 2017 (might have changed since then due to ALDs)
# ALSO, note that raw slope values have been arrgrated into these categories and steep only goes to 14 degrees 

# PLOT BIOMASS CHANGE AGAINST SLOPE AS A SCATTER PLOT----
ggplot(final_data, aes(x = slope, y = percent_change_in_biomass, color = community)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black") +
  labs(
    x = "Slope (degrees)",
    y = "Change in Biomass (%)",
    color = "Community Type"
  ) +
  scale_color_manual(values = c("Graminoid" = "#E69F00",
                                "Shrub" = "#009E73",
                                "Mix" = "#CC79A7")) +
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

# NOT going to include but helps to draw conclusions 
# steeper slopes (dominated by shrubs) decrease in biomass between the 2023 and 2024
# flatter areas (in this plot with mix) increased in biomass between 2023 and 2024
# this makes sense? As steeper slopes more at risk to erosion and ALD
# compare side by side to active layer graph 

