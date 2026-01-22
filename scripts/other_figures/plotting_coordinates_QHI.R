# PLOTTING COORDINATES 
# 21/01/2026

# Determining where QHI cores extracted from 
# I will use ALD coordinates, phenocam coordinates, and elevation.tiff file 
# Data provided by Isla M-S and Elise Gallois 

# LOAD LIBRARIES----
library(tidyverse)
library(sf)
library(ggplot2)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggmap)

# LOAD DATA----
gps_coordinates <- read_excel("datasets/location_data/phenocam_ald_gps_coordinates.xlsx")


# BASIC MAP PLOTTING ALL COORDINATES----
# Convert to sf object
gps_sf <- st_as_sf(gps_coordinates, coords = c("lon", "lat"), crs = 4326)

# Get base map data
canada <- ne_states(country = "canada", returnclass = "sf")

# Calculate center point for QHI
# General QHI study site location (center of your subplots)
# Study site coordinates
QHI_lon <- -138.91
QHI_lat <- 69.589


(zoomed_map <- ggplot() +
    geom_sf(data = canada, fill = "tan", color = "#5C4033", linewidth = 0.4) +
    # Add study site point using geom_point
    geom_point(data = gps_coordinates, 
               aes(x = lon, y = lat, colour = type, shape = type), 
               size = 2, stroke = 1, alpha = 0.5) +
    # Add label with arrow
    annotate("text", 
             x = -139, y = 69.56,
             label = "Study sites",
             size = 5, fontface = "bold", hjust = 0) +
    # Add "Canada" label on mainland
    annotate("text", 
             x = -139.42, y = 69.53,
             label = "Canadian\n Mainland",
             size = 5, fontface = "bold", color = "#5C4033") +
    # Add "Qikiqtaruk (Herschel Island)" label near the point
    annotate("text", 
             x = -139.1, y = 69.61,
             label = "Qikiqtaruk\n(Herschel Island)",
             size = 5, fontface = "bold", color = "#5C4033") +
    annotation_north_arrow(location = "tr", 
                           style = north_arrow_fancy_orienteering()) +
    coord_sf(xlim = c(-139.5, -138.8), ylim = c(69.5, 69.7), expand = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "lightblue"),
      panel.grid = element_line(color = "white", linewidth = 0.3),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
)
