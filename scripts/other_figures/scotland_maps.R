# SCOTLAND STUDY SITE 
# SCOTLAND VIEW AND ZOOMED IN VIEW 
# 12/01/2026
# AUTHOR: ELLIE BESTINGTON 

# LOAD LIBRARIES----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggmap)
library(cowplot)
library(readxl)
library(osmdata)
library(dplyr)   
library(raster)
library(grid)
library(ggrepel)

# MAP OF SCOTLAND---- 

# Get UK administrative areas
uk <- ne_states(country = "united kingdom", returnclass = "sf")

# List of Scottish council areas (based on your output)
scottish_areas <- c(
  "Scottish Borders", "Dumfries and Galloway", "Clackmannanshire", 
  "Stirling", "Falkirk", "West Lothian", "Edinburgh", "Midlothian", 
  "East Lothian", "South Ayrshire", "North Ayrshire", "Inverclyde", 
  "Renfrewshire", "West Dunbartonshire", "Argyll and Bute", "Highland", 
  "Moray", "Aberdeenshire", "Aberdeen", "Angus", "Dundee", 
  "Perth and Kinross", "Fife", "Outer Hebrides", "Orkney Islands", 
  "Shetland Islands", "North Lanarkshire", "East Dunbartonshire", 
  "Glasgow", "East Renfrewshire", "East Ayrshire", "South Lanarkshire"
)

# Filter for Scottish areas
scotland <- uk[uk$name_en %in% scottish_areas, ]

# Create coordinates for Invermallie Bothy 
invermallie_coords <- data.frame(
  name = "Invermallie Bothy",
  lon = -5.066514,
  lat = 56.952917
)

# Convert to sf object
invermallie_sf <- st_as_sf(invermallie_coords, 
                           coords = c("lon", "lat"), 
                           crs = 4326)

# Create the Scotland map
(scotland_map<-ggplot() +
    geom_sf(data = scotland, fill = "lightsteelblue", color = "white", size = 0.3)+
    geom_sf(data = invermallie_sf, color = "brown3", size = 2, shape = 0, stroke = 1.5) +
    annotate("text", x = -3, y = 57.5, label = "Invermallie Bothy,\n Lochaber", 
             size = 4, fontface = "bold", color = "black") +
    annotate("segment", x = -3.2, y = 57.2, xend = -4.8, yend = 56.95,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             color = "black", size = 0.8) +
    theme_classic() +
    coord_sf(xlim = c(-8.5, -0.5), ylim = c(54.5, 61.5)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.background = element_rect(fill = "white"))
)

# SAVE SCOTLAND MAP----
ggsave(scotland_map,
       filename = file.path("figures", "other_figures", "scotland_map.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)
