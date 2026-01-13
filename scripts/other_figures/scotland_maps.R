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
    geom_sf(data = scotland, fill = "grey85", color = "white", size = 0.3)+
    geom_sf(data = invermallie_sf, color = "brown3", size = 2, shape = 0, stroke = 1.5) +
    annotate("text", x = -3, y = 57.5, label = "Invermallie Bothy,\n Lochaber", 
             size = 4, fontface = "bold", color = "black") +
    annotate("segment", x = -3.2, y = 57.2, xend = -4.8, yend = 56.95,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             color = "black", size = 0.8) +
    labs(x = "Longitude", y = "Latitude") +
    theme_classic() +
    coord_sf(xlim = c(-8.5, -0.5), ylim = c(54.5, 61.5)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12))
)

# SAVE SCOTLAND MAP----
ggsave(scotland_map,
       filename = file.path("figures", "other_figures", "scotland_map.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)

# ZOOMED IN MAP OF INVERMALLIE BOTHY----
# Get OpenStreetMap data for the area around Invermallie Bothy
bbox <- c(left = -5.2, bottom = 56.9, right = -4.9, top = 57.0)
osm_data <- opq(bbox)
osm_data <- add_osm_feature(osm_data, key = 'highway')
osm_data<-add_osm_feature(osm_data, key = 'waterway')
osm_lines <- osmdata_sf(osm_data)$osm_lines
# Create zoomed-in map
(zoomed_in_map <- ggplot() +
    geom_sf(data = osm_lines, color = "grey50", size = 0.5) +
    geom_sf(data = invermallie_sf, color = "brown3", size = 3, shape = 0, stroke = 1.5) +
    annotate("text", x = -5.05, y = 56.955,
             label = "Invermallie Bothy", 
             size = 4, fontface = "bold", color = "black") +
    labs(x = "Longitude", y = "Latitude") +
    theme_classic() +
    coord_sf(xlim = c(-5.2, -4.9), ylim = c(56.9, 57.0)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12))
)



# Define bounding box
bbox <- c(left = -5.2, bottom = 56.9, right = -4.9, top = 57.0)

# Function to safely query OSM data
safe_osm_query <- function(bbox, key, value = NULL) {
  tryCatch({
    if (is.null(value)) {
      query <- opq(bbox, timeout = 60) %>% add_osm_feature(key = key)
    } else {
      query <- opq(bbox, timeout = 60) %>% add_osm_feature(key = key, value = value)
    }
    Sys.sleep(1)  # Pause between queries
    return(osmdata_sf(query))
  }, error = function(e) {
    message(paste("Failed to get", key, ":", e$message))
    return(NULL)
  })
}

# Get different OSM features (one at a time)
message("Fetching water bodies...")
water_data <- safe_osm_query(bbox, 'natural', 'water')
water_polygons <- if(!is.null(water_data)) water_data$osm_polygons else NULL

message("Fetching waterways...")
waterway_data <- safe_osm_query(bbox, 'waterway')
waterways <- if(!is.null(waterway_data)) waterway_data$osm_lines else NULL

message("Fetching forests...")
forest_data <- safe_osm_query(bbox, 'landuse', 'forest')
forests <- if(!is.null(forest_data)) forest_data$osm_polygons else NULL

message("Fetching footpaths...")
path_data <- safe_osm_query(bbox, 'highway', c('footway', 'path', 'track'))
footpaths <- if(!is.null(path_data)) path_data$osm_lines else NULL

message("Fetching roads...")
road_data <- safe_osm_query(bbox, 'highway', c('primary', 'secondary', 'tertiary', 'unclassified'))
roads <- if(!is.null(road_data)) road_data$osm_lines else NULL

# Create zoomed-in map
(zoomed_in_map <- ggplot() +
    # Base land color
    theme_classic() +
    theme(panel.background = element_rect(fill = "darkseagreen")) +
    
    # Add forests (green)
    {if(!is.null(forests) && nrow(forests) > 0) 
      geom_sf(data = forests, fill = "darkseagreen4", color = "darkseagreen3", 
              size = 0.2, alpha = 0.7)} +
    
    # Add water bodies (blue)
    {if(!is.null(water_polygons) && nrow(water_polygons) > 0) 
      geom_sf(data = water_polygons, fill = "lightblue3", color = "steelblue", size = 0.3)} +
    
    # Add waterways (blue lines)
    {if(!is.null(waterways) && nrow(waterways) > 0) 
      geom_sf(data = waterways, color = "steelblue", size = 0.8)} +
    
    # Add roads (dark grey)
    {if(!is.null(roads) && nrow(roads) > 0) 
      geom_sf(data = roads, color = "#CD9200", size = 1)} +
    
    # Add footpaths (dashed brown)
    {if(!is.null(footpaths) && nrow(footpaths) > 0) 
      geom_sf(data = footpaths, color = "brown", size = 0.6, linetype = "dashed")} +
    
    # Add bothy location (red box)
    geom_sf(data = invermallie_sf, color = "brown3", size = 3, shape = 0, stroke = 2) +
    
    # Add label
    annotate("text", x = -5.12, y = 56.955,
             label = "Invermallie Bothy", 
             size = 5, fontface = "bold", color = "black") +
    # Add arrow
    annotate("segment", x = -5.1, y = 56.952,
             xend = -5.07, yend = 56.952917,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             color = "black", size = 0.8) +
    
    # Add label for Loch Arkaig
    annotate("text", x = -5.14, y = 56.97,
             label = "Loch Arkaig",
             size = 3.5, fontface = "italic", color = "steelblue3", angle= -2) +
    
    # Add label for Loch Lochy
    annotate("text", x = -4.963, y = 56.9425,
             label = "Loch Lochy",
             size = 3.5, fontface = "italic", color = "steelblue3", angle= 45) +
    # Add label for River Mallie
    annotate("text", x = -5.10, y = 56.942,
             label = "River Mallie",
             size = 3.5, fontface = "italic", color = "steelblue2", angle= 20) +
    
    # Add label for B8005
    annotate("text", x = -5.01, y = 56.912,
             label = "B8005",
             size = 3.5, fontface = "italic", color = "antiquewhite2", angle= 45) +
    
    labs(x = "Longitude", y = "Latitude") +
    annotation_north_arrow(location = "tr", 
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    coord_sf(xlim = c(-5.17, -4.95), ylim = c(56.9, 57.0), expand = FALSE) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12))
)

# SAVE
ggsave(zoomed_in_map,
       filename = file.path("figures", "other_figures", "invermallie_zoomed_in_map.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)
