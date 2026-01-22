# UPDATED QHI ZOOMED IN MAP USING SATELITTE IMAGERY - AN ATTEMPT
# 22/01/2026


# LOAD LIBRARIES----
library(tidyverse)
library(readxl)
library(leaflet)


# LOAD DATA----
study_sites<-phenocam_ald_gps_coordinates <- read_excel("datasets/location_data/phenocam_ald_gps_coordinates.xlsx")


# FILTER DATA----
# only phenocam
phenocam_sites<-study_sites %>%
  filter(type=="phenocam")

# INTERACTIVE MAP USING LEAFLET PACKAGE----
leaflet_map <- leaflet(phenocam_sites) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Satellite imagery
  addMarkers(~lon, ~lat, popup = ~subplot, label = ~subplot) %>%
  addScaleBar()

# Display
leaflet_map

# Save as HTML
library(htmlwidgets)
saveWidget(leaflet_map, "herschel_island_interactive.html"
           
           
# 