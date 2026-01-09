# RQ0
# CLOSER QHI LOCATION WITH SUBPLOTS MARKED 
# 11/12/2025

# LOAD LIBRARIES----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggmap)
library(cowplot)
library(readxl)

# ZOOMED IN VIEW============
# LOAD DATA----
QHI_subplots <- read_excel("datasets/location_data/QHI_TOMST_location_clean.xlsx")

# Get base map data----
canada <- ne_states(country = "canada", returnclass = "sf")

# Convert subplots to spatial object----
subplots_sf <- st_as_sf(QHI_subplots, 
                        coords = c("lon", "lat"), 
                        crs = 4326)
# Calculate center point for QHI
# General QHI study site location (center of your subplots)
# Study site coordinates
site_lon <- -138.91
site_lat <- 69.589

# CREATE MAP 1----
(right_map <- ggplot() +
  geom_sf(data = canada, fill = "tan", color = "#5C4033", linewidth = 0.4) +
  # Add study site point using geom_point
  geom_point(aes(x = site_lon, y = site_lat), 
             color = "brown3", size = 5, shape = 16) +
  # Add label with arrow
  annotate("text", 
           x = -138.9, y = 69.577,
           label = "Study site",
           size = 4.5, fontface = "bold", hjust = 0) +
   # Add "Canada" label on mainland
   annotate("text", 
            x = -139.8, y = 69.55,
            label = "Canada",
            size = 6, fontface = "bold", color = "#5C4033") +
   # Add "Qikiqtaruk (Herschel Island)" label near the point
   annotate("text", 
            x = -139.1, y = 69.61,
            label = "Qikiqtaruk\n(Herschel Island)",
            size = 3.5, fontface = "bold", color = "#5C4033") +
  annotation_north_arrow(location = "tr", 
                          style = north_arrow_fancy_orienteering()) +
  coord_sf(xlim = c(-140, -138.7), ylim = c(69.5, 69.65)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid = element_line(color = "white", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_text(size = 9)
  )
)


# WORLD VIEW===========================
# GET COMPOTENTS----

# Get world data for Arctic view
world <- ne_countries(scale = "medium", returnclass = "sf")

# Qikiqtaruk/Herschel Island coordinates
qhi <- data.frame(
  name = "Qikiqtaruk\n(Herschel Island)",
  lon = -139.0,
  lat = 69.6
)

# Create polar stereographic projection centered on North Pole
polar_crs <- "+proj=ortho +lat_0=90 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Transform world map to polar projection
world_polar <- st_transform(world, crs = polar_crs)

# Create orthographic projection centered on Arctic
ortho_crs <- "+proj=ortho +lat_0=75 +lon_0=-130"

# Transform world map to polar projection
world_ortho <- st_transform(world, crs = ortho_crs)

# Transform QHI point to polar projection
qhi_sf <- st_as_sf(qhi, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = polar_crs)

# Get coordinates for the point (for arrow)
qhi_coords <- st_coordinates(qhi_sf)
point_x <- qhi_coords[1, "X"]
point_y <- qhi_coords[1, "Y"]

# Create label position (to the left of the point)
label_x <- qhi_coords[1] - 1500000  # Move left
label_y <- qhi_coords[2] - 800000   # Move down slightly

# CREATE MAP----
(large_QHI<- ggplot() +
   # Add world in light gray
   geom_sf(data = world_ortho, fill = "gray85", color = "white", size = 0.3) +
   # Add QHI point
   geom_sf(data = qhi_sf, color = "brown3", size = 5, shape = 16) +
   # Add arrow from label to point 
   geom_curve(
     aes(x = label_x + 450000, y = label_y + 150000,  # Start from label
         xend = point_x, yend = point_y),  # End near point
     arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
     linewidth = 0.7,
     color = "brown3") +
   # Add label box to the left
   annotate("label", x = label_x, y = label_y,
            label = "Qikiqtaruk\n(Herschel Island)",
            linewidth = 1, fontface = "bold",
            label.padding = unit(0.4, "lines"),
            label.r = unit(0.2, "lines"),
            fill = "white", color = "black") +
   # Set view
   coord_sf() +
   # Clean theme
   theme_minimal() +
   theme(
     panel.background = element_rect(fill = "white", color = "gray50", size = 1),
     plot.background = element_rect(fill = "white"),
     plot.margin = margin(20, 20, 20, 20),
     axis.title = element_blank()
   )
)



# COMBINE MAPS===========================
(combined_map <- plot_grid(
  large_QHI, 
  right_map,
  ncol = 2,
  rel_widths = c(1, 1)
)
)
# SAVE FIGURE===========================
ggsave(combined_map,
       filename = file.path("figures", "other_figures", "combined_maps.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)
