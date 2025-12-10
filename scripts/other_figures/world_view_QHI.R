# RQ0
# Location of QHI 
# 10/12/2025


# LOAD LIBRARIES----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)

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
  geom_sf(data = qhi_sf, color = "#D4A017", size = 5, shape = 16) +
  # Add arrow from label to point 
  geom_curve(
    aes(x = label_x + 450000, y = label_y + 150000,  # Start from label
        xend = point_x - 80000, yend = point_y - 80000),  # End near point
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    linewidth = 0.7,
    color = "#D4A017") +
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
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "gray50", size = 1),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(20, 20, 20, 20)
  )
)
