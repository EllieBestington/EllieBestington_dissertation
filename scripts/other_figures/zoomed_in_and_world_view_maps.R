# QHI STUDY SITE
# WORLD VIEW AND ZOOMED IN WITH PLOTS VIEW
# 12/01/2025
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

# WORLD VIEW ----

# GET COMPOTENTS

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


# CREATE MAP
(large_QHI<- ggplot() +
   # Add world in light gray
   geom_sf(data = world_ortho, fill = "gray85", color = "white", size = 0.3) +
   # Add QHI point
   geom_sf(data = qhi_sf, color = "brown3", size = 2, shape = 0, stroke = 1.5) +
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

# SAVE WORLD VIEW
ggsave(large_QHI,
       filename = file.path("figures", "other_figures", "QHI_world_view.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)

# ZOOMED IN VIEW WITH PLOTS ----
# LOAD DATA
QHI_coordinates  <- read_excel("phenocam_ald_gps_coordinates.xlsx")

# FILTER FOR PHENOCAM ONLY
QHI_subplots <- QHI_coordinates %>%
  dplyr::filter(type == "phenocam") %>%
  dplyr::select(subplot, longitude, latitude)

# Get base map data
canada <- ne_states(country = "canada", returnclass = "sf")

# Convert subplots to spatial object
subplots_sf <- st_as_sf(QHI_subplots,
                        coords = c("longitude", "latitude"),
                        crs = 4326)
# Calculate center point for QHI
# General QHI study site location (center of your subplots)
# Study site coordinates
site_lon <- -138.91
site_lat <- 69.589

# CREATE MAP
(zoomed_map <- ggplot() +
   geom_sf(data = canada, fill = "tan", color = "#5C4033", linewidth = 0.4) +
   # Add study site point using geom_point
   geom_point(data = QHI_subplots,
              aes(x = longitude, y = latitude),
              color = "brown3", size = 2, shape = 1, stroke = 1) +
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


# SAVE ZOOMED VIEW
ggsave(zoomed_map,
       filename = file.path("figures", "other_figures", "QHI_zoomed_view.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)


# ELISE EDITS ----
# correct the crs
custom_crs <- "+proj=laea +lat_0=69.59 +lon_0=-138.91 +datum=WGS84 +units=m +no_defs"
yukon_final <- st_transform(yukon_sf, crs = custom_crs)
subplots_final <- st_transform(subplots_sf, crs = custom_crs)

# get bounding box
xlims <- c(bbox["xmin"] - 14000, bbox["xmax"] + 1000)
ylims <- c(bbox["ymin"] - 6000, bbox["ymax"] + 7000)

# new plot
(final_map <- ggplot() +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "#D0E1F9", color = NA)) +
    geom_sf(data = yukon_final, fill = "#E3D18A", color = "#5C4033", linewidth = 0.5) +
    geom_sf(data = subplots_final, color = "brown3", size = 2.5, shape = 1, stroke = 1.2) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = "bl", width_hint = 0.2, style = "ticks") +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    labs(title = "QIKIQTARUK STUDY SITE",
         subtitle = "Herschel Island, Yukon Territory",
         x = "Longitude", y = "Latitude") +

    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
      panel.grid = element_line(color = "white", linewidth = 0.3),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
)


# SAVE ZOOMED VIEW
ggsave(final_map,
       filename = file.path("figures", "other_figures", "NEW_QHI_zoomed_view.png"),
       device = "png",
       height = 6, width = 10, units = "in",
       dpi = 300)










