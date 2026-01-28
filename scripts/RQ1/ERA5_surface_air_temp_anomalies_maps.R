# SURFACE AIR TEMPERATURE ANOMALY JULY 2024 
# 28/01/2026 


# LOAD LIBRARIES----
library(tidyverse)
library(ecmwfr)      # For downloading ERA5 data
library(ncdf4)       # For reading NetCDF files
library(raster)      # For spatial data handling
library(ggplot2)     # For plotting
library(sf)          # For spatial features
library(rnaturalearth) # For world map
library(viridis)     # For color scales
library(terra)

# SET WORKING DIRECTORY----
# set working directory 
setwd("C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/climate_data/ERA5 Data")
wd<-"C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/climate_data/ERA5 Data"


# JULY 2024 SURFACE AIR TEMP ANOMALY WORLD -----
# terra to read the values
t2m_2024 <- rast("7d91be1f823d11ebe4bd035c9a3651db.nc")
t2m_2024 <- setMinMax(t2m_2024)  # This forces it to scan the data

# baseline data 1990- 2020
baseline_all <- rast("fe643b60b687ae11197cc8fefaee9e85.nc")

# Average the 30 baseline years
t2m_baseline <- mean(baseline_all)

# Convert to Celsius
t2m_2024_c <- t2m_2024 - 273.15
t2m_baseline_c <- t2m_baseline - 273.15

# Calculate anomaly
anomaly <- t2m_2024_c - t2m_baseline_c

# Check the range again
print(global(anomaly, "range", na.rm = TRUE))

# Convert to dataframe for plotting
anomaly_df <- as.data.frame(anomaly, xy = TRUE, na.rm = FALSE)
names(anomaly_df) <- c("lon", "lat", "anomaly")

# Remove any infinite or NA values
anomaly_df <- anomaly_df[is.finite(anomaly_df$anomaly), ]

# Check the data
summary(anomaly_df$anomaly)
head(anomaly_df)

# Your anomaly data (assuming you already have anomaly_df)
# Check the distribution
quantile(anomaly_df$anomaly, probs = seq(0, 1, 0.1))


# Convert longitude from 0-360 to -180-180
anomaly_df$lon <- ifelse(anomaly_df$lon > 180, anomaly_df$lon - 360, anomaly_df$lon)

# Now sort by longitude to avoid plotting issues
anomaly_df <- anomaly_df[order(anomaly_df$lon, anomaly_df$lat), ]

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the plot
p <- ggplot() +
  geom_tile(data = anomaly_df, aes(x = lon, y = lat, fill = anomaly)) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = c("#000080", "#4169E1", "#87CEEB", "#FFFFCC", 
               "#FFDAB9", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
    values = scales::rescale(c(-8, -2, -0.5, 0, 0.5, 2, 4, 8, 12)),
    limits = c(-8, 12),
    breaks = seq(-8, 12, by = 2),
    name = "°C"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Surface Air Temperature Anomaly for July 2024",
    caption = "Data: ERA5. Reference period: 1991-2020."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                              margin = margin(b = 10)),
    plot.caption = element_text(size = 11, hjust = 0.5,
                                margin = margin(t = 10)),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p)

# Save
ggsave("surface_air_temp_anomaly_july_2024.png", 
       plot = p, 
       width = 12, 
       height = 7, 
       dpi = 300,
       bg = "white")


# AUGUST 2024 SURFACE AIR TEMP ANOMALY WORLD----
# terra to read the values
t2m_2024_aug <- rast("adae7407aa81cd470024ee18d4c0e4b.nc")
t2m_2024_aug <- setMinMax(t2m_2024)  # This forces it to scan the data

# baseline data 1990- 2020
baseline_all <- rast("fe643b60b687ae11197cc8fefaee9e85.nc")

# Average the 30 baseline years
t2m_baseline <- mean(baseline_all)

# Convert to Celsius
t2m_2024_aug_c <- t2m_2024_aug - 273.15
t2m_baseline_c <- t2m_baseline - 273.15

# Calculate anomaly
anomaly_aug <- t2m_2024_aug_c - t2m_baseline_c

# Check the range again
print(global(anomaly_aug, "range", na.rm = TRUE))

# Convert to dataframe for plotting
anomaly_aug_df <- as.data.frame(anomaly_aug, xy = TRUE, na.rm = FALSE)
names(anomaly_aug_df) <- c("lon", "lat", "anomaly")

# Remove any infinite or NA values
anomaly_aug_df <- anomaly_aug_df[is.finite(anomaly_aug_df$anomaly), ]

# Check the data
summary(anomaly_aug_df$anomaly)
head(anomaly_aug_df)

# Your anomaly data (assuming you already have anomaly_df)
# Check the distribution
quantile(anomaly_aug_df$anomaly, probs = seq(0, 1, 0.1))


# Convert longitude from 0-360 to -180-180
anomaly_aug_df$lon <- ifelse(anomaly_aug_df$lon > 180, anomaly_aug_df$lon - 360, anomaly_aug_df$lon)

# Now sort by longitude to avoid plotting issues
anomaly_aug_df <- anomaly_aug_df[order(anomaly_aug_df$lon, anomaly_aug_df$lat), ]

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the plot
t2_aug <- ggplot() +
  geom_tile(data = anomaly_aug_df, aes(x = lon, y = lat, fill = anomaly)) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = c("#000080", "#4169E1", "#87CEEB", "#FFFFCC", 
               "#FFDAB9", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
    values = scales::rescale(c(-8, -2, -0.5, 0, 0.5, 2, 4, 8, 12)),
    limits = c(-8, 12),
    breaks = seq(-8, 12, by = 2),
    name = "°C"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Surface Air Temperature Anomaly for August 2024",
    caption = "Data: ERA5. Reference period: 1991-2020."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                              margin = margin(b = 10)),
    plot.caption = element_text(size = 11, hjust = 0.5,
                                margin = margin(t = 10)),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(t2_aug)



# JULY 2024 QHI ZOOMED----

# Define the bounding box for Qikiqtaruk/Herschel Island region
# Adjust these as needed for how much area you want to show
lon_min <- -145
lon_max <- -133
lat_min <- 68
lat_max <- 71

# Filter your anomaly data to this region
anomaly_zoom <- anomaly_df[anomaly_df$lon >= lon_min & 
                             anomaly_df$lon <= lon_max & 
                             anomaly_df$lat >= lat_min & 
                             anomaly_df$lat <= lat_max, ]

# Get more detailed map data for North America
world <- ne_countries(scale = "large", returnclass = "sf")  # Use 'large' for more detail
states <- ne_states(country = c("canada", "united states of america"), returnclass = "sf")

# Create zoomed plot
p_zoom <- ggplot() +
  geom_tile(data = anomaly_zoom, aes(x = lon, y = lat, fill = anomaly)) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = states, fill = NA, color = "grey50", linewidth = 0.3) +
  # Add a point for Herschel Island
  geom_point(aes(x = -139.0, y = 69.6), color = "black", size = 2, shape = 21, fill = "yellow") +
  annotate("text", x = -139.0, y = 69.3, label = "Qikiqtaruk\n(Herschel Island)", 
           size = 4, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#000080", "#4169E1", "#87CEEB", "#FFFFCC", 
               "#FFDAB9", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
    values = scales::rescale(c(-8, -2, -0.5, 0, 0.5, 2, 4, 8, 12)),
    limits = c(-8, 12),
    breaks = seq(-8, 12, by = 2),
    name = "°C"
  ) +
  coord_sf(xlim = c(lon_min, lon_max), 
           ylim = c(lat_min, lat_max),
           expand = FALSE) +
  labs(
    title = "Surface Air Temperature Anomaly for July 2024",
    subtitle = "Qikiqtaruk (Herschel Island) Region",
    caption = "Data: ERA5. Reference period: 1991-2020."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_zoom)

# Save
ggsave("herschel_island_temp_anomaly_july_2024.png", 
       plot = p_zoom, 
       width = 10, 
       height = 8, 
       dpi = 300,
       bg = "white")


# AUGUST 2024 QHI ZOOMED----
# Define the bounding box for Qikiqtaruk/Herschel Island region
# Adjust these as needed for how much area you want to show
lon_min <- -145
lon_max <- -133
lat_min <- 68
lat_max <- 71

# Filter your anomaly data to this region
anomaly_aug_zoom <- anomaly_aug_df[anomaly_aug_df$lon >= lon_min & 
                             anomaly_aug_df$lon <= lon_max & 
                             anomaly_aug_df$lat >= lat_min & 
                             anomaly_aug_df$lat <= lat_max, ]

# Get more detailed map data for North America
world <- ne_countries(scale = "large", returnclass = "sf")  # Use 'large' for more detail
states <- ne_states(country = c("canada", "united states of america"), returnclass = "sf")

# Create zoomed plot
p_zoom_aug <- ggplot() +
  geom_tile(data = anomaly_aug_zoom, aes(x = lon, y = lat, fill = anomaly)) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = states, fill = NA, color = "grey50", linewidth = 0.3) +
  # Add a point for Herschel Island
  geom_point(aes(x = -139.0, y = 69.6), color = "black", size = 2, shape = 21, fill = "yellow") +
  annotate("text", x = -139.0, y = 69.3, label = "Qikiqtaruk\n(Herschel Island)", 
           size = 4, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#000080", "#4169E1", "#87CEEB", "#FFFFCC", 
               "#FFDAB9", "#FFA500", "#FF6347", "#DC143C", "#8B0000"),
    values = scales::rescale(c(-8, -2, -0.5, 0, 0.5, 2, 4, 8, 12)),
    limits = c(-8, 12),
    breaks = seq(-8, 12, by = 2),
    name = "°C"
  ) +
  coord_sf(xlim = c(lon_min, lon_max), 
           ylim = c(lat_min, lat_max),
           expand = FALSE) +
  labs(
    title = "Surface Air Temperature Anomaly for August 2024",
    subtitle = "Qikiqtaruk (Herschel Island) Region",
    caption = "Data: ERA5. Reference period: 1991-2020."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_zoom_aug)
# CHANGE WD BACK ----
setwd("C:/Users/ellie/OneDrive/Documents/dissertation_2026_bestington")
