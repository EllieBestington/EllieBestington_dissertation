# DETERMINING SLOPE AND ELEVATION OF ALD POINTS
# PLOTTING RELATIONSHIPS FOR CALIBRATION CURVES 


# LOAD LIBRARIES----
library(raster)
library(terra)
library(readxl)


# LOAD DATA----
phenocam_ald_gps_coordinates <- read_excel("~/dissertation_2026_bestington/datasets/location_data/phenocam_ald_gps_coordinates.xlsx")
ald_only<- phenocam_ald_gps_coordinates[phenocam_ald_gps_coordinates$type == "ald", ]
active_layer_thaw_combined <- read_excel("~/dissertation_2026_bestington/datasets/QHI_roots_data/active_layer_thaw_combined.xlsx")

# convert lat/lon to UTM Zone 7N

coordinates(ald_only) <- ~longitude + latitude
proj4string(ald_only) <- CRS("+proj=longlat +datum=WGS84")

# Transform to UTM Zone 7N to match your raster
sites_utm <- spTransform(ald_only, crs(elevation))



# LOAD DATA ELEVATION----

# set working directory 
setwd("C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/location_data/QHI_landscape_files")
wd<-"C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/location_data/QHI_landscape_files"

# load elevation data
elevation <- raster("elevation.tif")
elevation_terra <- rast("elevation.tif")

# view raster structure 
elevation

# PLOT ELEVATION DATA----
plot(elevation)

# Adjust plot size
par(mar = c(3, 3, 3, 3))
plot(elevation, 
     col = terrain.colors(100),
     main = "Elevation", 
     xlab = "Easting (m)", 
     ylab = "Northing (m)")
points(sites_utm, pch = 4, col = "red2", cex = 1.5)

# EXTRACT ELEVATION VALUES AT ALD POINTS----
elevation_values <- extract(elevation_terra, vect(sites_utm))
elevation_values_df <- as.data.frame(elevation_values)
elevation_values_df

# LOAD DATA SLOPE----
slope <- raster("slope.tif")
slope_terra <- rast("slope.tif")

# view raster structure
slope

# PLOT SLOPE DATA----
plot(slope)
# Adjust plot size
par(mar = c(3, 3, 3, 3))
plot(slope, 
     col = terrain.colors(100), 
     main = "Slope")
points(sites_utm, pch = 4, col = "red2", cex = 1.5)

# EXTRACT SLOPE VALUES AT ALD POINTS----
slope_values <- extract(slope_terra, vect(sites_utm))
slope_values_df <- as.data.frame(slope_values)
slope_values_df

# COMBINE ELEVATION AND SLOPE DATAFRAMES----
ald_elevation_slope_coordinates_ <- read_excel("~/dissertation_2026_bestington/datasets/QHI_roots_data/ald_elevation_slope_coordinates .xlsx")


# PLOT RELATIONSHIPS----
# filter day 228 (last day of measurments )
day228 <- ald_elevation_slope_coordinates_[ald_elevation_slope_coordinates_$doy == 228, ]

# plot thaw against elevation- calibration curve 
ggplot(day228, aes(x = elevation, y = thaw_av)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Active Layer Thaw Depth vs Elevation on Day 228",
       x = "Elevation (m)",
       y = "Thaw Depth (cm)") +
  theme_classic()

# plot thaw against slope- calibration curve 
ggplot(day228, aes(x = slope, y = thaw_av)) +
  geom_point(color = "green", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Active Layer Thaw Depth vs Slope on Day 228",
       x = "Slope (degrees)",
       y = "Thaw Depth (cm)") +
  theme_classic()
