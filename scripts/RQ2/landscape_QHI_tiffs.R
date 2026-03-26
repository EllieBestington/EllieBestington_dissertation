# PLOTTING ELEVATION.TIFF DATA QHI 
# 22/01/26

# AIM----
# To plot elevation data from a TIFF file using R in order to explore possible explanations for ALD changes 
# Investigating possible thawing/slumping movement 


# LOAD LIBRARIES----
library(raster)
library(terra)
library(readxl)

# LOAD STUDY SITES AND CONVERT COORDINATES----
phenocam_ald_gps_coordinates <- read_excel("~/dissertation_2026_bestington/datasets/location_data/phenocam_ald_gps_coordinates.xlsx")
phenocam_only<- phenocam_ald_gps_coordinates[phenocam_ald_gps_coordinates$type == "phenocam", ]

# convert lat/lon to UTM Zone 7N

coordinates(phenocam_only) <- ~lon + lat
proj4string(phenocam_only) <- CRS("+proj=longlat +datum=WGS84")

# Transform to UTM Zone 7N to match your raster
sites_utm <- spTransform(phenocam_only, crs(elevation))



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


# LOAD ASPECT DATA----
aspect <- raster("aspect.tif")
aspect_terra <- rast("aspect.tif")

# view raster structure
aspect

# PLOT ASPECT DATA----
plot(aspect)
# Adjust plot size
par(mar = c(3, 3, 3, 3))
plot(aspect, 
     col = terrain.colors(100),
     main = "Aspect")
points(sites_utm, pch = 4, col = "red2", cex = 1.5)


# COMBINE SLOPE AND ELEVATION----
elev_slope_stack <- stack(elevation, slope)
names(elev_slope_stack) <- c("Elevation", "Slope")
# Plot combined data
par(mar = c(3, 3, 3, 3))
plot(elev_slope_stack, 
     col = terrain.colors(100), 
     main = c("Elevation", "Slope"))
points(sites_utm, pch = 4, col = "red2", cex = 1.5)


