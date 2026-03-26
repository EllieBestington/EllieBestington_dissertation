# RQ0
# HWMId ANALYSIS 
# 11/12/2025

# LOAD LIBRARIES----
library(terra)
library(raster)

# Load the multi-year HWMI data----
hwmi <- rast("C:/Users/ellie/OneDrive - University of Edinburgh/Dissertation/Data/climate_data/arclim_HWMI.tif")

# Check how many layers (years) you have
hwmi
nlyr(hwmi)  # Should be 72 layers (1950-2021)

# Check layer names
names(hwmi)

# Qikiqtaruk coordinates----
qhi_coords <- cbind(-139.09, 69.59)

# Extract all years at once
qhi_timeseries <- extract(hwmi, qhi_coords)

# Convert to a data frame with years
qhi_data <- data.frame(
  year = 1950:2021,
  hwmi = as.numeric(qhi_timeseries[1, ])
)

# View the data
head(qhi_data)
print(qhi_data)

# Simple time series plot----
plot(qhi_data$year, qhi_data$hwmi, 
     type = "b",
     main = "Heatwave Magnitude Index at Qikiqtaruk (1950-2021)",
     xlab = "Year",
     ylab = "HWMI",
     col = "red",
     pch = 19)

# Add trend line
abline(lm(hwmi ~ year, data = qhi_data), col = "blue", lwd = 2)

# GGPLOT----
(ggplot2::ggplot(qhi_data, ggplot2::aes(x = year, y = hwmi)) +
  ggplot2::geom_point(color = "brown3") +
  ggplot2::geom_line() +
  ggplot2::geom_smooth(method = "lm", color = "deepskyblue2", se = FALSE) +
  ggplot2::labs(title = "Heatwave Magnitude Index at Qikiqtaruk (1950-2021)",
                x = "Year",
                y = "HWMI")+
    theme_classic()
)

# Plot a single year (e.g., 2021 - the most recent)-----
plot(hwmi[[72]], 
     main = "HWMI 2021",
     col = heat.colors(100))
points(qhi_coords, pch = 19, col = "blue", cex = 2)

# Or compare early vs recent period
par(mfrow = c(1, 2))
plot(hwmi[[1]], main = "HWMI 1950", col = heat.colors(100))


plot(hwmi[[72]], main = "HWMI 2021", col = heat.colors(100))



# ADDING YEARS 2022, 2023 AND 2024----
library(ecmwfr)

# Set up your API credentials
wf_set_key(key = "77876d3d-d031-4ae5-9c51-c5ec0cd39b4e")

# Define the request for ERA5 reanalysis data for 2m temperature for 2022-202
request <- list(
  product_type = "reanalysis",
  variable = "skin_temperature",
  year = "2022",
  month = c("06", "07", "08"),
  day = c("01", "02", "03", "04","05","06", "07", "08", "09","10", "11", "12"),  # all days
  time = c("00:00", "01:00","02:00", "03:00", "23:00"),  # all hours
  area = c(72, -144, 67, -134),  # North, West, South, East (Qikiqtaruk region)
  format = "netcdf"
)

# QHI heatwave severity increasing over time
# use this to back up what said in literature 
# I made this graph, using ARCLIM data from Russo et al 2014
# see also Russo 2014 paper on severity graph in appendices 
# I think going to be too hard to get data for 2022 onwards but could give a go over Christmas break 