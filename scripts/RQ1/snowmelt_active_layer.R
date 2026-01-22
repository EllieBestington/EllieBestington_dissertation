# SNOWMELT DATE AND ACTIVE LAYER DEPTH RELATIONSHIP?
# 22/01/26

# Seeing if the snowmelt date could help explain why ALD decreases each year.
# To accompany investigation of slumping too 

# LOAD LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)


# LOAD DATA----
snowmelt_dates <- read_excel("datasets/climate_data/snowmelt_dates.xlsx")
ald_all_years<- read_excel("datasets/QHI_roots_data/active_layer_thaw_combined.xlsx")

# NOTE: will need to update ALD data as currently this is not from where snowmelt data/phenocams are. Pending Elise 

# MERGE DATA----
# rename column in ald_all_years from sub_plot to subplot
ald_all_years <- ald_all_years %>% rename(subplot = sub_plot) 

ald_snowmelt <- ald_all_years %>% 
  left_join(snowmelt_dates, by = c("subplot", "year"))

# PLOT DATA----
(ggplot(ald_snowmelt, aes(x = doy, y = thaw_av, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "DoY 100% Snow Free", y = "Active Layer Depth (cm)", color = "Year") +
  theme_minimal() +
  ggtitle("Relationship between Snowmelt Date and Active Layer Depth")
)

 