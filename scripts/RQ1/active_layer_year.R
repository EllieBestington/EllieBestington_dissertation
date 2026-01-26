# ACTIVE LAYER DEPTH COMPARISON
# 19/01/2026

# LOAD LIBRARIES----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# LOAD DATA----
ald_data <- read_excel("datasets/QHI_roots_data/active_layer_depths_data.xlsx")
ald_all_years<-read_excel("datasets/QHI_roots_data/active_layer_thaw_combined.xlsx")

# DATA CLEANING----
ald_data$year <- as.factor(ald_data$year)
ald_all_years$year <- as.factor(ald_all_years$year)

# ACTIVE LAYER 2022 VS 2024 ----
(ald_plot<-ald_data %>%
    filter(year %in% c(2023, 2024)) %>%
    ggplot(aes(x = year, y = depth, fill = as.factor(year))) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
    labs(
      x = "Year",
      y = "Active Layer Depth (cm)",
      fill = "Year") +
    scale_fill_manual(values = c("2023" = "deepskyblue3", "2024" = "brown3"))+
    theme_classic() 
)

# WTF is active layer depth decreasing post heatwave- surely over the years active layer is getting bigger from permafrost thawing 
# some explanations 
# 1. Measurement error or inconsistencies in data collection methods between years or mixed up two years when putting data folder together 
# 2. Unusual weather patterns in 2024 that led to shallower active layers, such as increased precipitation or cooler temperatures (though doesn't correlate with temperature data)
# 3. Changes in vegetation cover or soil composition that could affect insulation and heat transfer to the permafrost layer- i.e. increase in vegetation cover, more mosses under warmer temperatures, create blanket and insulate?

# OLD PLOT -- ACTIVE LAYER 2022, 2024, 2025 COMPARISON----
(ald_all_years_plot<-ald_all_years %>%
    ggplot(aes(x = year, y = thaw_av, fill = as.factor(year))) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
    labs(
      x = "Year",
      y = "Active Layer Depth (cm)",
      fill = "Year") +
    scale_fill_manual(values = c("2022" = "lightblue", "2024" = "brown3", "2025"= "darkred"))+
    theme_classic()+
    theme(legend.position = "none")
)

# Why is active layer decreasing over time? Unless its been labelled wrong and instead of active layer it should be permafrost layer? But how would they measure that?
# Makes more sense that over the years, with heatwaves and CC for active layer depth to increase as permafrost thaw increases?
# worth noting though that our root core depths only go to 30 cm which isn't even the minimum of any of these ald 
# so maybe the active layer depth data is correct but our root data isn't deep enough to capture roots in the full active layer?


# NEW PLOT- ACTIVE LAYER 2022, 2024, 2025 COMPARISON----

# clean data
ald_all_years <- ald_all_years %>%
  mutate(year = as.factor(year))
# filter data for in 2022 doy 217, in 2024 doy 223 & 224, in 2025 doy 223 & 225
ald_filtered <- ald_all_years %>%
  filter((year == 2022 & doy == 217) |
           (year == 2024 & doy %in% c(223, 224)) |
           (year == 2025 & doy %in% c(223, 225)))
# filtering for last sampling dates in each year to compare ALD across years 

# plot 
(ald_all_years_plot <- ggplot(ald_filtered, aes(x = year, y = thaw_av, fill = year)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.5) +
    labs(title = "Active Layer Depth Across Years at End of Growing Season",
         x = "Year",
         y = "Active Layer Depth (cm)") +
    scale_fill_manual(values = c("2022" = "brown1", "2024" = "brown3", "2025"= "darkred"))+
    theme_classic() +
    theme(legend.position = "none")
)


# ANOVA TEST ACTIVE LAYER DEPTH YEARS----
anova_ald<- aov(thaw_av ~ year, data = ald_all_years)
summary(anova_ald)
# significant difference in active layer depth between years (p-value = 0.0213)


# CHECK ASSUMPTIONS----
# normality of residuals
plot(anova_ald)
shapiro.test(residuals(anova_ald))
hist(residuals(anova_ald))
# normal?

# homogeneity of variance
bartlett.test(thaw_av ~ year, data = ald_all_years)
# also fine