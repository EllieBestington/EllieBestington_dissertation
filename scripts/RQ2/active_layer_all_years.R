# CHANGE IN ACTIVE LAYER DEPTH AND BIOMASS ----
# 19/01/2026

# LOAD LIBRARIES----
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# won't work now as data frame for ald has changed in GitHub repo, but leaving code here for when we have ald data specific for core sites 

# LOAD DATA----
QHI_root_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")
ald_all_years<-read_excel("datasets/QHI_roots_data/active_layer_thaw_combined.xlsx")

# CLEAN DATA----
QHI_root_data <- QHI_root_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
  # remove row 32
  filter(row_number() != 32)

# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation 

# NEW DATA FRAME CHANGE IN ACTIVE LAYER DEPTH 2022 TO 2025----
ald_change <- ald_all_years %>% 
  filter(year %in% c(2022, 2025)) %>%
  select(site, sub_plot, year, thaw_av) %>%
  pivot_wider(names_from = year, values_from = thaw_av, names_prefix = "year_") %>%
  mutate(change_in_ALD_2022_2025 = year_2025 - year_2022) %>%
  select(site, sub_plot, change_in_ALD_2022_2025)

# PLOT CHANGES----
(ald_change_plot <- ggplot(ald_change, aes(x = sub_plot, y = change_in_ALD_2022_2025, fill = site)) +
  geom_point() +
  labs(title = "Change in Active Layer Depth from 2022 to 2025",
       x = "Site",
       y = "Change in Active Layer Depth (cm)") +
  theme_classic() +
  theme(legend.position = "none")
)


# NEW DATA FRAME CHANGE IN ROOT BIOMASS 2023 TO 2024----
root_biomass_change <- QHI_root_data %>% 
  filter(year %in% c(2023, 2024)) %>%
  group_by(subplot, year, core_ID) %>%
  summarise(rootmass_bulkdensity = sum(rootmass_bulkdensity, na.rm = TRUE), 
            .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = rootmass_bulkdensity, names_prefix = "year_") %>%
  mutate(change_in_root_biomass_2023_2024 = year_2024 - year_2023) %>%
  select(subplot, core_ID, change_in_root_biomass_2023_2024)

# note: only have 3 subplots picked in 2024, so only 9 changes in root biomass available (ongoing lab analysis)


# COMBINE DATA FRAMES---- 
colnames(ald_change)[2] <- "subplot"

# combine ald_change with root_biomass_change
combined_change <- merge(ald_change, root_biomass_change, by = "subplot")
# remove rows with NA values
combined_change <- na.omit(combined_change)

# PLOT COMBINED CHANGES----
(combined_change_plot <- ggplot(combined_change, aes(x = change_in_ALD_2022_2025, y = change_in_root_biomass_2023_2024 )) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Change in Active Layer Depth vs Change in Root Biomass Density",
       x = "Change in Active Layer Depth (cm)",
       y = "Change in Root Biomass Density (g cm⁻³)") +
  theme_classic()
)
# when active layer decreases (more permafrost), root biomass also decreases?
# when active layer increases (less permafrost), root biomass increases?
# The positive relationship suggests that deeper active layers may provide more accessible soil volume for root growth, leading to increased root biomass. 
# Conversely, when the active layer becomes shallower (potentially due to colder conditions, more permafrost or less thaw), root biomass tends to decrease, possibly due to restricted growing space or root die-back in frozen layers


# ANALYSIS----
# NOTE: hard to analyse with only 9 data points, but we can run a linear model to see if there is a significant relationship/value of the blue line 

lm_change <- lm(change_in_root_biomass_2023_2024 ~ change_in_ALD_2022_2025, data = combined_change)
summary(lm_change)

# check assumptions----
plot(lm_change)
shapiro.test(residuals(lm_change)) # normality
hist(residuals(lm_change)) # histogram of residuals
# with so few data points hard to meet assumptions, but the trend is there and worth exploring in furture when lab analysis is complete for more subplots in 2024
