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


# CLEAN DATA---
# change year to as factor in snowmelt_dates
snowmelt_dates <- snowmelt_dates %>% mutate(year = as.factor(year))



# PLOT DATA SNOWMELT DATA ONLY----
(snowmelt_dates_fig<-ggplot(snowmelt_dates, aes(x = year, y = doy, colour = year )) +
  geom_boxplot() +
  labs(x = "Year", y = "DoY 100% Snow Free") +
  theme_classic() +
  theme(legend.position = "none")
  
)

# only one in 2023 as doy was same for all subplots

ggsave("figures/RQ1/snowmelt_dates_boxplot.png", snowmelt_dates_fig, width = 6, height = 4)

# PLOT DATA ALD & SNOWMELT----
(ggplot(ald_snowmelt, aes(x = doy, y = thaw_av)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "DoY 100% Snow Free", y = "Active Layer Depth (cm)") +
  theme_classic() +
  ggtitle("Relationship between Snowmelt Date and Active Layer Depth")
)

# QUICK ANALYSIS OF ABOVE FIGURE----
# linear model 
ald_snowmelt_lm <- lm(thaw_av ~ doy, data = ald_snowmelt)
summary(ald_snowmelt_lm) 
# p value 0.9538 - no significant relationship between snowmelt date and ALD

# check assumptions
plot(ald_snowmelt_lm)
shapiro.test(residuals(ald_snowmelt_lm)) # p value would suggest normally distributed 
hist(residuals(ald_snowmelt_lm)) 
