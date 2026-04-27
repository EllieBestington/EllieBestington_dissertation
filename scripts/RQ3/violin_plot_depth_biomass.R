#PLOTs FOR PRESENTATION 
# 23/04/26


# LIBRARIES----
library(tidyverse)
library(ggplot2)
library(readxl)

# LOAD DATA----
QHI_combined_data <- read_excel("datasets/QHI_roots_data/QHI_combined_data.xlsx")

# CLEAN DATA----
QHI_combined_data <- QHI_combined_data %>%
  mutate(rootmass_bulkdensity = as.numeric(rootmass_bulkdensity)) %>%
  filter(!is.na(rootmass_bulkdensity)) %>%
  # remove row 32
  filter(row_number() != 32)

# removed outlier row 32 which had rootmass_bulkdensity of 5.98 g/cm3, likely a data entry error 
# see notebook for further laboratory explanation 


# FILTER P3 ONLY----
QHI_combined_data_P3 <- QHI_combined_data %>%
  filter(core_ID == "P3")

# Convert to numeric if needed
QHI_combined_data_P3$max_depth_increment <- as.numeric(QHI_combined_data_P3$max_depth_increment)


# VIOLIN PLOT: DEPTH, BIOMASS, YEAR----
QHI_mirrored <- QHI_combined_data_P3 %>%
  bind_rows(
    QHI_combined_data_P3 %>% 
      mutate(rootmass_bulkdensity = -rootmass_bulkdensity)  # Mirror to negative side
  )

# remove mix community - not focus for presentation 
QHI_mirrored <- QHI_mirrored %>%
  filter(community != "Mix")

# Plot with centered violins
(ggplot(QHI_mirrored, aes(x = rootmass_bulkdensity, y = max_depth_increment)) +
  geom_violin(aes(fill = community), 
              trim = FALSE, 
              scale = "width",
              alpha = 0.7) +
  facet_grid(year~ community) +
  scale_y_reverse ()+
  scale_x_continuous(position = "top") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.5) +
  labs(x = "Root Biomass (g/cm³)",
    y = "Depth (cm)",
    fill = "Community"
  ) +
  scale_fill_manual(values = c("Graminoid" = "#E69F00",
                                   "Shrub" = "#009E73"))+
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
)



