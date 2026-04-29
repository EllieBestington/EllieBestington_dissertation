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

# remove mix
QHI_combined_data_P3 <- QHI_combined_data_P3 %>%
  filter(community != "Mix")

# VIOLIN PLOT: DEPTH, BIOMASS, YEAR----
(ggplot(QHI_combined_data_P3, aes(x = rootmass_bulkdensity, y = max_depth_increment)) +
  geom_violin(aes(fill = community), 
              trim = FALSE, 
              scale = "width",
              alpha = 0.7) +
  facet_grid(year~ community) +
  scale_y_reverse() +
  scale_x_continuous(position = "top") +
  labs(
    x = "Root Biomass (g/cm³)",
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


# BARCHART DEPTH, BIOMASS, YEAR----

# First, make sure depth is a factor
QHI_combined_data_P3$max_depth_increment <- factor(QHI_combined_data_P3$max_depth_increment)

# Create bar chart
(ggplot(QHI_combined_data_P3, aes(x = rootmass_bulkdensity, y = max_depth_increment, fill = community)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_grid(year~ community) +
  scale_y_discrete(limits = rev) +  # Reverse so shallowest depth is on top
  labs(
    x = "Average Root Biomass (g/cm³)",
    y = "Depth (cm)",
    fill = "Community"
  ) +
    scale_fill_manual(values = c("Graminoid" = "#E69F00",
                                 "Shrub" = "#009E73"))+
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
)

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


# FIGURE P3s AND NO MIX COMMUNITY only-----
(community_biomass_year <- QHI_combined_data_P3 %>%
   filter(year %in% c(2023, 2024)) %>%
   filter(community != "Mix") %>%
   ggplot(aes(x = community, y = rootmass_bulkdensity, fill = as.factor(year))) +
   geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
   labs(
     x = "Community",
     y = "Root Biomass Density (g cm⁻³)",
     fill = "Year") +
   scale_fill_manual(values = c("2023" = "brown3", "2024" = "steelblue")) +
   scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
   theme_classic()
)


