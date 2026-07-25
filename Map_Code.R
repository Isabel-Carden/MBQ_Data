#install.packages(c("osmdata", "sf", "ggplot2", "ggspatial"))
#install.packages("ggplot2")  # Skip this if already installed
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
#install.packages(c("ggplot2", "cowplot", "ggarrow", "osmdata"))

###CODE FOR BIG MAP###
# Load libraries
library(ggplot2)
library(osmdata)
library(ggspatial)
library(dplyr)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Moorea coordinates
Longitude <- -149.83
Latitude <- -17.53

# Create map
gg_southpacific <- ggplot(data = world) +
  geom_sf(fill = "darkgreen", color = "black", size = 0.2) +
  geom_point(aes(x = Longitude, y = Latitude), color = "red", size = 4, shape = 22, fill = "white") +
  annotate("text", x = Longitude, y = Latitude - 5, label = "Mo'orea", color = "red", fontface = "bold") +
  coord_sf(
    xlim = c(-180, -70),
    ylim = c(-45, 25),
    expand = FALSE,
    crs = st_crs(4326)
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),          # <-- Add this line
    axis.text.x = element_text(size = 12),        # <-- Optional if you want separate control
    axis.text.y = element_text(size = 12)  
  )

# Show map
print(gg_southpacific)

# Bounding box
moorea_bbox <- c(min_lon = -149.96, min_lat = -17.6, max_lon = -149.72, max_lat = -17.41)

# OSM query for Moorea as an island polygon
moorea_query <- opq(bbox = moorea_bbox) %>%
  add_osm_feature(key = "place", value = "island")

# Get OSM data
moorea_osm <- osmdata_sf(moorea_query)
moorea_poly <- moorea_osm$osm_multipolygons

# Site data
sites <- data.frame(
  site = c("JetSki", "Hilton", "Church", "West"),
  lon = c(-149.78711, -149.84003, -149.87055, -149.91492),
  lat = c(-17.47788, -17.48461, -17.49192, -17.49744)
) %>%
  arrange(desc(lon)) %>%  # Sort east to west
  mutate(site = factor(site, levels = site)) %>%  # Set factor levels in order
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Plot the map with colored points and a legend
moorea_map <- ggplot() +
  geom_sf(data = moorea_poly, fill = "darkgreen", color = "black") +
  geom_sf(data = sites, aes(color = site), shape = 20, size = 4) +  # site mapped to color
  scale_color_manual(values = c(
    "JetSki" = "#D55E00",
    "Hilton" = "#e69f00",
    "Church" = "#56B4E9",
    "West" = "#0072B2"
  )) +
  coord_sf(xlim = c(-149.96, -149.72), ylim = c(-17.60, -17.46), expand = FALSE) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  labs(
    color = "Site"  
  ) +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(color = "black", size = 12),
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 5)),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    axis.ticks = element_line(color = "black"),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

# Show the map
moorea_map

###COMBINED MAP###
combined_map <- plot_grid(
  gg_southpacific,
  moorea_map,
  labels = c("A", "B"),
  label_size = 15,
  label_x = 0.02,
  nrow = 2,
  rel_heights = c(1, 1.2)  # Adjust height ratio as needed
)

combined_map
ggsave("combined_map.png", plot = combined_map, width = 10, height = 12, dpi = 300)