#install.packages(c("osmdata", "sf", "ggplot2", "ggspatial"))
#install.packages("ggplot2")
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
#install.packages(c("ggplot2", "cowplot", "ggarrow", "osmdata"))
#install.packages("patchwork")

library(ggplot2)
library(sf)
library(ggspatial)
library(osmdata)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(ggarrow)
library(patchwork)

world <- ne_countries(scale = "medium", returnclass = "sf")

g_southpacific <- ggplot(data = world) +
  geom_sf(fill = "darkgreen", color = "black", size = 0.2) +
  geom_rect(xmin = -151.3, xmax = -148.5, ymin = -18.75, ymax = -16.15,
            fill = NA, colour = "red", size = 1) +
  annotate(geom = "text", x = -152, y = -22, label = "French Polynesia",
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-180, -110), ylim = c(-35, 50), expand = FALSE) +
  theme_minimal() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = "azure", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray80"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8)
  )


moorea <- opq("Moorea") %>%
  add_osm_feature(key = "place", value = "island") %>%
  osmdata_sf()

moorea_poly <- moorea$osm_multipolygons

g_moorea <- ggplot(data = moorea_poly) +
  geom_sf(fill = "darkgreen", color = "darkgreen") +
  coord_sf(xlim = c(-149.72, -149.96), ylim = c(-17.4, -17.6), expand = FALSE) +
  theme_minimal() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = "azure", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray80"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8)
  )

combined_plot <- g_southpacific +
  geom_curve(
    aes(x = -150.0, y = -17.0, xend = -149.84, yend = -17.5),
    arrow = arrow(type = "closed", length = unit(0.2, "inches")),
    color = "red", size = 1.5, curvature = 0.1
  ) +
  g_moorea +
  plot_layout(widths = c(2.2, 1))

print(combined_plot)
ggsave("south_pacific_moorea_map_with_red_arrow.png", plot = combined_plot,
       width = 12, height = 6, dpi = 300)

# Get OSM features for Moorea
moorea <- opq("Moorea") %>%
  add_osm_feature(key = "place", value = "island") %>%
  osmdata_sf()

# Extract polygon (if available)
moorea_poly <- moorea$osm_multipolygons

(gworld <- ggplot(data = world) +
    geom_sf(aes(fill = region_wb)) +
    geom_rect(xmin = -134, xmax = -155, ymin = -7, ymax = -27, 
              fill = NA, colour = "black", size = 1.5) +
    scale_fill_viridis_d(option = "plasma") +
    theme(panel.background = element_rect(fill = "azure"),
          panel.border = element_rect(fill = NA)))

(g_southpacific <- ggplot(data = world) +
    geom_sf(fill = "darkgreen", color = "black", size = 0.2) +
    geom_rect(
      xmin = -151.3, xmax = -148.5,
      ymin = -18.75, ymax = -16.15,
      fill = NA, colour = "red", size = 1
    )+
    annotate(geom = "text", x = -152, y = -22, label = "French Polynesia", 
             fontface = "italic", color = "grey22", size = 6) +
    
    # Zoomed-out view
    coord_sf(xlim = c(-180, -110), ylim = c(-35, 50), expand = FALSE) +
    
    # Theme + background
    theme_minimal() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "azure", color = NA),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      axis.title = element_blank(),
      axis.text = element_text(size = 8)
    ))

(g_moorea <- ggplot(data = moorea_poly) +
    geom_sf(fill = "darkgreen", color = "darkgreen") +
    coord_sf(xlim = c(-149.72, -149.96), ylim = c(-17.4, -17.6), expand = FALSE) +
    theme_minimal() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "azure", color = NA),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      axis.title = element_blank(),
      axis.text = element_text(size = 8)
    ))

library(ggplot2)
library(cowplot)
library(grid)

combined_plot <- plot_grid(
  g_southpacific, g_moorea,
  nrow = 1,
  rel_widths = c(2.2, 1),
  align = "h",    # Align horizontally
  axis = "tb"     # Align top and bottom margins
)

print(combined_plot)

# Save the combined plot
ggsave("south_pacific_moorea_map.png", plot = combined_plot_with_arrow,
       width = 12, height = 6, dpi = 300)


