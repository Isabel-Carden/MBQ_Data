library(ggplot2)
library(Rmisc)
library(dplyr)
library(googlesheets4)
library(patchwork)

#Herbivory Assay
herb_data <- read_sheet("https://docs.google.com/spreadsheets/d/169_iAfJME3vntApKnjd_MLyABwFc-VSpz4YJqbX4QUw/edit?gid=0#gid=0")

herb_sum <- summarySE(herb_data, measurevar = "per_loss_hr", groupvars = "Site", na.rm = TRUE)

# Clean site names
herb_sum$Site <- gsub(" ", "_", herb_sum$Site)
herb_sum$Site <- factor(herb_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Post-hoc letters
herb_sum <- herb_sum %>%
  mutate(Letters = case_when(
    Site == "JetSki" ~ "ab",
    Site == "Hilton" ~ "a",
    Site == "Church" ~ "a",
    Site == "West" ~ "b"
  ))

# Letter placement
herb_sum$label_y <- herb_sum$per_loss_hr + herb_sum$se + 0.05 * max(herb_sum$per_loss_hr + herb_sum$se)

# Color palette
fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

# Herbivory plot
herb_plot <- ggplot(herb_sum, aes(x = Site, y = per_loss_hr, fill = Site)) +
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  geom_errorbar(aes(ymin = per_loss_hr - se, ymax = per_loss_hr + se),
                width = 0.15, size = 0.8) +
  geom_text(aes(label = Letters, y = label_y),
            size = 7, fontface = "bold") +
  scale_fill_manual(values = fill_colors) +
  scale_x_discrete(labels = c("JetSki", "Hilton", "Church", "West")) +
  xlab("Site") +
  ylab("Percent Weight Lost per Hour") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

#Nutrient Bioassay
grow_out_data <- read_sheet("https://docs.google.com/spreadsheets/d/1rsR_sxOwSnuQDyrUtvjgICQtOJ8asLwo0p5N4bFV3zY/edit?gid=0#gid=0")

growth_sum <- summarySE(grow_out_data, measurevar = "Percent_Change_Weight", groupvars = "Site_Name", na.rm = TRUE)

# Clean site names
growth_sum$Site_Name <- gsub(" ", "_", growth_sum$Site_Name)

# Replace Old_Church with Church
growth_sum$Site_Name[growth_sum$Site_Name == "Old_Church"] <- "Church"

# Convert to factor with correct order
growth_sum$Site_Name <- factor(growth_sum$Site_Name, levels = c("JetSki", "Hilton", "Church", "West"))

# Post-hoc letters
growth_sum <- growth_sum %>%
  mutate(Letters = case_when(
    Site_Name == "JetSki" ~ "b",
    Site_Name == "Hilton" ~ "ab",
    Site_Name == "Church" ~ "b",
    Site_Name == "West" ~ "a"
  ))

# Letter placement
growth_sum$label_y <- growth_sum$Percent_Change_Weight + growth_sum$se + 0.05 * max(growth_sum$Percent_Change_Weight + growth_sum$se)

# Growth plot
growth_plot <- ggplot(growth_sum, aes(x = Site_Name, y = Percent_Change_Weight, fill = Site_Name)) +
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  geom_errorbar(aes(ymin = Percent_Change_Weight - se, ymax = Percent_Change_Weight + se),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = Letters, y = label_y),
            size = 7, fontface = "bold") +
  scale_fill_manual(values = fill_colors) +
  scale_x_discrete(labels = c("JetSki", "Hilton", "Church", "West")) +
  xlab("Site") +
  ylab("Growth (% change in biomass / 6 days)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

#Combined Plots
combined_panel <- herb_plot + growth_plot + plot_layout(ncol = 2, guides = "collect")

# Show combined panel
print(combined_panel)

# Save combined panel
ggsave("herbivory_growth_panel.png", combined_panel, width = 14, height = 6, dpi = 300)

