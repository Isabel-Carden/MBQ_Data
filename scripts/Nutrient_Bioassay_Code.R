#install.packages("Rmisc")

library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggpubr)
library(Rmisc)
library(ggplot2)
library(multcompView)

gs4_deauth()
grow_out_data <- read_sheet("https://docs.google.com/spreadsheets/d/1rsR_sxOwSnuQDyrUtvjgICQtOJ8asLwo0p5N4bFV3zY/edit?gid=0#gid=0")

# Shapiro test - is it normally distributed?
shapiro.test(grow_out_data$Percent_Change_Weight)

#qqplot
model1<-aov(Percent_Change_Weight~Site_Name, data= grow_out_data)
qqPlot(model1) 

#Bartlett test - equal variances among groups?
as.factor(grow_out_data$Site_Name)
bartlett.test(Percent_Change_Weight ~ Site_Name, data = grow_out_data)

#one-way ANOVA
# Compute the analysis of variance
res.aov <- aov(Percent_Change_Weight ~ Site_Name, data = grow_out_data)
# Summary of the analysis
summary(res.aov)

#Post-Hoc analysis
TukeyHSD(res.aov)

#Bar Graphs
shapiro.test(grow_out_data$Percent_Change_Weight)
model2 <- aov(Percent_Change_Weight ~ Site_Name, data = grow_out_data)
qqPlot(model2)

as.factor(grow_out_data$Percent_Change_Weight)
bartlett.test(Percent_Change_Weight ~ Site_Name, data = grow_out_data) 

anova_result <- aov(Percent_Change_Weight ~ Site_Name, data = grow_out_data)
summary(anova_result)

#Tukey test
TukeyHSD(anova_result)

# Summarize data
summary_data <- summarySE(
  grow_out_data, 
  measurevar = "Percent_Change_Weight", 
  groupvars = "Site_Name"
)

# Post hoc letters
anova_result <- aov(Percent_Change_Weight ~ Site_Name, data = grow_out_data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
tukey_result
tukey_letters <- multcompLetters4(anova_result, tukey_result)
summary_data$Letters <- tukey_letters$Site_Name[match(summary_data$Site_Name, names(tukey_letters$Site_Name))] %>% 
  sapply(function(x) x$Letters)

# Reorder sites
site_levels <- c("JetSki", "Hilton", "Church", "West")
summary_data$Site_Name <- factor(summary_data$Site_Name, levels = site_levels)
summary_data <- summary_data %>%
  mutate(xpos = as.numeric(Site_Name))

# Post-hoc letters
summary_data <- summary_data %>%
  mutate(Letters = case_when(
    Site_Name == "JetSki" ~ "b",
    Site_Name == "Hilton" ~ "ab",
    Site_Name == "Church" ~ "b",
    Site_Name == "West" ~ "a"
  ))

# Color palette
fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

# Plot
final_plot <- ggplot(summary_data, aes(x = xpos, y = Percent_Change_Weight, fill = Site_Name)) +
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  geom_errorbar(
    aes(ymin = Percent_Change_Weight - se, ymax = Percent_Change_Weight + se),
    width = 0.15, size = 0.8
  ) +
  # Post-hoc letters, consistent distance above each error bar
  geom_text(
    aes(label = Letters, y = Percent_Change_Weight + se + 1.0 * se),
    size = 8, fontface = "bold"
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_x_continuous(
    name = "Site",
    breaks = 1:4,
    labels = c("JetSki", "Hilton", "Church", "West"),
    expand = expansion(add = c(0.25, 0.25))  # space between outer bars & y-axis
  ) +
  scale_y_continuous(
    name = "Growth (% change in biomass/6 days)",
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Show plot
print(final_plot)

#ggsave(filename = "Nutrient_Bioassay_Plot.png", plot = final_plot,       width = 8, height = 6, dpi = 300)

