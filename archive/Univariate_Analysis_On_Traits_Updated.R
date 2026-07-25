#univariate analysis
library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

#-------#

#Bartlett test (SAWW) - equal variances among groups? We hope so! Should fail to reject the null.
as.factor(univariate_trait_data$Site)
bartlett.test(SAWW ~ Site, data = univariate_trait_data) #p<0.05...time for transformations

univariate_trait_data$LOG_SAWW <- log10(univariate_trait_data$SAWW)
bartlett.test(LOG_SAWW ~ Site, data = univariate_trait_data)

# Shapiro test (SAWW) - is it normally distributed?
shapiro.test(univariate_trait_data$SAWW) #p<0.05; data not normal...log transformation
#univariate_trait_data$LOG_SAWW <- log10(univariate_trait_data$SAWW)
#shapiro.test(univariate_trait_data$LOG_SAWW) #p<0.05 still too low...try square root
#univariate_trait_data$SQSAWW <- sqrt(univariate_trait_data$SAWW)
#shapiro.test(univariate_trait_data$SQSAWW) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(SAWW ~ Site, data = univariate_trait_data) #p=0.056; NO significant difference between sites detected


#Results of Kruskal-Wallis
#p-value = 0.05597 not significant??? not significant by site but still a big contributor to the PCA
pairwise.wilcox.test(univariate_trait_data$SAWW, univariate_trait_data$Site, p.adjust.method = "BH")

# Check: print levels to confirm
print(levels(sum$Site))  # should be Jet_Ski, Hilton, Church, West

#graph
SAWW_uni_sum <- summarySE(univariate_trait_data, measurevar = "SAWW", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
SAWW_uni_sum$Site <- as.character(SAWW_uni_sum$Site)
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

#-------#

#Bartlett test (SAV)
as.factor(univariate_trait_data$Site)
bartlett.test(SAV ~ Site, data = univariate_trait_data) #p<0.05...time for transformations

univariate_trait_data$LOG_SAV <- log10(univariate_trait_data$SAV)
bartlett.test(LOG_SAV ~ Site, data = univariate_trait_data) #p<0.05
univariate_trait_data$SQ_SAV <- sqrt(univariate_trait_data$SAV)
bartlett.test(SQ_SAV ~ Site, data = univariate_trait_data) #p<0.05

# Shapiro test (SAV) - is it normally distributed?
shapiro.test(univariate_trait_data$SAV) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_SAV <- log10(univariate_trait_data$SAV)
shapiro.test(univariate_trait_data$LOG_SAWW) #p<0.05 still too los...try square root
#univariate_trait_data$SQSAWW <- sqrt(univariate_trait_data$SAWW)
#shapiro.test(univariate_trait_data$SQSAWW) #p<0.05...move on to non parametric approach

#Kurskal-Wallis (non-parametric version of ANOVA)
kruskal.test(SAV ~ Site, data = univariate_trait_data) #p=0.8284 so SAV not signficantly different across sites

#graph
SAV_uni_sum <- summarySE(univariate_trait_data, measurevar = "SAV", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
SAV_uni_sum$Site <- as.character(SAV_uni_sum$Site)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(SAV_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

#-------#

#Bartlett test (TH)
as.factor(univariate_trait_data$Site)
bartlett.test(TH ~ Site, data = univariate_trait_data) #p=0.1837 --> yay! move on to shapiro

# Shapiro test (TH) - is it normally distributed?
shapiro.test(univariate_trait_data$TH) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_TH <- log10(univariate_trait_data$TH)
shapiro.test(univariate_trait_data$LOG_TH) #p<0.05 still too los...try square root
univariate_trait_data$SQTH <- sqrt(univariate_trait_data$TH)
shapiro.test(univariate_trait_data$SQTH) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(TH ~ Site, data = univariate_trait_data) #p=3.572E-07...Significance for height!
pairwise.wilcox.test(univariate_trait_data$TH, univariate_trait_data$Site, p.adjust.method = "BH")

#graph
TH_uni_sum <- summarySE(univariate_trait_data, measurevar = "TH", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
TH_uni_sum$Site <- as.character(TH_uni_sum$Site)
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(TH_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

#-------#

#Bartlett test (TS)
as.factor(univariate_trait_data$Site)
bartlett.test(TS ~ Site, data = univariate_trait_data) #p=0.2872

# Shapiro test (TS) - is it normally distributed?
shapiro.test(univariate_trait_data$TS) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_TS <- log10(univariate_trait_data$TS)
shapiro.test(univariate_trait_data$LOG_TS) #p<0.05 still too los...try square root
univariate_trait_data$SQTS <- sqrt(univariate_trait_data$TS)
shapiro.test(univariate_trait_data$SQTS) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(TS ~ Site, data = univariate_trait_data) #p=0.0003329!

pairwise.wilcox.test(univariate_trait_data$TS, univariate_trait_data$Site, p.adjust.method = "BH")

#graph
TS_uni_sum <- summarySE(univariate_trait_data, measurevar = "TS", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
TS_uni_sum$Site <- as.character(TS_uni_sum$Site)
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(TS_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

#-------#

#Bartlett test (DWWW)
as.factor(univariate_trait_data$Site)
bartlett.test(DWWW ~ Site, data = univariate_trait_data) #p<0.05 
univariate_trait_data$LOG_DWWW <- log10(univariate_trait_data$DWWW)
bartlett.test(LOG_DWWW ~ Site, data = univariate_trait_data) #p=0.006125
univariate_trait_data$SQ_DWWW <- sqrt(univariate_trait_data$DWWW)
bartlett.test(SQ_DWWW ~ Site, data = univariate_trait_data) #p<0.05

# Shapiro test (DWWW) - is it normally distributed?
shapiro.test(univariate_trait_data$DWWW) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_DWWW <- log10(univariate_trait_data$DWWW)
shapiro.test(univariate_trait_data$LOG_DWWW) #p=n/a still too low...try square root
univariate_trait_data$SQDWWW <- sqrt(univariate_trait_data$DWWW)
shapiro.test(univariate_trait_data$SQDWWW) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(DWWW ~ Site, data = univariate_trait_data) #p=0.8177

#graph
DWWW_uni_sum <- summarySE(univariate_trait_data, measurevar = "DWWW", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
DWWW_uni_sum$Site <- as.character(DWWW_uni_sum$Site)
DWWW_uni_sum$Site <- factor(DWWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Check: print levels to confirm
print(levels(DWWW_uni_sum$Site))  # should be Jet_Ski, Hilton, Old_Church, West

#-------#

#Bartlett test (Toughness)
as.factor(univariate_trait_data$Site)
bartlett.test(Toughness ~ Site, data = univariate_trait_data) #p<0.05 

univariate_trait_data$LOG_Toughness <- log10(univariate_trait_data$Toughness)
bartlett.test(LOG_Toughness ~ Site, data = univariate_trait_data) #p<0.05
univariate_trait_data$SQ_Toughness <- sqrt(univariate_trait_data$Toughness)
bartlett.test(SQ_Toughness ~ Site, data = univariate_trait_data) #p<0.05

# Shapiro test (Toughness) - is it normally distributed?
shapiro.test(univariate_trait_data$Toughness) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_Toughness <- log10(univariate_trait_data$Toughness) #p<0.05
shapiro.test(univariate_trait_data$LOG_Toughness) 
univariate_trait_data$SQToughness <- sqrt(univariate_trait_data$Toughness)
shapiro.test(univariate_trait_data$SQToughness) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(Toughness ~ Site, data = univariate_trait_data) #p=1.5363-7...significance!
pairwise.wilcox.test(univariate_trait_data$Toughness, univariate_trait_data$Site, p.adjust.method = "BH")

Toughness_uni_sum <- summarySE(univariate_trait_data, measurevar = "Toughness", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
Toughness_uni_sum$Site <- as.character(Toughness_uni_sum$Site)
Toughness_uni_sum$Site <- factor(Toughness_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(Toughness_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

#-------#

#Bartlett test (WWTH)
as.factor(univariate_trait_data$Site)
bartlett.test(WWTH ~ Site, data = univariate_trait_data) #p<0.05 

univariate_trait_data$LOG_WWTH <- log10(univariate_trait_data$WWTH)
bartlett.test(LOG_WWTH ~ Site, data = univariate_trait_data) #p<0.05
univariate_trait_data$SQ_WWTH <- sqrt(univariate_trait_data$WWTH)
bartlett.test(SQ_WWTH ~ Site, data = univariate_trait_data) #p<0.05

# Shapiro test (WWTH) - is it normally distributed?
shapiro.test(univariate_trait_data$WWTH) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_Toughness <- log10(univariate_trait_data$WWTH) #p<0.05
shapiro.test(univariate_trait_data$LOG_WWTH) 
univariate_trait_data$SQWWTH <- sqrt(univariate_trait_data$WWTH)
shapiro.test(univariate_trait_data$SQWWTH) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(WWTH ~ Site, data = univariate_trait_data) #p=1.5363-7...significance!
pairwise.wilcox.test(univariate_trait_data$WWTH, univariate_trait_data$Site, p.adjust.method = "BH")

WWTH_uni_sum <- summarySE(univariate_trait_data, measurevar = "WWTH", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
WWTH_uni_sum$Site <- as.character(WWTH_uni_sum$Site)
WWTH_uni_sum$Site <- factor(WWTH_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(WWTH_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

#-------#

# Load/install libraries (only install once)
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")

library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)
library(patchwork)
library(cowplot)

# Create summary data (panel #1)
SAWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAWW", groupvars = "Site", na.rm = TRUE)
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

SAV_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAV", groupvars = "Site", na.rm = TRUE)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TH_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TH", groupvars = "Site", na.rm = TRUE)
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TS_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TS", groupvars = "Site", na.rm = TRUE)
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Plot function ----
create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.15, position = position_dodge(0.6)) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site") +
    xlab("Site") +
    ylab(ylab_text) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Adds 10% space above the bars
    theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 16),  # Increase site name font
      axis.text.y = element_text(size = 16)   # Increase y-axis number font
    )
  
}

# Generate plots
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "Surface Area to Wet Weight", fill_colors)
plot_SAV <- create_plot(SAV_uni_sum, "SAV", "Surface Area to Volume", fill_colors)
plot_TH <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)

# Extract legend from one of the plots
legend <- cowplot::get_legend(plot_SAWW + theme(legend.position = "right"))

# Combine plots into a 3x2 grid with the legend in the bottom-right quadrant
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),
  ncol = 2, nrow = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Calculate max y for dynamic label placement
max_SAWW <- max(SAWW_uni_sum$SAWW + SAWW_uni_sum$se)
max_SAV <- max(SAV_uni_sum$SAV + SAV_uni_sum$se)
max_TH <- max(TH_uni_sum$TH + TH_uni_sum$se)
max_TS <- max(TS_uni_sum$TS + TS_uni_sum$se)

# Define layout for patchwork
layout <- "
AB
CD
"

# Combine plots into a 3x2 grid with panel labels
library(patchwork)  # Assuming you're using patchwork for layout

# Define layout if needed
# layout <- "AABB\nCCDD"  # Example only

# Combine plots into a 2x2 grid with panel labels
combined_plot <- (plot_SAWW + plot_SAV + plot_TH + plot_TS) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',  # Adds panel labels A, B, C, D
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      legend.position = "bottom",
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  )

# Load necessary libraries
library(ggplot2)
library(patchwork)

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Plot function
create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.15, position = position_dodge(0.6)) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site") +
    xlab("Site") +
    ylab(ylab_text) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Adjust y-axis scale
    theme_minimal(base_size = 16) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Assuming 'univariate_trait_data' is your dataset
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "SA:WW", fill_colors)
plot_SAV <- create_plot(SAV_uni_sum, "SAV", "SA:V", fill_colors)
plot_TH <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)

# Combine plots into a 3x2 grid with a common legend
library(patchwork)
library(ggplot2)

# Combine the plots with layout and shared legend on the right
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),  # Empty space filler
  ncol = 2, nrow = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Add panel tags (A-E) and adjust tag appearance
combined_plot <- combined_plot +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      plot.tag.position = c(0, 1)
    )
  )

# Display the combined plot
print(combined_plot)

# Save the combined plot to file
ggsave("combined_plot_with_labels_and_legend_no_thickness.png",
       combined_plot, width = 12, height = 10, dpi = 300)

#----------#

# Create summary data (panel #2)
DWWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "DWWW", groupvars = "Site", na.rm = TRUE)
DWWW_uni_sum$Site <- factor(DWWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

Toughness_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "Toughness", groupvars = "Site", na.rm = TRUE)
Toughness_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

WWTH_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "WWTH", groupvars = "Site", na.rm = TRUE)
WWTH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Plot function ----
create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.15, position = position_dodge(0.6)) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site") +
    xlab("Site") +
    ylab(ylab_text) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Adds 10% space above the bars
    theme_minimal(base_size = 16) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate plots
plot_DWWW <- create_plot(DWWW_uni_sum, "DWWW", "Dry Weight to Wet Weight", fill_colors)
plot_Toughness <- create_plot(Toughness_uni_sum, "Toughness", "Toughness", fill_colors)
plot_WWTH <- create_plot(WWTH_uni_sum, "WWTH", "Wet Weight to Thallus Height", fill_colors)

# Extract legend from one of the plots
legend <- cowplot::get_legend(plot_DWWW + theme(legend.position = "right"))

# Combine plots into a 3x2 grid with the legend in the bottom-right quadrant
combined_plot <- wrap_plots(
  plot_DWWW, plot_Toughness, plot_WWTH, ggplot() + theme_void(),
  ncol = 2, nrow = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Calculate max y for dynamic label placement
max_DWWW <- max(DWWW_uni_sum$DWWW + DWWW_uni_sum$se)
max_Toughness <- max(Toughness_uni_sum$Toughness + Toughness_uni_sum$se)
max_WWTH <- max(WWTH_uni_sum$WWTH + WWTH_uni_sum$se)

# Define layout for patchwork
layout <- "
AB
C
"

# Combine plots into a 3x2 grid with panel labels
library(patchwork)  # Assuming you're using patchwork for layout

# Define layout if needed
# layout <- "AABB\nCCDD"  # Example only

# Combine plots into a 2x2 grid with panel labels
combined_plot <- (plot_DWWW + plot_Toughness + plot_WWTH) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',  # Adds panel labels A, B, C, D
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      legend.position = "bottom",
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  )

# Load necessary libraries
library(ggplot2)
library(patchwork)

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Plot function
create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.15, position = position_dodge(0.6)) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site") +
    xlab("Site") +
    ylab(ylab_text) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Adjust y-axis scale
    theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 16),  # Increase site name font
      axis.text.y = element_text(size = 16)   # Increase y-axis number font
    )
  

# Assuming 'univariate_trait_data' is your dataset
plot_DWWW <- create_plot(DWWW_uni_sum, "DWWW", "DW:WW", fill_colors)
plot_Toughness <- create_plot(SAV_uni_sum, "Toughness", "Toughness", fill_colors)
plot_WWTH <- create_plot(TH_uni_sum, "WWTH", "WW:TH", fill_colors)

# Combine plots into a 3x2 grid with a common legend
library(patchwork)
library(ggplot2)

# Combine the plots with layout and shared legend on the right
combined_plot <- wrap_plots(
  plot_DWWW, plot_Toughness, plot_WWTH, ggplot() + theme_void(),  # Empty space filler
  ncol = 2, nrow = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Add panel tags (A-E) and adjust tag appearance
combined_plot <- combined_plot +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      plot.tag.position = c(0, 1)
    )
  )

# Display the combined plot
print(combined_plot)

# Combine plots into a 2x2 grid with panel labels
comb_plot <- (plot_SAWW + plot_SAV + plot_TH + plot_TS + plot_DWWW + plot_Toughness) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',  # Adds panel labels A, B, C, D
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      legend.position = "bottom",
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  )
print(comb_plot)

# Save the combined plot to file
ggsave("combined_plot_with_labels_and_legend.png",
       comb_plot, width = 12, height = 10, dpi = 300)

###COMBINED PLOT###
# Fix: Use consistent Site levels
site_levels <- c("JetSki", "Hilton", "Church", "West")

SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = site_levels)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = site_levels)
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = site_levels)
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = site_levels)
DWWW_uni_sum$Site <- factor(DWWW_uni_sum$Site, levels = site_levels)
Toughness_uni_sum$Site <- factor(Toughness_uni_sum$Site, levels = site_levels)

create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.1, position = position_dodge(0.8)) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site") +
    xlab("Site") +
    ylab(ylab_text) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 16),  # Increase site name font
      axis.text.y = element_text(size = 16)   # Increase y-axis number font
    )


# Define color palette
fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

# Generate each plot
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "SA:WW", fill_colors)
plot_SAV  <- create_plot(SAV_uni_sum, "SAV", "SA:V", fill_colors)
plot_TH   <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS   <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)
plot_DWWW <- create_plot(DWWW_uni_sum, "DWWW", "DW:WW", fill_colors)
plot_Toughness <- create_plot(Toughness_uni_sum, "Toughness", "Toughness", fill_colors)

library(patchwork)

# Combine into a 3x2 grid with labels A–F
combined_plot <- (
  plot_SAWW + plot_SAV + plot_TH +
    plot_TS + plot_DWWW + plot_Toughness
) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',  # This will label A–F
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  )

combined_plot

# Save as PNG
ggsave("univariate_combined_traits.png",
       combined_plot, width = 12, height = 12, dpi = 300)

###DENSITY PLOTS###
##SAV##
ggplot(data = univariate_trait_data, aes(x=SAV)) + geom_histogram(binwidth=.5)
# qplot(dat$rating, binwidth=.5)

# Draw with black outline, white fill
ggplot(data = univariate_trait_data, aes(x=SAV)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

# Density curve
ggplot(data = univariate_trait_data, aes(x=SAV)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(data = univariate_trait_data, aes(x=SAV)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
str(univariate_trait_data)

ggplot(data = univariate_trait_data, aes(x=SAV)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(SAV, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

##SAWW##
ggplot(data = univariate_trait_data, aes(x=SAWW)) + geom_histogram(binwidth=.5)
# qplot(dat$rating, binwidth=.5)

# Draw with black outline, white fill
ggplot(data = univariate_trait_data, aes(x=SAWW)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

# Density curve
ggplot(data = univariate_trait_data, aes(x=SAWW)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(data = univariate_trait_data, aes(x=SAWW)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
str(univariate_trait_data)

ggplot(data = univariate_trait_data, aes(x=SAWW)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(SAWW, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

##DWWW##
ggplot(data = univariate_trait_data, aes(x=DWWW)) + geom_histogram(binwidth=.005)

# Draw with black outline, white fill
ggplot(data = univariate_trait_data, aes(x=DWWW)) +
  geom_histogram(binwidth=.005, colour="black", fill="white")

# Density curve
ggplot(data = univariate_trait_data, aes(x=DWWW)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(data = univariate_trait_data, aes(x=DWWW)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.005,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
str(univariate_trait_data)

ggplot(data = univariate_trait_data, aes(x=DWWW)) +
  geom_histogram(binwidth=.005, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(DWWW, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

###UPDATED COMBINED PLOTS###
library(dplyr)

# SAWW
SAWW_uni_sum <- SAWW_uni_sum %>%
  mutate(Letter = case_when(
    Site == "JetSki" ~ "a",
    Site == "Hilton" ~ "b",
    Site == "Church" ~ "bc",
    Site == "West" ~ "a"
  ))

# SAV — no significant differences
SAV_uni_sum <- SAV_uni_sum %>%
  mutate(Letter = "")

# TH
TH_uni_sum <- TH_uni_sum %>%
  mutate(Letter = case_when(
    Site == "JetSki" ~ "c",
    Site == "Hilton" ~ "a",
    Site == "Church" ~ "ab",
    Site == "West" ~ "b"
  ))

# TS
TS_uni_sum <- TS_uni_sum %>%
  mutate(Letter = case_when(
    Site == "JetSki" ~ "b",
    Site == "Hilton" ~ "a",
    Site == "Church" ~ "a",
    Site == "West" ~ "b"
  ))

# DWWW — no significant differences
DWWW_uni_sum <- DWWW_uni_sum %>%
  mutate(Letter = "")

# Toughness
Toughness_uni_sum <- Toughness_uni_sum %>%
  mutate(Letter = case_when(
    Site == "JetSki" ~ "b",
    Site == "Hilton" ~ "a",
    Site == "Church" ~ "a",
    Site == "West" ~ "a"
  ))

create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.8, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se,
                      ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.1, position = position_dodge(0.8)) +
    # Add significance letters above bars (skip blanks)
    geom_text(aes(label = Letter,
                  y = .data[[yvar]] + se + 0.05 * max(.data[[yvar]])),
              vjust = 0, size = 5, na.rm = TRUE) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site", y = ylab_text, x = "Site") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
}

fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "SA:WW", fill_colors)
plot_SAV  <- create_plot(SAV_uni_sum, "SAV", "SA:V", fill_colors)
plot_TH   <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS   <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)
plot_DWWW <- create_plot(DWWW_uni_sum, "DWWW", "DW:WW", fill_colors)
plot_Toughness <- create_plot(Toughness_uni_sum, "Toughness", "Toughness", fill_colors)

library(patchwork)

combined_plot <- (
  plot_SAWW + plot_SAV + plot_TH +
    plot_TS + plot_DWWW + plot_Toughness
) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    theme = theme(legend.position = "bottom")
  )
combined_plot
# Save at a wider aspect ratio
ggsave("combined_plot_wide.png", combined_plot, width = 18, height = 8, dpi = 300)

