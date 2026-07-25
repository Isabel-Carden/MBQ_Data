#load libraries#
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)
library(patchwork)
library(cowplot)

gs4_deauth()

#load google sheets#
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

###TRAIT ANALYSES###
#SAWW#
#Bartlett test (SAWW) - equal variances among groups? We hope so! Should fail to reject the null.
as.factor(univariate_trait_data$Site)
bartlett.test(SAWW ~ Site, data = univariate_trait_data) #p<0.05...time for transformations
univariate_trait_data$LOG_SAWW <- log10(univariate_trait_data$SAWW)
bartlett.test(LOG_SAWW ~ Site, data = univariate_trait_data)
# Shapiro test (SAWW) - is it normally distributed?
shapiro.test(univariate_trait_data$SAWW) #p<0.05; data not normal...log transformation
#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(SAWW ~ Site, data = univariate_trait_data) #p=0.056; NO significant difference between sites detected
#Results of Kruskal-Wallis
pairwise.wilcox.test(univariate_trait_data$SAWW, univariate_trait_data$Site, p.adjust.method = "BH")
#graph
SAWW_uni_sum <- summarySE(univariate_trait_data, measurevar = "SAWW", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
SAWW_uni_sum$Site <- as.character(SAWW_uni_sum$Site)
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

#SAV#
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
#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(SAV ~ Site, data = univariate_trait_data) #p=0.8284 so SAV not signficantly different across sites
#graph
SAV_uni_sum <- summarySE(univariate_trait_data, measurevar = "SAV", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
SAV_uni_sum$Site <- as.character(SAV_uni_sum$Site)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

#TH#
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
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

#TS#
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
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

#DWWW#
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

#Toughness#
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
Toughness_uni_sum$Site <- factor(Toughness_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

###SUMMARY PLOTS###
SAWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAWW", groupvars = "Site", na.rm = TRUE)
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

SAV_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAV", groupvars = "Site", na.rm = TRUE)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TH_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TH", groupvars = "Site", na.rm = TRUE)
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TS_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TS", groupvars = "Site", na.rm = TRUE)
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

DWWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "DWWW", groupvars = "Site", na.rm = TRUE)
DWWW_uni_sum$Site <- factor(DWWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

Toughness_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "Toughness", groupvars = "Site", na.rm = TRUE)
Toughness_uni_sum$Site <- factor(Toughness_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Fix inconsistent Site names
univariate_trait_data$Site <- recode(univariate_trait_data$Site,
                                     "Jet_Ski" = "JetSki",
                                     "Old_Church" = "Church")

#Set Site factor order
site_levels <- c("JetSki", "Hilton", "Church", "West")
univariate_trait_data$Site <- factor(univariate_trait_data$Site, levels = site_levels)

#Define a function to summarize and plot each trait
create_trait_plot <- function(trait_name, y_label, fill_colors) {
  # Summarize
  trait_sum <- summarySE(univariate_trait_data, measurevar = trait_name, groupvars = "Site", na.rm = TRUE)
  trait_sum$Site <- factor(trait_sum$Site, levels = site_levels)
  
  # Plot
  p <- ggplot(trait_sum, aes(x = Site, y = .data[[trait_name]], fill = Site)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(ymin = .data[[trait_name]] - se, ymax = .data[[trait_name]] + se),
                  width = 0.15, position = position_dodge(0.6)) +
    scale_fill_manual(values = fill_colors) +
    labs(y = y_label, x = "Site") +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold"),
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid = element_blank()
    )
  return(p)
}

#Choose fill colors
fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

#Build individual plots
plot_SAWW <- create_trait_plot("SAWW", "Surface Area to Wet Weight", fill_colors)
plot_SAV <- create_trait_plot("SAV", "Surface Area to Volume", fill_colors)
plot_TH <- create_trait_plot("TH", "Thallus Height", fill_colors)
plot_TS <- create_trait_plot("TS", "Tensile Strength", fill_colors)
plot_DWWW <- create_trait_plot("DWWW", "Dry Weight to Wet Weight", fill_colors)
plot_Toughness <- create_trait_plot("Toughness", "Toughness", fill_colors)

#Combine plots using patchwork
combined_plot <- (plot_SAWW | plot_SAV) /
  (plot_TH | plot_TS) /
  (plot_DWWW | plot_Toughness) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

#Show plot (especially when running script)
if (interactive()) {
  print(combined_plot)
}

#Save to file
ggsave("combined_plot_new.png", plot = combined_plot, width = 12, height = 10, dpi = 300)
