# Load necessary libraries
library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)

# Access your Google Sheet
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

# Make sure Site is a factor
univariate_trait_data$Site <- as.factor(univariate_trait_data$Site)

##SAWW##
# Check equal variances (Bartlett’s test)
bartlett.test(SAWW ~ Site, data = univariate_trait_data) #p=1.689e-15

#glm#
model_SAWW <- glm(SAWW ~ Site,
             family = Gamma(link = "log"),
             data = univariate_trait_data) #log bc response variable is continuous and strictly positive
summary(model_SAWW)
anova(model_SAWW, test = "Chisq")

install.packages("emmeans")  # only if needed
library(emmeans)
# Estimated marginal means for each Site
em <- emmeans(model_SAWW, ~ Site)

# Pairwise comparisons with Tukey adjustment
pairs(em, adjust = "tukey")

##SAV##
# Check equal variances (Bartlett’s test)
bartlett.test(SAV ~ Site, data = univariate_trait_data) #p=8.36e-9

#glm#
model_SAV <- glm(SAV ~ Site,
                family = inverse.gaussian(link = "log"),  # change family
                data = univariate_trait_data)

# Look at the summary
summary(model_SAV)
anova(model_SAV, test = "Chisq")

##DWWW##
# Check equal variances (Bartlett’s test)
bartlett.test(DWWW ~ Site, data = univariate_trait_data) #p=3.038e-10

#glm#
model_DWWW <- glm(DWWW ~ Site,
                 family = inverse.gaussian(link = "log"),  # change family
                 data = univariate_trait_data)

# Look at the summary
summary(model_DWWW)
anova(model_DWWW, test = "Chisq")


###Combined Plots###
#univariate analysis
library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)
library(patchwork)
library(cowplot)
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

# Load/install libraries (only install once)
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")

# Create summary data (panel #1)
SAWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAWW", groupvars = "Site", na.rm = TRUE)
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

SAV_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAV", groupvars = "Site", na.rm = TRUE)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TH_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TH", groupvars = "Site", na.rm = TRUE)
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TS_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TS", groupvars = "Site", na.rm = TRUE)
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

Toughness_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "Toughness", groupvars = "Site", na.rm = TRUE)
Toughness_uni_sum$Site <- factor(Toughness_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

DWWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "DWWW", groupvars = "Site", na.rm = TRUE)
DWWW_uni_sum$Site <- factor(DWWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

create_plot <- function(data, yvar, ylab_text, fill_colors) {
  
  # Ensure correct Site order
  data$Site <- factor(data$Site, levels = c("JetSki", "Hilton", "Church", "West"))
  
  # Add label positions above error bars
  data$label_y <- data[[yvar]] + data$se + 0.15 * max(data[[yvar]] + data$se)
  
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", width = 0.8, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.2) +
    geom_text(aes(label = Letter, y = label_y), size = 7, fontface = "bold") +
    scale_fill_manual(values = fill_colors) +
    xlab("Site") + ylab(ylab_text) +
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
}


# SAWW
SAWW_uni_sum <- SAWW_uni_sum %>%
  mutate(Letter = case_when(
    Site == "JetSki" ~ "a",
    Site == "Hilton" ~ "c",
    Site == "Church" ~ "bc",
    Site == "West" ~ "ab"
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

# Toughness — now labeled E
Toughness_uni_sum <- Toughness_uni_sum %>%
  mutate(Letter = case_when(
    Site == "JetSki" ~ "b",
    Site == "Hilton" ~ "a",
    Site == "Church" ~ "a",
    Site == "West" ~ "a"
  ),
  Letter = "E")  # overwrite with E for all sites if you want the panel letter E

# DWWW — now labeled F
DWWW_uni_sum <- DWWW_uni_sum %>%
  mutate(Letter = "")  # overwrite with F for all sites

fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "SA:WW", fill_colors)
plot_SAV  <- create_plot(SAV_uni_sum, "SAV", "SA:V", fill_colors)
plot_TH   <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS   <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)
plot_Toughness <- create_plot(Toughness_uni_sum, "Toughness", "Toughness", fill_colors)
plot_DWWW <- create_plot(DWWW_uni_sum, "DWWW", "DW:WW", fill_colors)

library(patchwork)

combined_plot <- (
  plot_SAWW + plot_SAV + plot_TH +
    plot_TS + plot_Toughness + plot_DWWW
) +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

combined_plot

ggsave(
  filename = "univariate_wletters_glm.png",
  plot = combined_plot,
  width = 12, height = 12, dpi = 300
)

###UPDATING R###
version
update.packages(ask = FALSE, checkBuilt = TRUE)

