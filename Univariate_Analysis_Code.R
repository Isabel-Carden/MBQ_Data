#install.packages("emmeans")
#if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(emmeans)
library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)
library(patchwork)
library(FSA)
library(dunn.test)
library(cowplot)

# Authentication and data loading
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

#Dunn's Test - TH, TS, and Toughness
univariate_trait_data <- univariate_trait_data %>%
  dplyr::rename(
    SAWW = `SA:WW`,
    SAV  = `SA:V`,
    DWWW = `DW:WW`,
    WWTH = `WW:TH`
  )

univariate_trait_data$Site <- factor(univariate_trait_data$Site,
                                     levels = c("JetSki", "Hilton", "Church", "West"))

# Define traits to analyze
traits <- c("SAWW", "SAV", "TH", "TS", "DWWW", "Toughness", "WWTH")

# Function for statistical analysis
analyze_trait <- function(trait_names, data) {
  cat("\n=== Analysis for", trait_names, "===\n")
  
  # Bartlett test for homogeneity of variances
  trait_escaped <- paste0("`", trait_names, "`")
  
  bartlett_result <- bartlett.test(
    as.formula(paste(trait_names, "~ Site")),
    data = data
  )
  cat("Bartlett test p-value:", bartlett_result$p.value, "\n")
  
  # Shapiro test for normality
  shapiro_result <- shapiro.test(data[[trait_names]])
  cat("Shapiro test p-value:", shapiro_result$p.value, "\n")
  
  # Kruskal-Wallis test
  kruskal_result <- kruskal.test(
    as.formula(paste(trait_names, "~ Site")),
    data = data
  )
  
  cat("Kruskal-Wallis p-value:", kruskal_result$p.value, "\n")
  
  # Pairwise comparisons if significant
  if (kruskal_result$p.value < 0.05) {
    cat("\n--- Wilcoxon Pairwise Comparisons ---\n")
    pairwise_result <- pairwise.wilcox.test(data[[trait_names]], data$Site,
                                            p.adjust.method = "BH")
    print(pairwise_result$p.value)
    
    # Dunn's Test
    cat("\n--- Dunn's Test Pairwise Comparisons ---\n")
    dunn_result <- dunnTest(data[[trait_names]], data$Site, method = "bh")
    print(dunn_result)
    
    # Store Dunn's test results for later use
    return(list(
      kruskal_p = kruskal_result$p.value,
      dunn_result = dunn_result
    ))
  } else {
    return(list(
      kruskal_p = kruskal_result$p.value,
      dunn_result = NULL
    ))
  }
}

# Run analysis for all traits and store results
results_list <- list()
for (trait in traits) {
  results_list[[trait]] <- analyze_trait(trait, univariate_trait_data)
}

# Create summary data for plotting with site names
create_summary <- function(trait, data) {
  summary_data <- summarySE(data, measurevar = trait, groupvars = "Site", na.rm = TRUE)
  # Use the same factor levels as your original data
  summary_data$Site <- factor(summary_data$Site, levels = c("JetSki", "Hilton", "Church", "West"))
  return(summary_data)
}

# Create all summary datasets
SAWW_uni_sum <- create_summary("SAWW", univariate_trait_data)
SAV_uni_sum  <- create_summary("SAV",  univariate_trait_data)
DWWW_uni_sum <- create_summary("DWWW", univariate_trait_data)
WWTH_uni_sum <- create_summary("WWTH", univariate_trait_data)
TH_uni_sum <- create_summary("TH", univariate_trait_data) #Dunn's used in final analysis
TS_uni_sum <- create_summary("TS", univariate_trait_data) #Dunn's used in final analysis
Toughness_uni_sum <- create_summary("Toughness", univariate_trait_data) #Dunn's used in final analysis

# Print summary of significant results with Dunn's test details
cat("\n=== SIGNIFICANCE SUMMARY ===\n")
for (i in 1:length(results_list)) {
  trait <- traits[i]
  result <- results_list[[trait]]
  significance <- ifelse(result$kruskal_p < 0.05, "SIGNIFICANT", "not significant")
  cat(sprintf("\n%s: p = %.4f (%s)\n", trait, result$kruskal_p, significance))
  
  # Print Dunn's test results for significant traits
  if (result$kruskal_p < 0.05 && !is.null(result$dunn_result)) {
    cat("Significant pairwise comparisons (Dunn's test):\n")
    # Extract significant comparisons (p < 0.05)
    sig_comparisons <- result$dunn_result$res[result$dunn_result$res$P.adj < 0.05, ]
    if (nrow(sig_comparisons) > 0) {
      print(sig_comparisons)
    } else {
      cat("No significant pairwise comparisons at p < 0.05\n")
    }
  }
}

#General Linearized Model (GLM) - SA:WW, SA:V, DW:WW
# Make sure Site is a factor
univariate_trait_data$Site <- as.factor(univariate_trait_data$Site)

##SAWW##
# Check equal variances (Bartlett’s test)
bartlett.test(SAWW ~ Site, data = univariate_trait_data)

#glm#
model_SAWW <- glm(SAWW ~ Site,
                  family = Gamma(link = "log"),
                  data = univariate_trait_data) #log because response variable is continuous and strictly positive
summary(model_SAWW)
anova(model_SAWW, test = "Chisq")

# Estimated marginal means for each Site
em_SAWW <- emmeans(model_SAWW, ~ Site)

# Pairwise comparisons with Tukey adjustment
pairs(em_SAWW, adjust = "tukey")

##SAV##
# Check equal variances (Bartlett’s test)
bartlett.test(SAV ~ Site, data = univariate_trait_data)

#glm#
model_SAV <- glm(SAV ~ Site,
                 family = inverse.gaussian(link = "log"),
                 data = univariate_trait_data)

# Look at the summary
summary(model_SAV)
anova(model_SAV, test = "Chisq")

# Estimated marginal means for each Site
em_SAV <- emmeans(model_SAV, ~ Site)

# Pairwise comparisons with Tukey adjustment
pairs(em_SAV, adjust = "tukey")

##DWWW##
# Check equal variances (Bartlett’s test)
bartlett.test(DWWW ~ Site, data = univariate_trait_data)

#glm#
model_DWWW <- glm(DWWW ~ Site,
                  family = inverse.gaussian(link = "log"),  # change family
                  data = univariate_trait_data)

# Look at the summary
summary(model_DWWW)
anova(model_DWWW, test = "Chisq")

# Estimated marginal means for each Site
em_DWWW <- emmeans(model_DWWW, ~ Site)

# Pairwise comparisons with Tukey adjustment
pairs(em_DWWW, adjust = "tukey")

#Combined Plots
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
      axis.title = element_text(face = "bold", size = 18),
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
  ))

# DWWW — now labeled F
DWWW_uni_sum <- DWWW_uni_sum %>%
  mutate(Letter = "")  # overwrite with F for all sites

fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "SA:WW (cm^2/g)", fill_colors)
plot_SAV  <- create_plot(SAV_uni_sum, "SAV", "SA:V (cm^2/mL)", fill_colors)
plot_TH   <- create_plot(TH_uni_sum, "TH", "Thallus Height (cm)", fill_colors)
plot_TS   <- create_plot(TS_uni_sum, "TS", "Tensile Strength (kg)", fill_colors)
plot_Toughness <- create_plot(Toughness_uni_sum, "Toughness", "Toughness (g)", fill_colors)
plot_DWWW <- create_plot(DWWW_uni_sum, "DWWW", "DW:WW (g/g)", fill_colors)

combined_plot <- (
  plot_SAWW + plot_SAV + plot_TH +
    plot_TS + plot_Toughness + plot_DWWW
) +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

combined_plot

ggsave(filename = "univariate_wletters_glm.png", plot = combined_plot, width = 12, height = 12, dpi = 300)
