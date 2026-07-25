library(ggplot2) 
library(dplyr)
library(factoextra)
library(vegan)
library(rcartocolor)
library(pairwiseAdonis)
library(tidyverse)
library(Rmisc)
library(patchwork)

# === Load Data ===
# Local snapshot of Clean_Data_Traits (run from project root).
# Source sheet: https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit
Alg_Traits <- readr::read_csv(
  "Data/traits_Clean_Data_Traits_snapshot.csv",
  show_col_types = FALSE
) %>%
  select(-Thickness, -P_calcification)

Traits_Filtered <- Alg_Traits %>%
  select(TH, TS, Toughness, `DW:WW`, `WW:TH`, `SA:V`, `SA:WW`)

trait_names <- colnames(Traits_Filtered)[1:7]

# === Build Matrix ===
Fun.mat_filtered <- Traits_Filtered %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

# === PERMDISP ===
group <- Alg_Traits$Site
dist_mat <- vegdist(Traits_Filtered, method = "euclidean")
bd <- betadisper(dist_mat, group)
permutest(bd)
TukeyHSD(bd)
#hilton-church --> 0.0012
#jetski-hilton --> 0.0035
#west-hilton --> 0.013

group_dispersion <- tapply(bd$distances, group, mean)
group_dispersion

# === Equal-N PERMDISP ===
grp <- factor(Alg_Traits$Site)
set.seed(1)

group_sizes <- table(grp)
min_n <- min(group_sizes)

sub_idx <- unlist(tapply(seq_along(grp), grp, function(ix) sample(ix, min_n)))
mat_sub <- Fun.mat_filtered[sub_idx, , drop = FALSE]
grp_sub <- droplevels(grp[sub_idx])

dist_sub <- vegdist(mat_sub, method = "euclidean")
bd_sub   <- betadisper(dist_sub, group = grp_sub)

print(anova(bd_sub))
print(permutest(bd_sub, permutations = 999))

dispersion_equalN <- data.frame(
  Group      = names(tapply(bd_sub$distances, grp_sub, mean)),
  Dispersion = as.numeric(tapply(bd_sub$distances, grp_sub, mean)),
  N          = as.integer(table(grp_sub))
)
print(dispersion_equalN)

# === PERMANOVA ===
#data frame is turned into a matrix 
Fun.mat_filtered <- (Traits_Filtered) %>%
  select(1:7)%>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  as.matrix()


# adonis runs on the data from y to the x, y is the data, all the response variables are y, nutrients and herbivory pulled from alg_traits
model1 <- adonis2(Fun.mat_filtered ~ Site, data = Alg_Traits, 
                  method = "bray", permutations = 999, by = "terms")
model1 

# === PAIRWISE PERMANOVA ===
pw_results <- pairwise.adonis(dist_mat, factors = Alg_Traits$Site, p.adjust.m = "bonferroni")
print(pw_results)
#church-jetski --> 0.006
#hilton-jetski --> 0.006
#west-jetski --> 0.006

model1 <- adonis2(Fun.mat_filtered ~ Site, data = Alg_Traits,
                  method = "bray", permutations = 999, by = "terms")
model1

# === PCA ===
Fun.pca <- prcomp(Fun.mat_filtered, scale = TRUE)
colnames(Fun.pca$rotation) <- trait_names
rownames(Fun.pca$rotation) <- trait_names

# === PC1 and PC2 ANOVAs ===
pc_scores <- as.data.frame(Fun.pca$x)
pc_scores$Site <- Alg_Traits$Site

res.aov.pc1 <- aov(PC1 ~ Site, data = pc_scores)
summary(res.aov.pc1)
TukeyHSD(res.aov.pc1)
#jetski-church --> 0.00014
#jetski-hilton --> <0.0001
#west-hilton --> 0.00059

res.aov.pc2 <- aov(PC2 ~ Site, data = pc_scores)
summary(res.aov.pc2)
TukeyHSD(res.aov.pc2)
#jetski-church --> 0.012

# === Contribution Plots ===
pc1_plot <- fviz_contrib(Fun.pca, choice = "var", axes = 1) +
  labs(x = "Trait", y = "Contribution (%)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

pc2_plot <- fviz_contrib(Fun.pca, choice = "var", axes = 2) +
  labs(x = "Trait", y = "Contribution (%)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

# === Biplot ===
Alg_Traits$Site <- factor(Alg_Traits$Site,
                          levels = c("JetSki", "Hilton", "Church", "West"))

site_colors <- c("#D55E00", "#e69f00", "#29c6ff", "#026aa6")
site_shapes <- c("JetSki" = 21,
                 "Hilton" = 22,
                 "Church" = 24,
                 "West"   = 23)

biplot_scores <- as.data.frame(Fun.pca$x[, 1:3, drop = FALSE])
names(biplot_scores) <- c("PC1", "PC2", "PC3")

# Remove any pre-existing PC columns from Alg_Traits before binding
Alg_Traits <- Alg_Traits %>% select(-any_of(c("PC1", "PC2", "PC3")))

Alg_Traits_pca <- dplyr::bind_cols(Alg_Traits, biplot_scores)

names(Alg_Traits_pca)

site_means <- Alg_Traits_pca %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop") %>%
  dplyr::mutate(Site = factor(Site, levels = c("JetSki", "Hilton", "Church", "West"))) %>%
  dplyr::arrange(Site)

biplot <- fviz_pca_biplot(
  Fun.pca,
  label = "var",
  labelsize = 8,
  habillage = Alg_Traits_pca$Site,
  addEllipses = TRUE,
  ellipse.level = 0.5,
  ellipse.alpha = 0.1,
  palette = site_colors,
  col.var = "black",
  legend.title = "Site",
  geom.ind = "blank",
) +
  theme_classic()

new_west <- biplot +
  geom_point(
    data = Alg_Traits_pca,
    aes(x = PC1, y = PC2, shape = Site, fill = Site),
    size = 3,
    color = "black",
    stroke = 0) +
  scale_shape_manual(values = site_shapes) +
  scale_fill_manual(values = c(
    "JetSki" = "#D55E00",
    "Hilton" = "#e69f00",
    "Church" = "#29c6ff",
    "West"   = "#026aa6"),
    na.value = "grey50") +
  geom_point(
    data = site_means,
    aes(x = PC1, y = PC2, shape = Site),
    fill = site_colors[as.integer(site_means$Site)],
    color = "black",
    size = 4,
    stroke = 1.5) +
  guides(
    shape = guide_legend(override.aes = list(fill = site_colors, color = site_colors, stroke = 0.5)),
    colour = "none",
    fill = "none")

# === Final Plot ===
big_text <- theme(
  text = element_text(size = 30),
  axis.title = element_text(size = 25, face = "bold"),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  strip.text = element_text(size = 25, face = "bold"),
  plot.tag = element_text(size = 30, face = "bold")
)

final_plot <- (new_west + big_text) / (pc1_plot + big_text) / (pc2_plot + big_text) +
  plot_layout(heights = c(3, 1.5, 1.5)) +
  plot_annotation(tag_levels = "A")

final_plot

# Save the plot
ggsave("PCA_plot_final.png", plot = final_plot, width = 10, height = 14, units = "in", dpi = 300, bg = "white")

# PC1 vs PC3 biplot
pca_data13 <- Alg_Traits_pca[, c("Site", "PC1", "PC3")]
means_data13 <- site_means13

site_means13 <- Alg_Traits_pca %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(PC1 = mean(PC1), PC3 = mean(PC3), .groups = "drop") %>%
  dplyr::mutate(Site = factor(Site, levels = c("JetSki", "Hilton", "Church", "West"))) %>%
  dplyr::arrange(Site)

biplot13 <- fviz_pca_biplot(
  Fun.pca,
  axes = c(1, 3),
  label = "var",
  labelsize = 8,
  habillage = Alg_Traits_pca$Site,
  addEllipses = TRUE,
  ellipse.level = 0.5,
  ellipse.alpha = 0.1,
  palette = site_colors,
  col.var = "black",
  legend.title = "Site",
  geom.ind = "blank",
  title = NULL
) +
  theme_classic()

newbiplot13 <- biplot13 +
  geom_point(data = pca_data13,
             aes(x = PC1, y = PC3, shape = Site, fill = Site),
             size = 3, color = "black", stroke = 0) +
  scale_shape_manual(values = site_shapes) +
  scale_fill_manual(values = site_colors) +
  geom_point(data = means_data13,
             aes(x = PC1, y = PC3, shape = Site),
             fill = site_colors[as.integer(means_data13$Site)],
             color = "black",
             size = 4,
             stroke = 1.5) +
  guides(
    shape = guide_legend(override.aes = list(fill = site_colors, color = site_colors, stroke = 0.5)),
    colour = "none",
    fill = "none")

newbiplot13 <- newbiplot13 + big_text
newbiplot13 <- newbiplot13 +
  scale_x_continuous(name = paste0("PC1 (", round(get_eigenvalue(Fun.pca)[1, 2], 1), "%)")) +
  scale_y_continuous(name = paste0("PC3 (", round(get_eigenvalue(Fun.pca)[3, 2], 1), "%)"))
newbiplot13

# PC2 vs PC3 biplot
pca_data23 <- Alg_Traits_pca[, c("Site", "PC2", "PC3")]
means_data23 <- site_means23

site_means23 <- Alg_Traits_pca %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(PC2 = mean(PC2), PC3 = mean(PC3), .groups = "drop") %>%
  dplyr::mutate(Site = factor(Site, levels = c("JetSki", "Hilton", "Church", "West"))) %>%
  dplyr::arrange(Site)

biplot23 <- fviz_pca_biplot(
  Fun.pca,
  axes = c(2, 3),
  label = "var",
  labelsize = 8,
  habillage = Alg_Traits_pca$Site,
  addEllipses = TRUE,
  ellipse.level = 0.5,
  ellipse.alpha = 0.1,
  palette = site_colors,
  col.var = "black",
  legend.title = "Site",
  geom.ind = "blank",
  title = NULL
) +
  theme_classic()

newbiplot23 <- biplot23 +
  geom_point(data = pca_data23,
             aes(x = PC2, y = PC3, shape = Site, fill = Site),
             size = 3, color = "black", stroke = 0) +
  scale_shape_manual(values = site_shapes) +
  scale_fill_manual(values = site_colors) +
  geom_point(data = means_data23,
             aes(x = PC2, y = PC3, shape = Site),
             fill = site_colors[as.integer(means_data23$Site)],
             color = "black",
             size = 4,
             stroke = 1.5) +
  guides(
    shape = guide_legend(override.aes = list(fill = site_colors, color = site_colors, stroke = 0.5)),
    colour = "none",
    fill = "none")

newbiplot23 <- newbiplot23 + big_text
newbiplot23 <- newbiplot23 +
  scale_x_continuous(name = paste0("PC2 (", round(get_eigenvalue(Fun.pca)[2, 2], 1), "%)")) +
  scale_y_continuous(name = paste0("PC3 (", round(get_eigenvalue(Fun.pca)[3, 2], 1), "%)"))
newbiplot23

# pc3 contribution plot
pc3_plot <- fviz_contrib(Fun.pca, choice = "var", axes = 3) +
  labs(x = "Trait", y = "Contribution (%)") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  big_text

# Combine
grob13 <- ggplotGrob(newbiplot13)
grob23 <- ggplotGrob(newbiplot23)
grob_pc3 <- ggplotGrob(pc3_plot)

library(gridExtra)
PC3_panel <- gridExtra::grid.arrange(grob13, grob23, grob_pc3, ncol = 1)

ggsave("PC3_biplot_panel.png", plot = PC3_panel, width = 9, height = 16, dpi = 300)
