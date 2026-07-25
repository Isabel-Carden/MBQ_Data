library(readxl)
library(ggplot2)
library(dplyr)
library(devtools)
library(factoextra)
library(vegan)
library(ggpubr)
library(car)
library(reshape2)
library(rcartocolor)
library(pairwiseAdonis)
library(tidyr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(googlesheets4)
library(Rmisc)
library(patchwork)

gs4_deauth()
Alg_Traits <- read_sheet(
  "https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800",
  .name_repair = "minimal"
) %>%
  select(-Thickness, -P_calcification) #removes these two columns

# Filter the dataset to keep only the relevant traits and ratios
Traits_Filtered <- Alg_Traits %>%
  select(
    TH,
    TS,
    Toughness,
    `DW:WW`,
    `WW:TH`,
    `SA:V`,
    `SA:WW`
  )

trait_names <- colnames(Traits_Filtered)[1:7]

#Permanova
#data frame is turned into a matrix 
Fun.mat_filtered <- (Traits_Filtered) %>%
  select(1:7)%>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  as.matrix()


# adonis runs on the data from y to the x, y is the data, all the response variables are y, nutrients and herbivory pulled from alg_traits
model1 <- adonis2(Fun.mat_filtered ~ Site, data = Alg_Traits, 
                  method = "bray", permutations = 999, by = "terms")
model1 

#Principal Component Analysis
#make the pca, scale the data
# Run PCA
Fun.pca <- prcomp(Fun.mat_filtered, scale = TRUE)
colnames(Fun.pca$rotation) <- trait_names
rownames(Fun.pca$rotation) <- trait_names

# PCA individual plot (optional - view individuals in PCA space)
fviz_pca_ind(Fun.pca) +
  labs(title = "PCA - Individuals")

# PCA variable plot (optional - view traits as vectors)
fviz_pca_var(Fun.pca) +
  labs(title = "PCA - Variables")

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
    plot.title = element_blank(),                # Remove title
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Eigenvalues
eigenvalues <- Fun.pca$sdev^2
eigenvalues

# Prepare data frame
pc_df <- data.frame(
  PC = factor(paste0("PC", 1:length(eigenvalues)), levels = paste0("PC", 1:length(eigenvalues))),
  Eigenvalue = eigenvalues
)

dev.off()  # Important: closes the file device

pc_scores <- as.data.frame(Fun.pca$x[, 1:3])  # Extract first 3 PCs
colnames(pc_scores) <- c("PC1", "PC2", "PC3")

Alg_Traits <- bind_cols(Alg_Traits, pc_scores)

Alg_Traits$Site <- factor(Alg_Traits$Site,
                          levels = c("JetSki", "Hilton", "Church", "West"))

site_colors <- c("#D55E00", "#e69f00", "#29c6ff", "#026aa6")

biplot <- fviz_pca_biplot(Fun.pca,
                          label = "var",
                          labelsize = 8,
                          habillage = interaction(Alg_Traits$Site),
                          addEllipses = TRUE,
                          ellipse.level = 0.5,
                          palette = site_colors,
                          col.var = "black",
                          legend.title = "Site",
                          geom = "point",
                          pointsize = 3) +  # <-- smaller points here
  theme_classic() +
  labs(x = "PC1 (35.9%)", y = "PC2 (19.2%)") +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

new_west<- biplot + geom_point(data = subset(Alg_Traits, Site == "West"),
                               aes(x = PC1, y = PC2, shape = Site),
                               size = 2, color = site_colors[4]) +
  scale_shape_manual(values = c("JetSki" = 16,  
                                "Hilton" = 15,
                                "Church" = 17,  
                                "West" = 18)) +
  scale_color_manual(values = site_colors)

big_text <- theme(
  text = element_text(size =30),            # all text
  axis.title = element_text(size = 25, face = "bold"),
  axis.text = element_text(size =16),
  legend.text = element_text(size = 16),
  strip.text = element_text(size = 25, face = "bold"),  # facet labels
  plot.tag = element_text(size = 30, face = "bold")     # A/B/C labels
)

bottom_row <- pc1_plot + pc2_plot +
  plot_layout(ncol = 2)

final_plot <- new_west / pc1_plot / pc2_plot +
  plot_layout(heights = c(3, 1.5, 1.5)) +
  plot_annotation(tag_levels = "A") &
  big_text

final_plot

# Save the plot
ggsave("PCA_plot_final.png",
       plot = final_plot,
       width = 10,
       height = 14,
       units = "in",
       dpi = 300,
       bg = "white")

###PC1 vs PC3###
summary(Fun.pca)$importance


# Extract variance explained (%)
var_percent <- summary(Fun.pca)$importance[2, ] * 100

# PC1 vs PC3 biplot
biplot13 <- fviz_pca_biplot(
  Fun.pca,
  axes = c(1, 3),
  label = "var",
  habillage = interaction(Alg_Traits$Site),
  addEllipses = TRUE,
  ellipse.level = 0.5,
  palette = site_colors,
  col.var = "black",
  geom = "point",
  pointsize = 3,
  title = NULL        
) +
  theme_classic()

biplot13

newbiplot13 <- biplot13 +
  geom_point(
    data = subset(Alg_Traits, Site == "West"),
    aes(x = PC1, y = PC3, shape = Site),
    size = 4, color = site_colors[4]
  ) +
  scale_shape_manual(values = c(
    "JetSki" = 16,
    "Hilton" = 15,
    "Church" = 17,
    "West" = 18
  )) +
  scale_color_manual(values = site_colors) +
  labs(
    x = paste0("PC1 (", round(var_percent[1], 2), "%)"),
    y = paste0("PC3 (", round(var_percent[3], 2), "%)")
  )
newbiplot13 <- newbiplot13 & big_text


ggsave("PC1vsPC3_biplot.png", plot = newbiplot13, width = 8, height = 6, dpi = 300)

# PC2 vs PC3 biplot
biplot23 <- fviz_pca_biplot(
  Fun.pca,
  axes = c(2, 3),
  label = "var",
  habillage = interaction(Alg_Traits$Site),
  addEllipses = TRUE,
  ellipse.level = 0.5,
  palette = site_colors,
  col.var = "black",
  geom = "point",
  pointsize = 3,
  title = NULL
) +
  theme_classic() +
  labs(
    x = paste0("PC2 (", round(var_percent[2], 2), "%)"),
    y = paste0("PC3 (", round(var_percent[3], 2), "%)")
  )
biplot23

newbiplot23<-biplot23 + geom_point(data = subset(Alg_Traits, Site == "West"),
                                   aes(x = PC1, y = PC2, shape = Site),
                                   size = 4, color = site_colors[4]) +
  scale_shape_manual(values = c("JetSki" = 16,  
                                "Hilton" = 15,
                                "Church" = 17,  
                                "West" = 18)) +
  scale_color_manual(values = site_colors) &
  big_text

newbiplot23

ggsave("PC2vsPC3_biplot.png", plot = biplot23, width = 8, height = 6, dpi = 300)

pc3_plot <- fviz_contrib(Fun.pca, choice = "var", axes = 3) +
  labs(x = "Trait", y = "Contribution (%)") +
  theme_classic(base_size = 12) +  
  theme(
    plot.title = element_blank(),                # Remove title
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) &
  big_text
pc3_plot

PC3_panel <- newbiplot13 /
  newbiplot23 /
  pc3_plot +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A")


PC3_panel


ggsave("PC3_biplot_panel.png", plot = PC3_panel, width = 9, height = 16, dpi = 300)
