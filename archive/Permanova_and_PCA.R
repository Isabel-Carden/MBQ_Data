rm(list=ls())
#please plug and chug. 

#Load Libraries
install.packages("ggfortify", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
library(readxl)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
install.packages("dplyr", dependencies = TRUE)
library(dplyr)
install.packages("devtools", dependencies = TRUE)
library(devtools)
install.packages("factoextra", dependencies = TRUE)
library(factoextra)
install.packages("vegan", dependencies = TRUE)
library(vegan)
install.packages("ggpubr", dependencies = TRUE)
library(ggpubr)
install.packages("car", dependencies = TRUE)
library(car)
install.packages("reshape2", dependencies = TRUE)
library(reshape2)
install.packages("rcartocolor", dependencies = TRUE)
library(rcartocolor)

#Permanova
library(vegan)
install.packages("vegan")
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
library(ggplot2)
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("knitr")
library(knitr)
install.packages("kableExtra")
library(kableExtra)

#All Data
install.packages("Rmisc")
library(readr)
library(car)
library(dplyr)
library(ggpubr)
library(googlesheets4)
library(ggpubr)
library(Rmisc)
gs4_deauth()
# Read in data, remove %calcification, drop any rows with NAs -  replace with your actual file path
Alg_Traits <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")
  #select(-P_calcification)%>% #pipe --> connects with dropping N/A
  #drop_na() #didn't work
Alg_Traits$P_calcification=NULL
View(Alg_Traits)
  ####PERMANOVA
# Filter the dataset to keep only the relevant traits and ratios
Alg_Traits_filtered <- Alg_Traits %>%
  select(TH, TS, Toughness, DWWW, WWTH, SAV, SAWW)
Alg_Traits_filtered

# Select your response variables (the multivariate data)
#str(multi) #look at char v factor v num; didn't work (not found)

######Permanova#############Permanova#############Permanova#############Permanova#############Permanova#######
# Convert selected columns 1:8) to numeric (in case they were characters/factors), and begin making a matrix
Fun.mat_filtered <- Alg_Traits_filtered %>%
  select(1:8) %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  as.matrix()
Fun.mat_filtered
# Select your response variables (the multivariate data)
#str(multi) #look at char v factor v num
model1 <- adonis2(Fun.mat_filtered ~ Site, data = Alg_Traits, 
                  method = "bray", permutations = 999, by = "terms") 
model1 #yay! significant!!!!!!!
##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA
#make the pca, scale the data
Fun.pca <- prcomp(Fun.mat_filtered,  scale = TRUE) 
Fun.pca
#plots all of the points, just checking it works
fviz_pca_ind(Fun.pca) +
  labs(x="PC1", y="PC2")
#plots the vectors (loading plot)
fviz_pca_var(Fun.pca) +
  labs(x="PC1", y="PC2")

#Vector contributions for PCA1
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)

# Colorblind-friendly colors
dark_orange_cb <- "#CC6600"
blue_cb <- "#0072B2"

# Prepare contributions
contrib_df <- as.data.frame(Fun.pca$rotation^2) * 100
contrib_df$Trait <- gsub("_", " ", rownames(contrib_df))  # Remove underscores
contrib_df$Trait <- str_wrap(contrib_df$Trait, width = 10)  # Wrap long names

# Top contributors
pc1_df <- contrib_df %>%
  select(Trait, PC1 = PC1) %>%
  arrange(desc(PC1)) %>%
  slice(1:10)

pc2_df <- contrib_df %>%
  select(Trait, PC2 = PC2) %>%
  arrange(desc(PC2)) %>%
  slice(1:10)

# Null contribution line
null_line <- 100 / ncol(Fun.mat_filtered)

# Set common Y axis limit
max_val <- max(c(pc1_df$PC1, pc2_df$PC2))
y_limit <- ceiling(max_val * 1.1)  # 10% headroom

# PC1 plot
p1 <- ggplot(pc1_df, aes(x = reorder(Trait, PC1), y = PC1)) +
  geom_col(fill = blue_cb) +
  geom_hline(yintercept = null_line, linetype = "dashed", color = dark_orange_cb, size = 1.3) +
  labs(title = "Top Contributors to PC1", x = "Trait", y = "Contribution to PC1 (%)") +
  ylim(0, y_limit) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12)
  )

# PC2 plot
p2 <- ggplot(pc2_df, aes(x = reorder(Trait, PC2), y = PC2)) +
  geom_col(fill = blue_cb) +
  geom_hline(yintercept = null_line, linetype = "dashed", color = dark_orange_cb, size = 1.3) +
  labs(title = "Top Contributors to PC2", x = "Trait", y = "Contribution to PC2 (%)") +
  ylim(0, y_limit) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12)
  )

# Combine side by side
ggarrange(p1, p2, ncol = 2, common.legend = FALSE)
##PCA Biplot code##

combined_plot <- ggarrange(p1, p2, ncol = 2, common.legend = FALSE)

# Save to PDF
ggsave("PCA_Contributions_Graphs.pdf", plot = combined_plot, width = 16, height = 6, units = "in")

#only removed one outlier (valonia)
#mostly fixing errors
#nine hilton data from first days --> inconsistent in thickness
#formula for if we need to removed outliers (z-score) --> 
#removed H-29.08-Padi-H becasue DW:WW was above 1 (1.4)
#removed outliers with DW:WW over 1
#removed padina from west with negative DW values
#low herb shifts it away from being tough; high herbivory requires thickness
#low herb shifts right bc you can take advantage of resources without worrying about herbivores
#low nutrients --> LH needs to be thick and strong and tough; LL shifts towards SA ratios and very high variability in trait diversity so this means that there are lots of different ways to exist; herbivory collapses variation; gradient with grow out

library(factoextra)
library(ggplot2)
library(dplyr)

# Make sure Site factor levels are set correctly
Alg_Traits$Site <- factor(Alg_Traits$Site,
                          levels = c("JetSki", "Hilton", "Church", "West"))

# Define your palette (matching your sites)
site_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Create PCA biplot with ellipses
biplot <- fviz_pca_biplot(Fun.pca,
                          label = "var",
                          habillage = interaction(Alg_Traits$Site),
                          addEllipses = TRUE,
                          ellipse.level = 0.5,
                          palette = site_colors,
                          col.var = "black",
                          legend.title = "Site",
                          geom = "point") +
  theme_classic() +
  labs(x = "PC1 (35.6%)", y = "PC2 (17.2%)") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"))

# Modify ellipse layer: remove fill, keep border color, and bold border
for (i in seq_along(biplot$layers)) {
  if (inherits(biplot$layers[[i]]$geom, "GeomPolygon")) {
    biplot$layers[[i]]$aes_params$fill <- NA              # remove fill
    # Keep the original color (ellipse border) as is
    # Increase linewidth for bold borders
    biplot$layers[[i]]$aes_params$linewidth <- 1.5
  }
}

print(biplot)

ggsave("PCA_plot.pdf", plot = biplot, width = 8, height = 6, dpi = 300)
