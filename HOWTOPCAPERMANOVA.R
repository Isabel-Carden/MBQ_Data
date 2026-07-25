rm(list=ls())
#please plug and chug. 

#Load Libraries
#install.packages("ggfortify", dependencies = TRUE)
#install.packages("readxl", dependencies = TRUE)
library(readxl)
#install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
#install.packages("dplyr", dependencies = TRUE)
library(dplyr)
#install.packages("devtools", dependencies = TRUE)
library(devtools)
#install.packages("factoextra", dependencies = TRUE)
library(factoextra)
#install.packages("vegan", dependencies = TRUE)
library(vegan)
#install.packages("ggpubr", dependencies = TRUE)
library(ggpubr)
#install.packages("car", dependencies = TRUE)
library(car)
#install.packages("reshape2", dependencies = TRUE)
library(reshape2)
#install.packages("rcartocolor", dependencies = TRUE)
library(rcartocolor)
#PerManova
library(vegan)
#install.packages("vegan")
#devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
library(ggplot2)
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("knitr")
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#All Data
# Read in data, remove %calcification, drop any rows with NAs -  replace with your actual file path
TurbTraits <- read_csv("Associated Traits/TurbinariaUnderstoryTraits.csv") %>%
  select(-PercentCalcification)%>%
  drop_na()
####PERMANOVA
# Filter the dataset to keep only the relevant traits and ratios
TurbTraits_filtered <- TurbTraits %>%
  select(Height,WWH, Tensile, Toughness, DWWW,)
TurbTraits_filtered
# Select your response variables (the multivariate data)
#str(multi) #look at char v factor v num
######Permanova#############Permanova#############Permanova#############Permanova#############Permanova#######
# Convert selected columns (4:12) to numeric (in case they were characters/factors), and begin making a matrix
Fun.mat_filtered <- TurbTraits_filtered %>%
  select(4:10) %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  as.matrix()
Fun.mat_filtered
# Select your response variables (the multivariate data)
#str(multi) #look at char v factor v num
model1 <- adonis2(Fun.mat_filtered ~ Individual * Location, data = TurbTraits_filtered, 
                  method = "bray", permutations = 999, by = "terms")
model1 
##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA##########PCA
#make the pca, scale the data
Fun.pca <- prcomp(Fun.mat_filtered,  scale = TRUE) 
Fun.pca
#plots all of the points, just checking it works
fviz_pca_ind(Fun.pca) 
#plots the vectors (loading plot)
fviz_pca_var(Fun.pca)
#Vector contributions for PCA1
fviz_contrib(Fun.pca, choice= "var", axes=1) 
#H, WW, DW ,WW:H all above red on PC1
#Vector contributions for PCA2
fviz_contrib(Fun.pca, choice="var", axes= 2)
# penetrometer above red line
##PCA Biplot code##
# Biplot colored by Species and Location (e.g., "Halimeda_in", "Padina_out")
biplot <- fviz_pca_biplot(Fun.pca, 
                          label = "var", 
                          habillage = interaction(TurbTraits_filtered$Individual, TurbTraits_filtered$Location),
                          addEllipses = TRUE,
                          col.var = "black",
                          ellipse.level = 0.5) +
  theme_classic()
biplot