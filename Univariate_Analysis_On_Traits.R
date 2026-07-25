#univariate analysis
library(readr)
library(car)
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(Rmisc)
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")
View(univariate_trait_data)

#Bartlett test (SAWW) - equal variances among groups? We hope so! Should fail to reject the null.
as.factor(univariate_trait_data$Site)
bartlett.test(SAWW ~ Site, data = univariate_trait_data) #p<0.05...time for transformations

univariate_trait_data$LOG_SAWW <- log10(univariate_trait_data$SAWW)
bartlett.test(LOG_SAWW ~ Site, data = univariate_trait_data)


# Shapiro test (SAWW) - is it normally distributed?
shapiro.test(univariate_trait_data$SAWW) #p<0.05; data not normal...log transformation
#univariate_trait_data$LOG_SAWW <- log10(univariate_trait_data$SAWW)
#shapiro.test(univariate_trait_data$LOG_SAWW) #p<0.05 still too los...try square root
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


# Plot
dataplot1 <- ggplot(SAWW_uni_sum, aes(x = Site, y = SAWW, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = SAWW - se, ymax = SAWW + se),
                size = 0.8, width = 0.3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("SA:WW") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))
dataplot1

#-----------#

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

# Plot
dataplot1 <- ggplot(SAV_uni_sum, aes(x = Site, y = SAV, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = SAV - se, ymax = SAV + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("SAV") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))
dataplot1

#-----------#

#Bartlett test (Thallus_Thickness)
as.factor(univariate_trait_data$Site)
bartlett.test(Thickness ~ Site, data = univariate_trait_data) #p<0.05

univariate_trait_data$LOG_Thickness <- log10(univariate_trait_data$Thallus_Thickness)
bartlett.test(LOG_Thickness ~ Site, data = univariate_trait_data) #p=0.00019
univariate_trait_data$SQ_Thickness <- sqrt(univariate_trait_data$Thickness)
bartlett.test(SQ_Thickness ~ Site, data = univariate_trait_data) #p=0.1337; bartlett test for thallus thickness works when I square root it

# Shapiro test (Thallus_Thickness) - is it normally distributed?
shapiro.test(univariate_trait_data$Thickness) #p<0.05; data not normal...log transformation
univariate_trait_data$LOG_Thickness <- log10(univariate_trait_dataThickness)
shapiro.test(univariate_trait_data$LOG_Thickness) #p<0.05 still too los...try square root
univariate_trait_data$SQThickness <- sqrt(univariate_trait_data$Thickness)
shapiro.test(univariate_trait_data$SQThickness) #p<0.05...move on to non parametric approach

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(Thickness ~ Site, data = univariate_trait_data) #p=0.005563...Significance for thickness!

pairwise.wilcox.test(univariate_trait_data$Thickness, univariate_trait_data$Site, p.adjust.method = "BH")

#graph
Thallus_Thickness_uni_sum <- summarySE(univariate_trait_data, measurevar = "Thallus_Thickness", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
Thallus_Thickness_uni_sum$Site <- as.character(Thallus_Thickness_uni_sum$Site)
Thallus_Thickness_uni_sum$Site <- factor(Thallus_Thickness_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Check: print levels to confirm
print(levels(Thallus_Thickness_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

# Plot
dataplot1 <- ggplot(Thallus_Thickness_uni_sum, aes(x = Site, y = Thallus_Thickness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Thallus_Thickness - se, ymax = Thallus_Thickness + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("Thallus Thickness") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))
dataplot1


#-----------#

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

# Plot
dataplot1 <- ggplot(TH_uni_sum, aes(x = Site, y = TH, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = TH - se, ymax = TH + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("TH") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))
dataplot1

#-----------#

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

# Plot
dataplot1 <- ggplot(TS_uni_sum, aes(x = Site, y = TS, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = TS - se, ymax = TS + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("TS") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))
dataplot1

#-----------#

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

# Plot
dataplot1 <- ggplot(DWWW_uni_sum, aes(x = Site, y = DWWW, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = DWWW - se, ymax = DWWW + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("Dry Weight:Wet Weight") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))

dataplot1


#-----------#

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

#graph
Toughness_uni_sum <- summarySE(univariate_trait_data, measurevar = "Toughness", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
Toughness_uni_sum$Site <- as.character(Toughness_uni_sum$Site)
Toughness_uni_sum$Site <- factor(Toughness_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(Toughness_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

# Plot
dataplot1 <- ggplot(Toughness_uni_sum, aes(x = Site, y = Toughness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Toughness - se, ymax = Toughness + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("Toughness") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))

dataplot1

#-----------#

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

#graph
WWTH_uni_sum <- summarySE(univariate_trait_data, measurevar = "WWTH", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
WWTH_uni_sum$Site <- as.character(WWTH_uni_sum$Site)
WWTH_uni_sum$Site <- factor(WWTH_uni_sum$Site, levels = c("Jet_Ski", "Hilton", "Old_Church", "West"))

# Check: print levels to confirm
print(levels(WWTH_uni_sum$Site))  # should be Jet_Ski, Hilton, Church, West

# Plot
dataplot1 <- ggplot(WWTH_uni_sum, aes(x = Site, y = WWTH, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = WWTH - se, ymax = WWTH + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("WWTH") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=13,face="bold"))

dataplot1

#----------#
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

# Read data
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

# Create summary data ----
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
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate plots
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "Surface Area to Wet Weight", fill_colors)
plot_SAV <- create_plot(SAV_uni_sum, "SAV", "Surface Area to Volume", fill_colors)
plot_TH <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)

# Extract legend from one of the plots
legend <- cowplot::get_legend(plot_SAWW + theme(legend.position = "bottom"))

# Combine plots into a 3x2 grid with the legend in the bottom-right quadrant
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),
  ncol = 2, nrow = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

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

library(patchwork)

# Combine plots into a 3x2 grid with panel labels
combined_plot <- (plot_SAWW + plot_SAV + plot_TH + plot_TS) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',  # Adds panel labels A, B, C, D, E
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1,
      legend.position = "bottom",  # Position the legend at the bottom
      legend.justification = c(1, 1),  # Center the legend horizontally
      legend.direction = "horizontal",  # Arrange legend items horizontally
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
      ))
    )
  )

print(combined_plot)

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
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),
  ncol = 2, nrow = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Add tags (A-E) to the plots
combined_plot <- combined_plot +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 18, hjust = 0, vjust = 1)
        plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1)

# Display the combined plot
print(combined_plot)

# Save the combined plot to a file
ggsave("combined_plot_with_labels_and_legend.png", combined_plot, width = 12, height = 10, dpi = 300)
#----------#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)
library(Rmisc)
install.packages("ggplot2", dependencies = TRUE)

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Define the plotting function
create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.6, color = "black") +
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
gs4_deauth()
univariate_trait_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aAwoejLaJwx9qgimHU3AKo2CSkTIXhkvGFonqSMaXk/edit?gid=1214096800#gid=1214096800")

# Create summary data
SAWW_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAWW", groupvars = "Site", na.rm = TRUE)
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

SAV_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "SAV", groupvars = "Site", na.rm = TRUE)
SAV_uni_sum$Site <- factor(SAV_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TH_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TH", groupvars = "Site", na.rm = TRUE)
TH_uni_sum$Site <- factor(TH_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

TS_uni_sum <- summarySE(data = univariate_trait_data, measurevar = "TS", groupvars = "Site", na.rm = TRUE)
TS_uni_sum$Site <- factor(TS_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Generate plots
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "SA:WW", fill_colors)
plot_SAV <- create_plot(SAV_uni_sum, "SAV", "SA:V", fill_colors)
plot_TH <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)

# Extract legend from one of the plots
legend <- cowplot::get_legend(plot_SAWW + theme(legend.position = "bottom"))

# Combine plots into a 3x2 grid with the legend at the bottom
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),
  ncol = 2, nrow = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Add tags (A-D) to the plots
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),
  ncol = 2, nrow = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") +
  plot_annotation(tag_levels = 'A') &  # Set tag levels to 'A'
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 18, hjust = 0, vjust = 1))

# Ensure the combined plot is rendered
print(combined_plot)

# Save the combined plot to a PNG file
ggsave("comb_plots.png", plot = combined_plot, width = 12, height = 10, dpi = 300)

#----------@
# Ensure necessary libraries are loaded
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

# Function to replace spaces with underscores in column names
clean_colnames <- function(df) {
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}

# Apply the function to your data
univariate_trait_data <- clean_colnames(univariate_trait_data)

# Create summary data ----
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
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate plots
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "Surface Area to Wet Weight", fill_colors)
plot_SAV <- create_plot(SAV_uni_sum, "SAV", "Surface Area to Volume", fill_colors)
plot_TH <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)

# Extract legend from one of the plots
legend <- cowplot::get_legend(plot_SAWW + theme(legend.position = "bottom"))

# Combine plots into a 3x2 grid with the legend in the bottom-right quadrant
combined_plot <- wrap_plots(
  plot_SAWW, plot_SAV, plot_TH,
  plot_TS, ggplot() + theme_void(),
  ncol = 2, nrow = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

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

# Combine plots into a 3x2 grid with the legend at the bottom
combined_plot <- (plot_SAWW + plot_SAV + plot_TH + plot_TS) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',  # Adds panel labels A, B, C, D, E
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      legend.position = "bottom",  # Position the legend at the bottom
      legend.justification = c(0.5, 0),  # Center the legend horizontally
      legend.direction = "horizontal",  # Arrange legend items horizontally
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 12),
      axis.title = element_text(face = "bold")
    )
  )

print(combined_plot)

# Ensure 'Site' is a factor with levels
SAWW_uni_sum$Site <- factor(SAWW_uni_sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Define fill colors
fill_colors <- c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")

# Plot function with mapped fill aesthetic
create_plot <- function(data, yvar, ylab_text, fill_colors) {
  ggplot(data, aes(x = Site, y = .data[[yvar]], fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = .data[[yvar]] - se, ymax = .data[[yvar]] + se),
                  size = 0.8, width = 0.15, position = position_dodge(0.6)) +
    scale_fill_manual(values = fill_colors) +
    labs(fill = "Site") +
    xlab("Site") +
    ylab(ylab_text) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal(base_size = 16) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate plots
plot_SAWW <- create_plot(SAWW_uni_sum, "SAWW", "Surface Area to Wet Weight", fill_colors)
plot_SAV <- create_plot(SAV_uni_sum, "SAV", "Surface Area to Volume", fill_colors)
plot_TH <- create_plot(TH_uni_sum, "TH", "Thallus Height", fill_colors)
plot_TS <- create_plot(TS_uni_sum, "TS", "Tensile Strength", fill_colors)

# Combine plots into a 3x2 grid with the legend at the bottom
combined_plot <- (plot_SAWW + plot_SAV + plot_TH + plot_TS) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 1),
      legend.position = "bottom",
      legend.justification = c(0.5, 0),
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 12),
      axis.title = element_text(face = "bold")
    )
  )

print(combined_plot)
# Save the combined plot as a PNG file
ggsave("combined_plot1.0.png", plot = combined_plot, width = 10, height = 8, units = "in", dpi = 300)
