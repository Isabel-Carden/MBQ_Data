#install.packages("car")
  #library(readr)
  #library(car)

#read csv
  #HerbivoryAssay <- read_csv("Data/HerbivoryAssay.csv")
  #View(HerbivoryAssay)
  #Herbivory_Assay<-na.omit(HerbivoryAssay)
  #as.numeric(Herbivory_Assay$loss_per_hr)

#test for normal distribution
  #qqp(Herbivory_Assay, "norm")
  #hist(Herbivory_Assay$loss_per_hr)
  #shapiro test determines if data is normally distributed
  #shapiro.test(Herbivory_Assay$loss_per_hr)

#qqplot
#aov(y~x, data); response goes first
  #model1<-aov(loss_per_hr~Site, data=Herbivory_Assay)
  #qqp(model1)
  #bartlett test determines if there is equal variance among groups; should fail to   reject the null hypothesis
  #as.factor(Herbivory_Assay$Site)
  #bartlett.test(loss_per_hr~Site, data=Herbivory_Assay)
  #p-value is low, and does not fail to reject the null, unfortunately we will
  #transform our data and re-run the shapiro and bartlett test
  #run anova to get residuals, then qqnorm function. statology!!

library(readr)
library(car)
library(dplyr)
library(ggpubr)
library(googlesheets4)
library(Rmisc)
herb_data <- read_sheet("https://docs.google.com/spreadsheets/d/169_iAfJME3vntApKnjd_MLyABwFc-VSpz4YJqbX4QUw/edit?gid=0#gid=0")

# Shapiro test - is it normally distributed?
shapiro.test(herb_data$per_loss_hr)

#qqplot
model1<-aov(per_loss_hr~Site, data= herb_data)
qqp(model1)

#Bartlett test - equal variances among groups? 
as.factor(herb_data$Site)
bartlett.test(per_loss_hr ~ Site, data = herb_data)

herb_data$LOGpercent_loss_per_hour <- log10(herb_data$per_loss_hr)
herb_data$SQpercent_loss_per_hour <- sqrt(herb_data$per_loss_hr)

# Assumptions testing for Log transformation:
shapiro.test(herb_data$LOGpercent_loss_per_hour)

model_LOG<-aov(LOGpercent_loss_per_hour~Site, data= herb_data) #Looks good!
qqp(model_LOG)
bartlett.test(LOGpercent_loss_per_hour ~ Site, data = herb_data) #Does not look good :(

# Assumptions testing for sqrt transformation:
shapiro.test(herb_data$SQpercent_loss_per_hour)

model_SQ<-aov(SQpercent_loss_per_hour~Site, data= herb_data) #Looks good!
qqp(model_SQ)
bartlett.test(SQpercent_loss_per_hour ~ Site, data = herb_data) #Not so good again

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal.test(per_loss_hr ~ Site, data = herb_data) #significant difference between sites detected
#more than two groups, with significant difference found, post-hoc is needed, pairwise wilcox test will tell us which is which
pairwise.wilcox.test(herb_data$per_loss_hr, herb_data$Site, p.adjust.method = "BH")
#making graphs

pw <- pairwise.wilcox.test(
  herb_data$per_loss_hr,
  herb_data$Site,
  p.adjust.method = "BH"
)

pw


#ggbarplot(herb_data, x = "Site", y = "per_loss_hr",
#add = c("mean_se"),
#order = c("Hilton", "Church", "West", "Jet_Ski"),
#ylab = "Percent Weight Lost per Hour", xlab = "Site Name")
sum <- summarySE(herb_data, measurevar = "per_loss_hr", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
sum$Site <- as.character(sum$Site)
sum$Site <- factor(sum$Site, levels = c("Jet_Ski", "Hilton", "Church", "West"))

# Check: print levels to confirm
print(levels(sum$Site))  # should be Jet_Ski, Hilton, Church, West

# Plot
dataplot1 <- ggplot(sum, aes(x = Site, y = per_loss_hr, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = per_loss_hr - se, ymax = per_loss_hr + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9))  +
  scale_fill_manual(values = c("#F0E442", "#009E73", "#CC79A7", "#56B4E9")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("Percent Weight Change per Hour") +
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

###UPDATED CODE###
library(ggplot2)
library(Rmisc)
library(dplyr)
library(googlesheets4)

# Load data
herb_data <- read_sheet("https://docs.google.com/spreadsheets/d/169_iAfJME3vntApKnjd_MLyABwFc-VSpz4YJqbX4QUw/edit?gid=0#gid=0")

# Summarize data (mean ± SE)
sum <- summarySE(herb_data, measurevar = "per_loss_hr", groupvars = "Site", na.rm = TRUE)

# 🔧 Fix potential naming mismatches and reorder sites
sum$Site <- gsub(" ", "_", sum$Site)  # ensure consistent underscores
sum$Site <- factor(sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Check that all levels are present
print(levels(sum$Site))  

# Add numeric x positions (to control spacing)
sum <- sum %>%
  mutate(xpos = as.numeric(Site))

# OPTIONAL: Add post-hoc letters (update when you have results)
sum <- sum %>%
  mutate(Letters = case_when(
    Site == "JetSki" ~ "a",
    Site == "Hilton" ~ "ab",
    Site == "Church" ~ "b",
    Site == "West" ~ "a"
  ))

# Color palette (matches your growth comparison)
fill_colors <- c("#D55E00", "#E69F00", "#56B4E9", "#0072B2")

# Create plot
herb_plot <- ggplot(sum, aes(x = xpos, y = per_loss_hr, fill = Site)) +
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  geom_errorbar(
    aes(ymin = per_loss_hr - se, ymax = per_loss_hr + se),
    width = 0.15, size = 0.8
  ) +
  geom_text(
    aes(label = Letters, y = per_loss_hr + se + 0.10 * max(sum$per_loss_hr + sum$se)),
    size = 8, fontface = "bold"
  )+
  scale_fill_manual(values = fill_colors) +
  scale_x_continuous(
    name = "Site",
    breaks = 1:4,
    labels = c("JetSki", "Hilton", "Old Church", "West"),
    expand = expansion(add = c(0.25, 0.25))  # keeps gap on both sides
  ) +
  scale_y_continuous(
    name = "Percent Weight Lost per Hour",
    expand = expansion(mult = c(0, 0.05)),  # less empty space above
    limits = c(0, max(sum$per_loss_hr + sum$se) * 1.2)  # top = 20% above tallest bar
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
print(herb_plot)

# Save high-res PNG
ggsave(
  filename = "herbivory_comparison_WSN.png",
  plot = herb_plot,
  width = 8,
  height = 6,
  dpi = 300
)

