
#install.packages("FSA")
#install.packages("dunn.test")


library(readr)
library(car)
library(dplyr)
library(ggpubr)
library(googlesheets4)
library(Rmisc)
library(FSA)
library(dunn.test)

gs4_deauth()
herb_data <- read_sheet("https://docs.google.com/spreadsheets/d/169_iAfJME3vntApKnjd_MLyABwFc-VSpz4YJqbX4QUw/edit")
#View(herb_data)

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

model_LOG<-aov(LOGpercent_loss_per_hour~Site, data= herb_data) 
qqp(model_LOG)
bartlett.test(LOGpercent_loss_per_hour ~ Site, data = herb_data) 

# Assumptions testing for sqrt transformation:
shapiro.test(herb_data$SQpercent_loss_per_hour)

model_SQ<-aov(SQpercent_loss_per_hour~Site, data= herb_data) 
qqp(model_SQ)
bartlett.test(SQpercent_loss_per_hour ~ Site, data = herb_data) 

#Kruskal-Wallis (non-parametric version of ANOVA)
kruskal_result <- kruskal.test(per_loss_hr ~ Site, data = herb_data) 
print(kruskal_result)

#more than two groups, with significant difference found, post-hoc is needed, pairwise wilcox test will tell us which is which
pairwise_result <- pairwise.wilcox.test(herb_data$per_loss_hr, herb_data$Site, p.adjust.method = "BH")
print(pairwise_result)

# Dunn's Test
if(kruskal_result$p.value < 0.05) {
  cat("\n--- Dunn's Test Post-Hoc Analysis ---\n")
  dunn_result <- dunnTest(herb_data$per_loss_hr, herb_data$Site, method = "bh")
  print(dunn_result)
  
  # Extract significant comparisons
  sig_comparisons <- dunn_result$res[dunn_result$res$P.adj < 0.05, ]
  if(nrow(sig_comparisons) > 0) {
    cat("\nSignificant pairwise comparisons (p < 0.05):\n")
    print(sig_comparisons)
  } else {
    cat("\nNo significant pairwise comparisons at p < 0.05\n")
  }
}
# All pairwise comparisons from Dunn's test
dunn_result$res

sum <- summarySE(herb_data, measurevar = "per_loss_hr", groupvars = "Site", na.rm = TRUE)
# Make sure Site is a character first, then reorder as factor
sum$Site <- as.character(sum$Site)
sum$Site <- factor(sum$Site, levels = c("JetSki", "Hilton", "Church", "West"))

# Check: print levels to confirm
print(levels(sum$Site))  # should be Jet_Ski, Hilton, Church, West

# Plot
dataplot1 <- ggplot(sum, aes(x = Site, y = per_loss_hr, fill = Site)) +
  geom_bar(width=0.9,stat = "identity", position = "dodge") +
  geom_errorbar(width=0.3,aes(ymin = per_loss_hr - se, ymax = per_loss_hr + se),
                size = 0.8, width = 0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#D55E00", "#e69f00", "#56B4E9", "#0072B2")) +
  labs(fill = "Site") +
  xlab("Site") +
  ylab("Percent Mass Consumed per Hour") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=20,face="bold"),
        legend.title=element_text(size=20,face="bold"),
        axis.text.x =element_text(size=20,face="bold"),
        axis.text.y=element_text(size=20,face="bold"))+
  geom_bar(width=0.9,stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.5)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(sum$per_loss_hr + sum$se) + 1))+
  scale_x_discrete(labels=c("JetSki",'Hilton',"Church","West"))
dataplot1


#ggsave("HerbivoryAssayPlot.png", plot = dataplot1, device=png, width = 10, height = 10, units = "in")