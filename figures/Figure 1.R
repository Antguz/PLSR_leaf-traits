###############################################################
###Figure 1
###############################################################

###Libraries
library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)

####Select and prepare data
data <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/01-traits.csv")
data <- data[, c(1:11)]
data[Life_form == "Liana", Life_form := "Lianas"]
data[Life_form == "Tree", Life_form := "Trees"]
pa <- c("#e66101", "#5e3c99")
color <- "grey75"

#Plot details
tamano <- 12
tamano2 <- 10
mar <- theme(plot.margin = margin(0, 0, 0, 0, "pt"))

#Create plots
LMA <- ggplot(data, aes(x = Life_form, y = LMA, fill = Life_form)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Life form", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .70, outlier.shape = NA) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  ylab(  expression(paste("LMA (g m"^-2, ")", sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none")

WC <- ggplot(data, aes(x = Life_form, y = WC, fill = Life_form)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Life form", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .70, outlier.shape = NA) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  ylab(  expression(paste("WC (%)", sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none")

EWT <- ggplot(data, aes(x = Life_form, y = EWT, fill = Life_form)) +
  geom_point(shape = 21, size= 1, position = position_jitterdodge(), color="white", alpha= 0.8) +
  geom_violin(alpha=0.4, position = position_dodge(width = .75), size= 0.5, color="black") +
  scale_fill_manual("Life form", values= pa) +
  geom_boxplot(width=0.1, color = "white", alpha = .70, outlier.shape = NA) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  ylab(  expression(paste("EWT (g m"^-2, ")", sep = "")))  +
  xlab(  c("Life form")  )  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar +
  theme(legend.position = "none")

#Transform
data$LMA_log <- log10(data$LMA)
data$WC_log <- log10(data$WC)
data$EWT_log <- log10(data$EWT)

#Create histograms
logLMA <- ggplot(data, aes(x= log10(LMA))) + 
  geom_density(fill= color, colour = "black", alpha = 0.5) +
  rotate() + theme_void() + mar

logWC <- ggplot(data, aes(x= log10(WC))) + 
  geom_density(fill= color, colour = "black", alpha = 0.5) +
  rotate() + theme_void() + mar

logEWT <- ggplot(data, aes(x= log10(EWT))) + 
  geom_density(fill= color, colour = "black", alpha = 0.5) +
  rotate() + theme_void() + mar

#Merge panels
Figure_1 <- ggarrange(LMA, logLMA, WC, logWC, EWT, logEWT,
                      ncol = 2, nrow = 3,  align = "hv", 
                      widths = c(2, 1), 
                      heights = c(2, 2, 2),
                      labels = c("a", "", "b", "", "c", ""), 
                      font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                      label.x = 0.25,
                      label.y = 0.99,
                      common.legend = FALSE)
#Export figure
tiff("Figure_1.tif", width = 9.5, height = 13, units = "cm", res = 600)

Figure_1

dev.off()

