###############################################################
### Figure S1
###############################################################

#Note: inputs come from selection_scales_CWT.R

###Libraries
library(ggplot2)
library(ggpubr)
library(data.table)

####Select and prepare data
scales <- fread("/home/antguz/Documents/PLSR-models/Data/01-selection of scales/scales.csv")
scales$ID <- 1:nrow(scales)
colnames(scales)[10] <- "names"
scales <- scales[10:381]
LMA <-fread("/home/antguz/Documents/PLSR-models/Data/01-selection of scales/LMA_scales_complete.csv")
WC <- fread("/home/antguz/Documents/PLSR-models/Data/01-selection of scales/WC_scales_complete.csv")
EWT <- fread("/home/antguz/Documents/PLSR-models/Data/01-selection of scales/EWT_scales_complete.csv")

#Data melt
scales$Scales <- with(scales, paste(V1, V2, V3, V4, V5, V6, V7, V8, V9, sep = ", "))

melt_scales <- melt(scales, id.vars = c("names", "count", "ID"),
             measure.vars = c("V1", "V2", "V3", "V4", "V5"))

melt_scales <- na.exclude(melt_scales)

#Data merge for scales and traits

LMA <- merge(scales, LMA, by = "Scales")
LMA <- LMA[order(ID)]
LMA$Trait <- "LMA"

WC <- merge(scales, WC, by = "Scales")
WC <- WC[order(ID)]
WC$Trait <- "WC"

EWT <- merge(scales, EWT, by = "Scales")
EWT <- EWT[order(ID)]
EWT$Trait <- "EWT"

data <- rbind(LMA, WC, EWT)
data$Trait <- as.factor(data$Trait)
data$Trait <- factor(data$Trait, levels = c("LMA", "WC", "EWT"))

###Plot initial parameters

pa <- c("#1b9e77", "#d95f02", "#7570b3")
tamano <- 13
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", linetype="solid"))

point_size <- 0.3

op <- range(data$Onesigma_comp)
rsq <- range(data$training_Rsq)
rmse <- range(data$training_RMSE)

###Plot scales
a <- ggplot(melt_scales, aes(value, ID)) +
  geom_hline(yintercept= 270, linetype="dashed", color = "red", size = 0.4, alpha = 0.8) +
  geom_hline(yintercept= c(45.5, 129.5, 255.5), linetype= "solid", color = "grey50") +
  geom_point(size = point_size) +
  scale_y_reverse(limits = c(382, 9), expand = c(0.00, 0.00)) +
  scale_x_continuous(limits = c(1, 9), expand = c(0.05, 0.05), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  xlab("Scales") + th +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) 
  

###Plot number of components

b <- ggplot(data, aes(Onesigma_comp, ID)) +
  geom_hline(yintercept= 270, linetype="dashed", color = "red", size = 0.4, alpha = 0.8) +
  geom_hline(yintercept= c(45.5, 129.5, 255.5), linetype= "solid", color = "grey50") +
  scale_y_reverse(limits = c(382, 9), expand = c(0.00, 0.00)) +
  scale_x_continuous(limits = c(op[1], op[2]), expand = c(0.0, 0.0), breaks = c(7, 10, 13, 16)) +
  geom_point(aes(color = Trait), size = point_size) +
  scale_color_manual("Leaf trait", values = pa) +
  xlab("Components") + th +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size= 2)))

c <- ggplot(data, aes(training_Rsq, ID)) +
  geom_hline(yintercept= 270, linetype="dashed", color = "red", size = 0.4, alpha = 0.8) +
  geom_hline(yintercept= c(45.5, 129.5, 255.5), linetype= "solid", color = "grey50") +
  scale_y_reverse(limits = c(382, 9), expand = c(0.00, 0.00)) +
  geom_point(aes(color = Trait), size = point_size) +
  scale_color_manual("Leaf trait", values = pa) +
  scale_x_continuous(limits = c(rsq[1], 1), expand = c(0.0, 0.0), breaks = c(0.7, 0.8, 0.9, 1)) +
  xlab(expression(paste("R"^2, sep = ""))) + th +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size= 2)))


d <- ggplot(data, aes(training_RMSE, ID)) +
  geom_hline(yintercept= 270, linetype="dashed", color = "red", size = 0.4, alpha = 0.8) +
  geom_hline(yintercept= c(45.5, 129.5, 255.5), linetype= "solid", color = "grey50") +
  scale_y_reverse(limits = c(382, 9), expand = c(0.00, 0.00)) +
  geom_point(aes(color = Trait), size = point_size) +
  scale_color_manual("Leaf trait", values = pa) +
  scale_x_continuous(limits = c(rmse[1], rmse[2]), expand = c(0.0, 0.0)) +
  xlab("RMSE") + th +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size= 2)))

fig <- ggarrange(a, b, c, d,
                 ncol = 4, nrow = 1,  align = "hv", 
                 widths = c(2, 3, 3, 3), 
                 labels = NULL, 
                 font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                 label.x = 0.28,
                 label.y = 0.83,
                 heights = c(4),
                 common.legend = TRUE)


tiff("Figure_S1.tif", width = 20, height = 21, units = "cm", res = 600)

fig

dev.off()

  
