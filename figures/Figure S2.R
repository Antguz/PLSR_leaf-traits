###############################################################
###Figure S2
###############################################################

#Note: inputs come from code_base.R

###Libraries
library(Rmisc)
library(ggplot2)
library(data.table)
library(ggpubr)
library(PupillometryR)

####Select and prepare data
training <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/traits_training.csv")
training$Process <- "Training"
testing <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/traits_testing.csv")
testing$Process <- "Testing"

data <- rbind(training, testing)
colnames(data)[3:5] <- c("LMA", "WC", "EWT")
data$LMA <- 10^data$LMA
data$WC <- 10^data$WC
data$EWT <- 10^data$EWT
data[Life_form == "Tree", Life_form := "Trees"]
data[Life_form == "Liana", Life_form := "Lianas"]

data$Life_form <- as.factor(data$Life_form)
data$Life_form <- factor(data$Life_form, levels = c("Lianas", "Trees"))

data$Process <- as.factor(data$Process)
data$Process <- factor(data$Process, levels = c("Training", "Testing"))

pa <- c("#e66101", "#5e3c99")
tamano <- 12
tamano2 <- 10
mar <- theme(plot.margin = margin(0, 0, 0, 0, "pt"))

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))



###Plots

LMA <- summarySE(data, measurevar = "LMA",
                     groupvars=c("Life_form", "Process"))

LMA <- ggplot() +
  geom_flat_violin(data = data, aes(x = Life_form, y = LMA, fill = Process), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .15, colour = "white") +
  geom_point(data = data, aes(x = as.numeric(Life_form)-.15, y = LMA, colour = Process), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = data, aes(x = Life_form, y = LMA, fill = Process), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  scale_fill_manual(values = c("Training" = pa[1], "Testing" = pa[2])) +
  scale_colour_manual(values = c("Training" = pa[1], "Testing" = pa[2])) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  ylab(  expression(paste("LMA (g m"^-2, ")", sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar
  
WC <- ggplot() +
  geom_flat_violin(data = data, aes(x = Life_form, y = WC, fill = Process), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .15, colour = "white") +
  geom_point(data = data, aes(x = as.numeric(Life_form)-.15, y = WC, colour = Process), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = data, aes(x = Life_form, y = WC, fill = Process), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  scale_fill_manual(values = c("Training" = pa[1], "Testing" = pa[2])) +
  scale_colour_manual(values = c("Training" = pa[1], "Testing" = pa[2])) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  ylab(  expression(paste("WC (%)", sep = "")))  +
  xlab(NULL)  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar

EWT <- ggplot() +
  geom_flat_violin(data = data, aes(x = Life_form, y = EWT, fill = Process), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .15, colour = "white") +
  geom_point(data = data, aes(x = as.numeric(Life_form)-.15, y = EWT, colour = Process), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = data, aes(x = Life_form, y = EWT, fill = Process), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  scale_fill_manual(values = c("Training" = pa[1], "Testing" = pa[2])) +
  scale_colour_manual(values = c("Training" = pa[1], "Testing" = pa[2])) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  ylab(  expression(paste("EWT (g m"^-2, ")", sep = "")))  +
  xlab("Life forms")  +
  theme_classic(base_size = tamano) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = margin(4, 4, 0, 0, "mm")) +
  theme(axis.text.x = element_text(color = "black", size = tamano2), axis.text.y = element_text(color = "black", size = tamano2)) + mar



tiff("Figure_S2.tif", width = 8, height = 15, units = "cm", res = 600)

ggarrange(LMA, WC, EWT, 
          ncol = 1, nrow = 3,  align = "hv", 
          widths = c(2), 
          heights = c(2, 2, 2),
          labels = c("a", "b", "c"), 
          font.label = list(size = 14, color = "black", face = "plain", family = NULL),
          label.x = 0.20,
          label.y = 0.99,
          common.legend = TRUE)

dev.off()
