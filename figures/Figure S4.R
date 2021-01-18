###############################################################
###Figure S4
###############################################################

#Note: inputs come from code_base.R

library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)
library(Rmisc)

###Data
#Read data

LMA_coef <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/03-Coefficients/LMA_coefficients.csv", header = TRUE)
LMA_coef <- LMA_coef[, !"iteration"]
LMA_coef <- LMA_coef[, !"intercept"]
LMA_coef[Spectra == "CWT", Spectra := "Wavelet"]
WC_coef <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/03-Coefficients/WC_coefficients.csv", header = TRUE)
WC_coef <- WC_coef[, !"iteration"]
WC_coef <- WC_coef[, !"intercept"]
WC_coef[Spectra == "CWT", Spectra := "Wavelet"]
EWT_coef <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/03-Coefficients/EWT_coefficients.csv", header = TRUE)
EWT_coef <- EWT_coef[, !"iteration"]
EWT_coef <- EWT_coef[, !"intercept"]
EWT_coef[Spectra == "CWT", Spectra := "Wavelet"]

#Summary
summaryfun <- function(x)list(mean = mean(x), 
                              nsd = mean(x) - sd(x), 
                              psd = mean(x) + sd(x))

stats <- rep(c("mean", "nsd", "psd"), 2)

LMA_summary <- cbind(stats, LMA_coef[, lapply(.SD, summaryfun), by = c("Spectra")])
WC_summary <- cbind(stats, WC_coef[, lapply(.SD, summaryfun), by = c("Spectra")])
EWT_summary <- cbind(stats, EWT_coef[, lapply(.SD, summaryfun), by = c("Spectra")])

LMA_melt <- melt(LMA_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD, 
                 value.name = "coef")

WC_melt <- melt(WC_summary, id.vars=c("Spectra", "stats"),
                measure.vars = .SD, 
                value.name = "coef")

EWT_melt <- melt(EWT_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD, 
                 value.name = "coef")

LMA_melt$variable <- as.numeric(as.character(LMA_melt$variable))
WC_melt$variable <- as.numeric(as.character(WC_melt$variable))
EWT_melt$variable <- as.numeric(as.character(EWT_melt$variable))

LMA_melt$coef <- as.numeric(LMA_melt$coef)
WC_melt$coef <- as.numeric(WC_melt$coef)
EWT_melt$coef <- as.numeric(EWT_melt$coef)

LMA_dcast <- dcast(LMA_melt, Spectra + variable ~ stats, value.var = "coef")
WC_dcast <- dcast(WC_melt, Spectra + variable ~ stats, value.var = "coef")
EWT_dcast <- dcast(EWT_melt, Spectra + variable ~ stats, value.var = "coef")

LMA_dcast$Spectra <- factor(LMA_dcast$Spectra, levels = c("Reflectance", "Wavelet"))
WC_dcast$Spectra <- factor(WC_dcast$Spectra, levels = c("Reflectance", "Wavelet"))
EWT_dcast$Spectra <- factor(EWT_dcast$Spectra, levels = c("Reflectance", "Wavelet"))

###Modify frames

LMA_vis <- subset(LMA_dcast, variable <= 1)
LMA_vis$Region <- "VIS-NIR" 
LMA_vis$Trait <- "LMA" 

LMA_lwir <- subset(LMA_dcast, variable >= 1)
LMA_lwir$Region <- "MLWIR"
LMA_lwir$Trait <- "LMA"

WC_vis <- subset(WC_dcast, variable <= 1)
WC_vis$Region <- "VIS-NIR"
WC_vis$Trait <- "WC"

WC_lwir <- subset(WC_dcast, variable >= 1)
WC_lwir$Region <- "MLWIR"
WC_lwir$Trait <- "WC"

EWT_vis <- subset(EWT_dcast, variable <= 1)
EWT_vis$Region <- "VIS-NIR"
EWT_vis$Trait <- "EWT"

EWT_lwir <- subset(EWT_dcast, variable >= 1)
EWT_lwir$Region <- "MLWIR"
EWT_lwir$Trait <- "EWT"

###Plot

pa <- c("#e66101", "#5e3c99")
tamano <- 14
tamano2 <- 12

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

range <- c(-1, 1)

A <- ggplot() +
  geom_hline(yintercept= 0, linetype= "solid", color = "grey", size = 0.5) +
  geom_ribbon(data = LMA_vis, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_vis, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(-3.1, 3.1), expand = c(0, 0), breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("", -2, "", 0, "", 2, "")) +
  th +
  facet_grid(. ~ Region)

B <- ggplot() +
  geom_hline(yintercept= 0, linetype= "solid", color = "grey", size = 0.5) +
  geom_ribbon(data = LMA_lwir, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_lwir, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.5, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(-3.1, 3.1), expand = c(0, 0), breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("", -2, "", 0, "", 2, "")) +
  th +
  facet_grid(. ~ Region)

C <- ggplot() +
  geom_hline(yintercept= 0, linetype= "solid", color = "grey", size = 0.5) +
  geom_ribbon(data = WC_vis, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_vis, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("PLSR coefficients") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(-0.75, 0.75), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

D <- ggplot() +
  geom_hline(yintercept= 0, linetype= "solid", color = "grey", size = 0.5) +
  geom_ribbon(data = WC_lwir, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_lwir, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.5, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(-0.75, 0.75), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

E <- ggplot() +
  geom_hline(yintercept= 0, linetype= "solid", color = "grey", size = 0.5) +
  geom_ribbon(data = EWT_vis, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_vis, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(-1.9, 1.9), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

Fa <- ggplot() +
  geom_hline(yintercept= 0, linetype= "solid", color = "grey", size = 0.5) +
  geom_ribbon(data = EWT_lwir, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_lwir, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.5, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(-1.9, 1.9), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)



figure <- ggarrange(A, B, C, D, E, Fa, 
                    ncol = 2, nrow = 3,  align = "hv", 
                    widths = c(2, 4), 
                    heights = c(2, 2, 2),
                    common.legend = TRUE)


tiff("Figure_S4.tif", width = 25, height = 15, units = "cm", res = 600)

annotate_figure(figure,
                bottom = text_grob(expression(paste("Wavelength (", mu, "m)", sep = "")), color = "black", size = 14))

dev.off()
