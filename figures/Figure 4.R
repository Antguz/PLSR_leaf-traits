###############################################################
###Figure 4
###############################################################

#Note: inputs come from code_base.R

###Libraries
library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)
library(Rmisc)

###Data and prepare data
#Read data

LMA_VIP <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/04-VIP/LMA_VIP.csv", header = TRUE)
LMA_VIP <- LMA_VIP[, !"iteration"]
LMA_VIP[Spectra == "CWT", Spectra := "Wavelet"]
WC_VIP <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/04-VIP/WC_VIP.csv", header = TRUE)
WC_VIP <- WC_VIP[, !"iteration"]
WC_VIP[Spectra == "CWT", Spectra := "Wavelet"]
EWT_VIP <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/04-VIP/EWT_VIP.csv", header = TRUE)
EWT_VIP <- EWT_VIP[, !"iteration"]
EWT_VIP[Spectra == "CWT", Spectra := "Wavelet"]

###Prepare the data of VIP
summaryfun <- function(x)list(mean = mean(x), 
                              nsd = mean(x) - sd(x), 
                              psd = mean(x) + sd(x))

stats <- rep(c("mean", "nsd", "psd"), 2)

LMA_summary <- cbind(stats, LMA_VIP[, lapply(.SD, summaryfun), by = c("Spectra")])
WC_summary <- cbind(stats, WC_VIP[, lapply(.SD, summaryfun), by = c("Spectra")])
EWT_summary <- cbind(stats, EWT_VIP[, lapply(.SD, summaryfun), by = c("Spectra")])

LMA_melt <- melt(LMA_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD, 
                 value.name = "VIP")

WC_melt <- melt(WC_summary, id.vars=c("Spectra", "stats"),
                measure.vars = .SD, 
                value.name = "VIP")

EWT_melt <- melt(EWT_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD, 
                 value.name = "VIP")

LMA_melt$variable <- as.numeric(as.character(LMA_melt$variable))
WC_melt$variable <- as.numeric(as.character(WC_melt$variable))
EWT_melt$variable <- as.numeric(as.character(EWT_melt$variable))

LMA_melt$VIP <- as.numeric(LMA_melt$VIP)
WC_melt$VIP <- as.numeric(WC_melt$VIP)
EWT_melt$VIP <- as.numeric(EWT_melt$VIP)

LMA_dcast <- dcast(LMA_melt, Spectra + variable ~ stats, value.var = "VIP")
WC_dcast <- dcast(WC_melt, Spectra + variable ~ stats, value.var = "VIP")
EWT_dcast <- dcast(EWT_melt, Spectra + variable ~ stats, value.var = "VIP")

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

###Prepare data for bands of importance

peak_LMA_ref <- peakdet(v = LMA_dcast[Spectra == "Reflectance", mean], 
                   delta = quantile(LMA_dcast[Spectra == "Reflectance", mean], 0.95)/2, 
                   x = LMA_dcast[Spectra == "Reflectance", variable])
peak_LMA_ref <- peak_LMA_ref$maxtab$pos

peak_WC_ref <- peakdet(v = WC_dcast[Spectra == "Reflectance", mean], 
                   delta = quantile(WC_dcast[Spectra == "Reflectance", mean], 0.95)/2, 
                   x = WC_dcast[Spectra == "Reflectance", variable])
peak_WC_ref <- peak_WC_ref$maxtab$pos

peak_EWT_ref <- peakdet(v = EWT_dcast[Spectra == "Reflectance", mean], 
                  delta = quantile(EWT_dcast[Spectra == "Reflectance", mean], 0.95)/2, 
                  x = EWT_dcast[Spectra == "Reflectance", variable])
peak_EWT_ref <- peak_EWT_ref$maxtab$pos


peak_LMA_cwt <- peakdet(v = LMA_dcast[Spectra == "Wavelet", mean], 
                        delta = quantile(LMA_dcast[Spectra == "Wavelet", mean], 0.95)/2, 
                        x = LMA_dcast[Spectra == "Wavelet", variable])
peak_LMA_cwt <- peak_LMA_cwt$maxtab$pos


peak_WC_cwt <- peakdet(v = WC_dcast[Spectra == "Wavelet", mean], 
                       delta = quantile(WC_dcast[Spectra == "Wavelet", mean], 0.95)/2, 
                       x = WC_dcast[Spectra == "Wavelet", variable])
peak_WC_cwt <- peak_WC_cwt$maxtab$pos


peak_EWT_cwt <- peakdet(v = EWT_dcast[Spectra == "Wavelet", mean], 
                        delta = quantile(EWT_dcast[Spectra == "Wavelet", mean], 0.95)/2, 
                        x = EWT_dcast[Spectra == "Wavelet", variable])
peak_EWT_cwt <- peak_EWT_cwt$maxtab$pos


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

A <- ggplot() +
  geom_ribbon(data = LMA_vis, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_vis, aes(x = variable, y = mean, colour = Spectra), alpha = 0.80, size = 0.5) +
  geom_vline(xintercept = peak_LMA_ref[peak_LMA_ref < 1.2], linetype = "dashed", color = pa[1], size = 0.3) +
  geom_vline(xintercept = peak_LMA_cwt[peak_LMA_cwt < 1.2], linetype = "dashed", color = pa[2], size = 0.3) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 8.8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8), labels = c(0, "", 0.4, "", 0.8)) +
  th +
  facet_grid(. ~ Region)


B <- ggplot() +
  geom_ribbon(data = LMA_lwir, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_lwir, aes(x = variable, y = mean, colour = Spectra), alpha = 0.80, size = 0.5) +
  geom_vline(xintercept = peak_LMA_ref[peak_LMA_ref > 1.2], linetype = "dashed", color = pa[1], size = 0.3) +
  geom_vline(xintercept = peak_LMA_cwt[peak_LMA_cwt > 1.2], linetype = "dashed", color = pa[2], size = 0.3) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.50, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 8.8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8), labels = c(0, "", 0.4, "", 0.8)) +
  th + theme( axis.text.y = element_blank()) +
  facet_grid(. ~ Region)

C <- ggplot() +
  geom_ribbon(data = WC_vis, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_vis, aes(x = variable, y = mean, colour = Spectra), alpha = 0.80, size = 0.5) +
  geom_vline(xintercept = peak_WC_ref[peak_WC_ref < 1.2], linetype = "dashed", color = pa[1], size = 0.3) +
  geom_vline(xintercept = peak_WC_cwt[peak_WC_cwt < 1.2], linetype = "dashed", color = pa[2], size = 0.3) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("PLSR VIP") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 8.8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8), labels = c(0, "", 0.4, "", 0.8)) +
  th +
  facet_grid(. ~ Region)

D <- ggplot() +
  geom_ribbon(data = WC_lwir, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_lwir, aes(x = variable, y = mean, colour = Spectra), alpha = 0.80, size = 0.5) +
  geom_vline(xintercept = peak_WC_ref[peak_WC_ref > 1.2], linetype = "dashed", color = pa[1], size = 0.3) +
  geom_vline(xintercept = peak_WC_cwt[peak_WC_cwt > 1.2], linetype = "dashed", color = pa[2], size = 0.3) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.50, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 8.8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8), labels = c(0, "", 0.4, "", 0.8)) +
  th + theme( axis.text.y = element_blank()) +
  facet_grid(. ~ Region)

E <- ggplot() +
  geom_ribbon(data = EWT_vis, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_vis, aes(x = variable, y = mean, colour = Spectra), alpha = 0.80, size = 0.5) +
  geom_vline(xintercept = peak_EWT_ref[peak_EWT_ref < 1.2], linetype = "dashed", color = pa[1], size = 0.3) +
  geom_vline(xintercept = peak_EWT_cwt[peak_EWT_cwt < 1.2], linetype = "dashed", color = pa[2], size = 0.3) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 8.8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8), labels = c(0, "", 0.4, "", 0.8)) +
  th +
  facet_grid(. ~ Region)

Fa <- ggplot() +
  geom_ribbon(data = EWT_lwir, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_lwir, aes(x = variable, y = mean, colour = Spectra), alpha = 0.80, size = 0.5) +
  geom_vline(xintercept = peak_EWT_ref[peak_EWT_ref > 1.2], linetype = "dashed", color = pa[1], size = 0.3) +
  geom_vline(xintercept = peak_EWT_cwt[peak_EWT_cwt > 1.2], linetype = "dashed", color = pa[2], size = 0.3) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.50, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 8.8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8), labels = c(0, "", 0.4, "", 0.8)) +
  th + theme( axis.text.y = element_blank()) + 
  facet_grid(. ~ Region)


figure <- ggarrange(A, B, C, D, E, Fa,
                    ncol = 2, nrow = 3,  align = "hv", 
                    widths = c(2, 4), 
                    heights = c(2, 2, 2), 
                    labels = c("a", "b",  "c", "d", "e", "f"), 
                    font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                    label.x = 0.22,
                    label.y = 0.78,
                    common.legend = TRUE)


tiff("Figure_4.tif", width = 25, height = 15, units = "cm", res = 600)

annotate_figure(figure,
                bottom = text_grob(expression(paste("Wavelength (", mu, "m)", sep = "")), color = "black", size = 14))

dev.off()
