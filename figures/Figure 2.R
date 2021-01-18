###############################################################
###Figure 2
###############################################################

#Note: inputs come from code_base.R

###Libraries
library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)
library(Rmisc)

###Data-------------------------------------
#Reflectance spectra
ref <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/04-ref.csv")
ref <- ref[, c(1, 7:2732)]
ref[Life_form == "Liana", Life_form := "Lianas"]
ref[Life_form == "Tree", Life_form := "Trees"]

#Wavelet spectra
cwt <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/04-cwt.csv")
cwt <- cwt[, c(1, 7:2732)]
cwt[Life_form == "Liana", Life_form := "Lianas"]
cwt[Life_form == "Tree", Life_form := "Trees"]
  
#Wavelength
wavelength <- as.numeric(names(ref)[2:2727])

###Data management---------------------
###Get mean, min, max, and IC 
summaryfun <- function(x)list(mean = mean(x), 
                              min = min(x), 
                              max = max(x), 
                              CIlower = (mean(x) - qnorm(0.95)*sd(x)/sqrt(length(x))),
                              CIupper = (mean(x) + qnorm(0.95)*sd(x)/sqrt(length(x))))

stats <- rep(c("mean", "min", "max", "CIlower", "CIupper"), 2)

ref_summary <- cbind(stats, ref[, lapply(.SD, summaryfun), by = c("Life_form")])
cwt_summary <- cbind(stats, cwt[, lapply(.SD, summaryfun), by = c("Life_form")])

ref_melt <- melt(ref_summary,id.vars=c("Life_form", "stats"),
                 measure.vars = .SD, 
                 value.name = "Spectra")

ref_melt$Spectra <- as.numeric(ref_melt$Spectra)

cwt_melt <- melt(cwt_summary,id.vars=c("Life_form", "stats"),
                 measure.vars = .SD,
                 value.name = "Spectra")

cwt_melt$Spectra <- as.numeric(cwt_melt$Spectra)

ref_dcast <- dcast(ref_melt, Life_form + variable ~ stats, value.var = "Spectra")
cwt_dcast <- dcast(cwt_melt, Life_form + variable ~ stats, value.var = "Spectra")

colnames(ref_dcast)[2] <- "Wavelength"
colnames(cwt_dcast)[2] <- "Wavelength"

ref_dcast$Wavelength <- as.numeric(as.character(ref_dcast$Wavelength))
cwt_dcast$Wavelength <- as.numeric(as.character(cwt_dcast$Wavelength))

ref_vis <- subset(ref_dcast, Wavelength <= 1)
ref_vis$Region <- "VIS-NIR"
ref_lwir <- subset(ref_dcast, Wavelength >= 1)
ref_lwir$Region <- "MLWIR"
cwt_vis <- subset(cwt_dcast, Wavelength <= 1)
cwt_vis$Region <- "VIS-NIR"
cwt_lwir <- subset(cwt_dcast, Wavelength >= 1)
cwt_lwir$Region <- "MLWIR"

###Plot---------------------------------------------------------

#initial arguments
pa <- c("#e66101", "#5e3c99")
tamano <- 12
tamano2 <- 10

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", linetype="solid"))

A <- ggplot() +
  geom_ribbon(data = ref_vis, aes(x = Wavelength, ymin = min, ymax = max, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = ref_vis, aes(x = Wavelength, y = mean, colour = Life_form, group = Life_form), alpha = 0.90, size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab("Reflectance") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0), breaks = c(0, 0.15, 0.3, 0.45, 0.6), labels = c(0, "", 0.3, "", 0.6)) +
  th +
  facet_grid(. ~ Region)
  
B <- ggplot() +
  geom_ribbon(data = ref_lwir, aes(x = Wavelength, ymin = min, ymax = max, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = ref_lwir, aes(x = Wavelength, y = mean, colour = Life_form, group = Life_form), alpha = 0.90, size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.55, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 0.20), expand = c(0, 0), breaks = c(0, 0.05, 0.10, 0.15, 0.2), labels = c(0, "", 0.10, "", 0.2)) +
  th +
  facet_grid(. ~ Region)

C <- ggplot() +
  geom_ribbon(data = cwt_vis, aes(x = Wavelength, ymin = min, ymax = max, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = cwt_vis, aes(x = Wavelength, y = mean, colour = Life_form, group = Life_form), alpha = 0.90, size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab("Wavelet") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(-1.74, 1.74), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

D <- ggplot() +
  geom_ribbon(data = cwt_lwir, aes(x = Wavelength, ymin = min, ymax = max, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = cwt_lwir, aes(x = Wavelength, y = mean, colour = Life_form, group = Life_form), alpha = 0.90, size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.55, 11), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(-0.20, 0.20), expand = c(0, 0), breaks = c(-0.15, 0, 0.15), labels = c(-0.15, 0, 0.15)) +
  th +
  facet_grid(. ~ Region)

fig <- ggarrange(A, B, C, D,
                 ncol = 2, nrow = 2,  align = "hv", 
                 widths = c(2, 4), 
                 labels = c("a", "b", "c", "d"), 
                 font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                 label.x = 0.28,
                 label.y = 0.83,
                 heights = c(2, 2),
                 common.legend = TRUE)

tiff("Figure_2.tif", width = 19, height = 11, units = "cm", res = 600)

annotate_figure(fig,
                bottom = text_grob(expression(paste("Wavelength (", mu, "m)", sep = "")), color = "black", size = 14))

dev.off()

