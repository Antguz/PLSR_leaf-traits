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
#Read of ID codes
ID <- fread("01-traits.csv")
ID <- ID[, c(1:11)]

#Non-transformed spectra data
vis_ref <- fread("02-vis_ressav.csv")
lwir_ref <- fread("02-lwir_ressav.csv")

vis_ref <- subset(vis_ref, Wavelength >= 0.45 & Wavelength <= 1.0) #Subset to a especific spectral range
lwir_ref <- subset(lwir_ref, Wavelength >= 2.6 & Wavelength <= 13) #Subset to a especific spectral range

#Transformed spectra data
vis_cwt <- fread("03-vis_cwt.csv")
lwir_cwt <- fread("03-lwir_cwt.csv")

###Data management---------------------
#Get wavelength
vis_wave <- vis_ref$Wavelength
lwir_wave <- lwir_cwt$Wavelength

###Prepare the data
ID <- ID[, c(1)]

ref <- rbind(vis_ref, lwir_ref, use.names = FALSE)
wavelength <- ref$Wavelength
ref <- t(ref[, 2:701])
colnames(ref) <- wavelength
cwt <- rbind(vis_cwt, lwir_cwt, use.names = FALSE)
cwt <- t(cwt[, 2:701])
colnames(cwt) <- wavelength

ref <- cbind(ID, ref)
cwt <- cbind(ID, cwt)

###Get mean and sd 
ref_mean <- ref[, lapply(.SD, mean, na.rm = TRUE), by = c("Life_form")]
cwt_mean <- cwt[, lapply(.SD, mean, na.rm = TRUE), by = c("Life_form")]

ref_melt_mean <- melt(ref_mean,id.vars=c("Life_form"),
                 measure.vars = .SD,
                 value.name = "Spectra")

cwt_melt_mean <- melt(cwt_mean,id.vars=c("Life_form"),
                 measure.vars = .SD,
                 value.name = "Spectra")

ref_sd <- ref[, lapply(.SD, sd, na.rm = TRUE), by = c("Life_form")]
cwt_sd <- cwt[, lapply(.SD, sd, na.rm = TRUE), by = c("Life_form")]

ref_melt_sd <- melt(ref_sd,id.vars=c("Life_form"),
                      measure.vars = .SD,
                      value.name = "sd")

cwt_melt_sd <- melt(cwt_sd,id.vars=c("Life_form"),
                      measure.vars = .SD,
                      value.name = "sd")

ref_melt <- merge(ref_melt_mean, ref_melt_sd, by = c("Life_form", "variable"))
cwt_melt <- merge(cwt_melt_mean, cwt_melt_sd, by = c("Life_form", "variable"))

ref_melt$variable <- as.numeric(as.character(ref_melt$variable))
colnames(ref_melt)[2] <- "Wavelength"
cwt_melt$variable <- as.numeric(as.character(cwt_melt$variable))
colnames(cwt_melt)[2] <- "Wavelength"

ref_vis <- subset(ref_melt, Wavelength <= 1)
ref_vis$Region <- "VIS-NIR"
ref_lwir <- subset(ref_melt, Wavelength >= 1)
ref_lwir$Region <- "MLWIR"
cwt_vis <- subset(cwt_melt, Wavelength <= 1)
cwt_vis$Region <- "VIS-NIR"
cwt_lwir <- subset(cwt_melt, Wavelength >= 1)
cwt_lwir$Region <- "MLWIR"

###Plot---------------------------------------------------------

#initial arguments
pa <- c("#33B09F", "#B66A34")
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
  geom_ribbon(data = ref_vis, aes(x = Wavelength, ymin = Spectra-sd, ymax = Spectra+sd, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = ref_vis, aes(x = Wavelength, y = Spectra, colour = Life_form, group = Life_form), size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab("Reflectance") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0), breaks = c(0, 0.2, 0.4, 0.6), labels = c(0, 0.2, 0.4, 0.6)) +
  th +
  facet_grid(. ~ Region)
  
B <- ggplot() +
  geom_ribbon(data = ref_lwir, aes(x = Wavelength, ymin = Spectra-sd, ymax = Spectra+sd, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = ref_lwir, aes(x = Wavelength, y = Spectra, colour = Life_form, group = Life_form), size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.6, 13), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0, 0), breaks = c(0, 0.05, 0.10, 0.15), labels = c(0, 0.05, 0.10, 0.15)) +
  th +
  facet_grid(. ~ Region)


C <- ggplot() +
  geom_ribbon(data = cwt_vis, aes(x = Wavelength, ymin = Spectra-sd, ymax = Spectra+sd, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = cwt_vis, aes(x = Wavelength, y = Spectra, colour = Life_form, group = Life_form), size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab("Wavelet") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(-0.20, 0.13), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

D <- ggplot() +
  geom_ribbon(data = cwt_lwir, aes(x = Wavelength, ymin = Spectra-sd, ymax = Spectra+sd, fill = Life_form, colour = NA), alpha = 0.2) +
  geom_line(data = cwt_lwir, aes(x = Wavelength, y = Spectra, colour = Life_form, group = Life_form), size = 0.5) +
  scale_fill_manual("Life form", values= pa) +
  scale_colour_manual("Life form", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.6, 13), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(-0.20, 0.13), expand = c(0, 0)) +
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
