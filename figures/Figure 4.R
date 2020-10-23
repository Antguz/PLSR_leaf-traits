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

ref_LMA_VIP <- fread("ref_LMA_VIP.csv")
ref_WC_VIP <- fread("ref_WC_VIP.csv")
ref_EWT_VIP <- fread("ref_EWT_VIP.csv")
cwt_LMA_VIP <- fread("cwt_LMA_VIP.csv")
cwt_WC_VIP <- fread("cwt_WC_VIP.csv")
cwt_EWT_VIP <- fread("cwt_EWT_VIP.csv")

vis_ref <- fread("02-vis_ressav.csv")
lwir_ref <- fread("02-lwir_ressav.csv")

vis_ref <- subset(vis_ref, Wavelength >= 0.45 & Wavelength <= 1.0) #Subset to a especific spectral range
lwir_ref <- subset(lwir_ref, Wavelength >= 2.6 & Wavelength <= 13) #Subset to a especific spectral range

#Get wavelength
vis_wave <- vis_ref$Wavelength
lwir_wave <- lwir_cwt$Wavelength

Wavelength <- c(vis_wave, lwir_wave)

###Prepare the data of VIP

summary_data <- function(x) {
  mean <- mean(x)
  sd_lower <- mean - sd(x)
  sd_upper <- mean + sd(x)
  return(c(sd_upper = sd_upper, mean = mean, sd_lower = sd_lower))
}

val_ref_LMA <- as.data.table(t(apply(ref_LMA_VIP, 1, summary_data)))
val_ref_WC <- as.data.table(t(apply(ref_WC_VIP, 1, summary_data)))
val_ref_EWT <- as.data.table(t(apply(ref_EWT_VIP, 1, summary_data)))
val_cwt_LMA <- as.data.table(t(apply(cwt_LMA_VIP, 1, summary_data)))
val_cwt_WC <- as.data.table(t(apply(cwt_WC_VIP, 1, summary_data)))
val_cwt_EWT <- as.data.table(t(apply(cwt_EWT_VIP, 1, summary_data)))

###Modify frames

val_ref_LMA[, c("Spectra", "Wavelength") := list("Reflectance", Wavelength)]
val_ref_WC[, c("Spectra", "Wavelength") := list("Reflectance", Wavelength)]
val_ref_EWT[, c("Spectra", "Wavelength") := list("Reflectance", Wavelength)]
val_cwt_LMA[, c("Spectra", "Wavelength") := list("Wavelet", Wavelength)]
val_cwt_WC[, c("Spectra", "Wavelength") := list("Wavelet", Wavelength)]
val_cwt_EWT[, c("Spectra", "Wavelength") := list("Wavelet", Wavelength)]

LMA <- rbind(val_ref_LMA, val_cwt_LMA)
WC <- rbind(val_ref_WC, val_cwt_WC)
EWT <- rbind(val_ref_EWT, val_cwt_EWT)

LMA_vis <- subset(LMA, Wavelength <= 1)
LMA_vis$Region <- "VIS-NIR" 
LMA_vis$Trait <- "LMA" 
LMA_vis$Spectra <- as.factor(LMA_vis$Spectra)
LMA_vis$Spectra <- factor(LMA_vis$Spectra, levels = c("Reflectance", "Wavelet"))

LMA_lwir <- subset(LMA, Wavelength >= 1)
LMA_lwir$Region <- "MLWIR"
LMA_lwir$Trait <- "LMA"
LMA_lwir$Spectra <- as.factor(LMA_lwir$Spectra)
LMA_lwir$Spectra <- factor(LMA_lwir$Spectra, levels = c("Reflectance", "Wavelet"))

WC_vis <- subset(WC, Wavelength <= 1)
WC_vis$Region <- "VIS-NIR"
WC_vis$Trait <- "WC"
WC_vis$Spectra <- as.factor(WC_vis$Spectra)
WC_vis$Spectra <- factor(WC_vis$Spectra, levels = c("Reflectance", "Wavelet"))

WC_lwir <- subset(WC, Wavelength >= 1)
WC_lwir$Region <- "MLWIR"
WC_lwir$Trait <- "WC"
WC_lwir$Spectra <- as.factor(WC_lwir$Spectra)
WC_lwir$Spectra <- factor(WC_lwir$Spectra, levels = c("Reflectance", "Wavelet"))

EWT_vis <- subset(EWT, Wavelength <= 1)
EWT_vis$Region <- "VIS-NIR"
EWT_vis$Trait <- "EWT"
EWT_vis$Spectra <- as.factor(EWT_vis$Spectra)
EWT_vis$Spectra <- factor(EWT_vis$Spectra, levels = c("Reflectance", "Wavelet"))

EWT_lwir <- subset(EWT, Wavelength >= 1)
EWT_lwir$Region <- "MLWIR"
EWT_lwir$Trait <- "EWT"
EWT_lwir$Spectra <- as.factor(EWT_lwir$Spectra)
EWT_lwir$Spectra <- factor(EWT_lwir$Spectra, levels = c("Reflectance", "Wavelet"))

###Prepare data for bands of importance

bands <- function(data, quant = 0.75) {
  
  percentile <- quantile(data[, 2], quant)
  
  valid <- data$mean >= percentile
  
  data$importance[valid] <- "YES"
  data$importance[!valid] <- "NO"
  
  frame <- data.frame(star = rep(NA, 50), end = rep(NA, 50))
  
  row_star <- 1
  row_end <- 1
  
  for(i in 2:(nrow(data)-1)) {
    
    if(data$importance[i-1] == "NO") {
      if(data$importance[i] == "YES") {
        frame$star[row_star] <- data$Wavelength[i]
        row_star <- row_star + 1
      }
    }
    
    if(data$importance[i] == "YES") {
      if(data$importance[i+1] == "NO") {
        frame$end[row_end] <- data$Wavelength[i]
        row_end <- row_end + 1
      }  
    }
    
  }
  
  if(is.na(frame$end[row_end])) {
    frame$end[row_star-1] <- frame$star[row_star-1] 
  }
  
  frame <- na.exclude(frame)
  return(frame)
}

perc <- 0.95

importance_ref_LMA <- bands(as.data.frame(val_ref_LMA), perc)
importance_ref_WC <- bands(as.data.frame(val_ref_WC), perc)
importance_ref_EWT <- bands(as.data.frame(val_ref_EWT), perc)
importance_cwt_LMA <- bands(as.data.frame(val_cwt_LMA), perc)
importance_cwt_WC <- bands(as.data.frame(val_cwt_WC), perc)
importance_cwt_EWT <- bands(as.data.frame(val_cwt_EWT), perc)

###Plot

pa <- c("#33B09F", "#B66A34")
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
  geom_rect(data = importance_ref_LMA, aes(xmin = star - 0.0005, xmax= end+0.0005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[1], alpha = 0.25) +
  geom_rect(data = importance_cwt_LMA, aes(xmin = star - 0.0005, xmax= end+0.0005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[2], alpha = 0.25) +
  geom_ribbon(data = LMA_vis, aes(x = Wavelength, ymin = sd_lower, ymax = sd_upper, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_vis, aes(x = Wavelength, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("PLSR VIP") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)



B <- ggplot() +
  geom_rect(data = importance_ref_LMA, aes(xmin = star - 0.005, xmax= end+0.005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[1], alpha = 0.25) +
  geom_rect(data = importance_cwt_LMA, aes(xmin = star - 0.005, xmax= end+0.005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[2], alpha = 0.25) +
  geom_ribbon(data = LMA_lwir, aes(x = Wavelength, ymin = sd_lower, ymax = sd_upper, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_lwir, aes(x = Wavelength, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.6, 13), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

LMA <- rbind(LMA_vis, LMA_lwir)
C <- ggdensity(LMA, "mean", fill = "Spectra", palette = pa) + 
     scale_x_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
     rotate() + clean_theme() + theme(legend.position = "none") +
     facet_grid(. ~ Trait)
  

D <- ggplot() +
  geom_rect(data = importance_ref_WC, aes(xmin = star - 0.0005, xmax= end+0.0005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[1], alpha = 0.25) +
  geom_rect(data = importance_cwt_WC, aes(xmin = star - 0.0005, xmax= end+0.0005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[2], alpha = 0.25) +
  geom_ribbon(data = WC_vis, aes(x = Wavelength, ymin = sd_lower, ymax = sd_upper, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_vis, aes(x = Wavelength, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("PLSR VIP") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

E <- ggplot() +
  geom_rect(data = importance_ref_WC, aes(xmin = star - 0.005, xmax= end+0.005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[1], alpha = 0.25) +
  geom_rect(data = importance_cwt_WC, aes(xmin = star - 0.005, xmax= end+0.005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[2], alpha = 0.25) +
  geom_ribbon(data = WC_lwir, aes(x = Wavelength, ymin = sd_lower, ymax = sd_upper, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_lwir, aes(x = Wavelength, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.6, 13), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

WC <- rbind(WC_vis, WC_lwir)
Fa <- ggdensity(WC, "mean", fill = "Spectra", palette = pa) + 
      scale_x_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
      rotate() + clean_theme() + theme(legend.position = "none") + facet_grid(. ~ Trait)

G <- ggplot() +
  geom_rect(data = importance_ref_EWT, aes(xmin = star - 0.0005, xmax= end+0.0005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[1], alpha = 0.25) +
  geom_rect(data = importance_cwt_EWT, aes(xmin = star - 0.0005, xmax= end+0.0005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[2], alpha = 0.25) +
  geom_ribbon(data = EWT_vis, aes(x = Wavelength, ymin = sd_lower, ymax = sd_upper, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_vis, aes(x = Wavelength, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab("PLSR VIP") +
  xlab(NULL) +
  scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

H <- ggplot() +
  geom_rect(data = importance_ref_EWT, aes(xmin = star - 0.005, xmax= end+0.005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[1], alpha = 0.25) +
  geom_rect(data = importance_cwt_EWT, aes(xmin = star - 0.005, xmax= end+0.005, ymin=-Inf, ymax=Inf), color = NA, fill = pa[2], alpha = 0.25) +
  geom_ribbon(data = EWT_lwir, aes(x = Wavelength, ymin = sd_lower, ymax = sd_upper, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_lwir, aes(x = Wavelength, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(limits = c(2.6, 13), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13)) +
  scale_y_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
  th +
  facet_grid(. ~ Region)

EWT <- rbind(EWT_vis, EWT_lwir)
I <- ggdensity(EWT, "mean", fill = "Spectra", palette = pa) + 
     scale_x_continuous(limits = c(0, 8.15), expand = c(0, 0)) +
     rotate() + clean_theme() + theme(legend.position = "none") + facet_grid(. ~ Trait)


figure <- ggarrange(A, B, C, D, E, Fa, G, H, I,
                    ncol = 3, nrow = 3,  align = "hv", 
                    widths = c(2, 4, 1), 
                    heights = c(2, 2, 2), 
                    labels = c("a", "b", "", "c", "d", "", "e", "f", ""), 
                    font.label = list(size = 14, color = "black", face = "plain", family = NULL),
                    label.x = 0.22,
                    label.y = 0.78,
                    common.legend = TRUE)


tiff("Figure_4a.tif", width = 25, height = 15, units = "cm", res = 600)

annotate_figure(figure,
                bottom = text_grob(expression(paste("Wavelength (", mu, "m)", sep = "")), color = "black", size = 14))

dev.off()
