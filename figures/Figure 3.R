###############################################################
###Figure 3
###############################################################

#Note: inputs come from code_base.R

###Library
library(ggplot2)
library(ggpubr)
library(data.table)

###Components
opt_ref_LMA <- fread("opt_ref_LMA.csv")
opt_ref_WC <- fread("opt_ref_WC.csv")
opt_ref_EWT <- fread("opt_ref_EWT.csv")
opt_cwt_LMA <- fread("opt_cwt_LMA.csv")
opt_cwt_WC <- fread("opt_cwt_WC.csv")
opt_cwt_EWT <- fread("opt_cwt_EWT.csv")

opt_ref_LMA$Spectra <- as.factor("Reflectance")
opt_ref_WC$Spectra <- as.factor("Reflectance")
opt_ref_EWT$Spectra <- as.factor("Reflectance")
opt_cwt_LMA$Spectra <- as.factor("Wavelet")
opt_cwt_WC$Spectra <- as.factor("Wavelet")
opt_cwt_EWT$Spectra <- as.factor("Wavelet")

opt_LMA <- rbind(opt_ref_LMA, opt_cwt_LMA)
opt_LMA$Trait <- "LMA"
opt_WC <- rbind(opt_ref_WC, opt_cwt_WC)
opt_WC$Trait <- "WC"
opt_EWT <- rbind(opt_ref_EWT, opt_cwt_EWT)
opt_EWT$Trait <- "EWT"

###RSPV
ref_LMA <- fread("RMSE_ref_LMA.csv", skip = 2)
ref_WC <- fread("RMSE_ref_WC.csv", skip = 2)
ref_EWT <- fread("RMSE_ref_EWT.csv", skip = 2)
cwt_LMA <- fread("RMSE_cwt_LMA.csv", skip = 2)
cwt_WC <- fread("RMSE_cwt_WC.csv", skip = 2)
cwt_EWT <- fread("RMSE_cwt_EWT.csv", skip = 2)

ref_LMA$Spectra <- as.factor("Reflectance")
ref_WC$Spectra <- as.factor("Reflectance")
ref_EWT$Spectra <- as.factor("Reflectance")
cwt_LMA$Spectra <- as.factor("Wavelet")
cwt_WC$Spectra <- as.factor("Wavelet")
cwt_EWT$Spectra <- as.factor("Wavelet")

ref_LMA$Trait <- "LMA"
ref_WC$Trait <- "WC"
ref_EWT$Trait <- "EWT"
cwt_LMA$Trait <- "LMA"
cwt_WC$Trait <- "WC"
cwt_EWT$Trait <- "EWT"

LMA <- rbind(ref_LMA, cwt_LMA)
WC <- rbind(ref_WC, cwt_WC)
EWT <- rbind(ref_EWT, cwt_EWT)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

LMA_summary <- as.data.table(t(apply(LMA[,1:100], 1, data_summary)))
WC_summary <- as.data.table(t(apply(WC[,1:100], 1, data_summary)))
EWT_summary <- as.data.table(t(apply(EWT[,1:100], 1, data_summary)))

LMA_summary <- cbind(LMA_summary, LMA[, 101:102])
WC_summary <- cbind(WC_summary, WC[, 101:102])
EWT_summary <- cbind(EWT_summary, EWT[, 101:102])

LMA_summary$Components <- rep(1:65, 2)
WC_summary$Components <- rep(1:65, 2)
EWT_summary$Components <- rep(1:65, 2)

###Ranges

min(LMA_summary$ymin)
max(LMA_summary$ymax)

min(WC_summary$ymin)
max(WC_summary$ymax)

min(EWT_summary$ymin)
max(EWT_summary$ymax)

###Plot-------------------------

pa <- c("#33B09F", "#B66A34")
tamano <- 11
tamano2 <- 11

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 10, 1, 1, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", linetype="solid"))

tamano <- 9
tamano2 <- 9

th2 <- theme_bw(base_size = 11) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 10, 1, 1, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = 12, color = "black"),
                                           strip.text.y = element_text(size = 12, color = "black"),
                                           strip.background = element_rect(color= "black", linetype="solid"))



A <- ggplot() +
  geom_ribbon(data = LMA_summary, aes(x = Components, ymin = ymin, ymax = ymax, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_summary, aes(x = Components, y = y, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.06, 0.21), expand = c(0, 0)) +
  geom_rect(aes(xmin= 27, xmax= 37, ymin=-Inf, ymax=+Inf), fill = pa[1], alpha=0.2) +
  geom_vline(xintercept = 34, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_rect(aes(xmin= 14, xmax= 16, ymin= -Inf, ymax= +Inf), fill = pa[2], alpha=0.2) +
  geom_vline(xintercept = 15, linetype = "dotted", color = pa[2], size = 0.5) +
  xlab("Components") +
  ylab("RMSEP")  + th 

B <- ggplot() +
  geom_ribbon(data = WC_summary, aes(x = Components, ymin = ymin, ymax = ymax, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_summary, aes(x = Components, y = y, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.023, 0.06), expand = c(0, 0)) +
  geom_rect(aes(xmin= 29, xmax= 34, ymin=-Inf, ymax=+Inf), fill = pa[1], alpha=0.2) +
  geom_vline(xintercept = 30, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_rect(aes(xmin= 12, xmax= 16, ymin= -Inf, ymax= +Inf), fill = pa[2], alpha=0.2) +
  geom_vline(xintercept = 14, linetype = "dotted", color = pa[2], size = 0.5) +
  xlab("Components") +
  ylab("RMSEP")  + th 

C <- ggplot() +
  geom_ribbon(data = EWT_summary, aes(x = Components, ymin = ymin, ymax = ymax, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_summary, aes(x = Components, y = y, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.072, 0.14), expand = c(0, 0)) +
  geom_rect(aes(xmin= 22, xmax= 27, ymin=-Inf, ymax=+Inf), fill = pa[1], alpha=0.2) +
  geom_vline(xintercept = 24, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_rect(aes(xmin= 9, xmax= 12, ymin= -Inf, ymax= +Inf), fill = pa[2], alpha=0.2) +
  geom_vline(xintercept = 11, linetype = "dotted", color = pa[2], size = 0.5) +
  xlab("Components") + 
  ylab("RMSEP")  + th 

D <- ggplot(opt_LMA, aes(x= Onesigma, fill= Spectra)) +
     geom_histogram(color= "white", alpha=0.6, position = 'identity', binwidth = 1) +
     scale_fill_manual(values= pa) +
     scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
     scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  geom_vline(xintercept = 34, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_vline(xintercept = 15, linetype = "dotted", color = pa[2], size = 0.5) +
  ylab("Frequency") + xlab("") +
  th2 + theme(legend.position="top") +  facet_wrap(~ Trait)
  
  
E <- ggplot(opt_WC, aes(x= Onesigma, fill= Spectra)) +
  geom_histogram(color= "white", alpha=0.6, position = 'identity', binwidth = 1) +
  scale_fill_manual(values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  geom_vline(xintercept = 30, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_vline(xintercept = 14, linetype = "dotted", color = pa[2], size = 0.5) +
  ylab("Frequency") + xlab("") +
  th2 + theme(legend.position="top") +  facet_wrap(~ Trait)

Fa <- ggplot(opt_EWT, aes(x= Onesigma, fill= Spectra)) +
  geom_histogram(color= "white", alpha=0.6, position = 'identity', binwidth = 1) +
  scale_fill_manual(values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  geom_vline(xintercept = 24, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_vline(xintercept = 11, linetype = "dotted", color = pa[2], size = 0.5) +
  ylab("Frequency") + xlab("") +
  th2 + theme(legend.position="top") +  facet_wrap(~ Trait)


Z <- ggarrange(D, E, Fa, A, B, C,
               ncol = 3, nrow = 2, align = "hv", 
               widths = c(2.5, 2.5, 2.5), 
               heights = c(1, 2),
               labels = c("a", "b", "c", "d"), 
               font.label = list(size = 14, color = "black", face = "plain", family = NULL),
               label.x = 0.90,
               label.y = 0.99,
               common.legend = TRUE)

tiff("Figure_3.tif", width = 21, height = 10, units = "cm", res = 600)

Z

dev.off()
