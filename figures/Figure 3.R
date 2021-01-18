###############################################################
###Figure 3
###############################################################

#Note: inputs come from code_base.R

###Library
library(ggplot2)
library(ggpubr)
library(data.table)

###Components
opt_LMA <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/01-Optimal number/opt_LMA.csv")
opt_LMA[Spectra == "CWT", Spectra := "Wavelet"]
opt_WC <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/01-Optimal number/opt_WC.csv")
opt_WC[Spectra == "CWT", Spectra := "Wavelet"]
opt_EWT <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/01-Optimal number/opt_EWT.csv")
opt_EWT[Spectra == "CWT", Spectra := "Wavelet"]

opt_LMA$Spectra <- factor(opt_LMA$Spectra, levels = c("Reflectance", "Wavelet"))
opt_WC$Spectra <- factor(opt_WC$Spectra, levels = c("Reflectance", "Wavelet"))
opt_EWT$Spectra <- factor(opt_EWT$Spectra, levels = c("Reflectance", "Wavelet"))

###RSPV
RMSEP_LMA <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/01-Optimal number/RMSE_LMA.csv")
RMSEP_LMA <- RMSEP_LMA[, !"Trait"]
RMSEP_LMA[Spectra == "CWT", Spectra := "Wavelet"]
RMSEP_WC <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/01-Optimal number/RMSE_WC.csv")
RMSEP_WC <- RMSEP_WC[, !"Trait"]
RMSEP_WC[Spectra == "CWT", Spectra := "Wavelet"]
RMSEP_EWT <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/01-Optimal number/RMSE_EWT.csv")
RMSEP_EWT <- RMSEP_EWT[, !"Trait"]
RMSEP_EWT[Spectra == "CWT", Spectra := "Wavelet"]

RMSEP_LMA$Spectra <- factor(RMSEP_LMA$Spectra, levels = c("Reflectance", "Wavelet"))
RMSEP_WC$Spectra <- factor(RMSEP_WC$Spectra, levels = c("Reflectance", "Wavelet"))
RMSEP_EWT$Spectra <- factor(RMSEP_EWT$Spectra, levels = c("Reflectance", "Wavelet"))


summaryfun <- function(x)list(mean = mean(x), 
                              nsd = mean(x) - sd(x), 
                              psd = mean(x) + sd(x))

stats <- rep(c("mean", "nsd", "psd"), 2)

LMA_summary <- cbind(stats, RMSEP_LMA[, lapply(.SD, summaryfun), by = c("Spectra")])
WC_summary <- cbind(stats, RMSEP_WC[, lapply(.SD, summaryfun), by = c("Spectra")])
EWT_summary <- cbind(stats, RMSEP_EWT[, lapply(.SD, summaryfun), by = c("Spectra")])

LMA_melt <- melt(LMA_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD,
                 value.name = "RMSEP")

LMA_melt$RMSEP <- as.numeric(LMA_melt$RMSEP)

WC_melt <- melt(WC_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD,
                 value.name = "RMSEP")

WC_melt$RMSEP <- as.numeric(WC_melt$RMSEP)

EWT_melt <- melt(EWT_summary, id.vars=c("Spectra", "stats"),
                 measure.vars = .SD,
                 value.name = "RMSEP")

EWT_melt$RMSEP <- as.numeric(EWT_melt$RMSEP)

LMA_dcast <- dcast(LMA_melt, Spectra + variable ~ stats, value.var = "RMSEP")
LMA_dcast <- LMA_dcast[variable != "Iteration" & variable != "Intercept"]
LMA_dcast$variable <- rep(1:50, 2)

WC_dcast <- dcast(WC_melt, Spectra + variable ~ stats, value.var = "RMSEP")
WC_dcast <- WC_dcast[variable != "Iteration" & variable != "Intercept"]
WC_dcast$variable <- rep(1:50, 2)

EWT_dcast <- dcast(EWT_melt, Spectra + variable ~ stats, value.var = "RMSEP")
EWT_dcast <- EWT_dcast[variable != "Iteration" & variable != "Intercept"]
EWT_dcast$variable <- rep(1:50, 2)

###Statistics
min(LMA_dcast$nsd)
max(LMA_dcast$psd)

min(WC_dcast$nsd)
max(WC_dcast$psd)

min(EWT_dcast$nsd)
max(EWT_dcast$psd)

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

LMA_stats <- opt_LMA[, list(mode(Optimal), min(Optimal), max(Optimal)), by = Spectra] 
WC_stats <- opt_WC[, list(mode(Optimal), min(Optimal), max(Optimal)), by = Spectra] 
EWT_stats <- opt_EWT[, list(mode(Optimal), min(Optimal), max(Optimal)), by = Spectra] 

###Plot-------------------------

pa <- c("#e66101", "#5e3c99")
tamano <- 12
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
  geom_ribbon(data = LMA_dcast, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = LMA_dcast, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0), breaks = c(1, 10, 20, 30, 40, 50), labels = c(1, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0.06, 0.21), expand = c(0, 0)) +
  geom_rect(aes(xmin= 28, xmax= 30, ymin=-Inf, ymax=+Inf), fill = pa[1], alpha=0.2) +
  geom_vline(xintercept = 29, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_rect(aes(xmin= 14, xmax= 16, ymin= -Inf, ymax= +Inf), fill = pa[2], alpha=0.2) +
  geom_vline(xintercept = 15.59, linetype = "dotted", color = pa[2], size = 0.5) +
  xlab("Components") +
  ylab("RMSEP")  + th 

B <- ggplot() +
  geom_ribbon(data = WC_dcast, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = WC_dcast, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0), breaks = c(1, 10, 20, 30, 40, 50), labels = c(1, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0.023, 0.059), expand = c(0, 0)) +
  geom_rect(aes(xmin= 27, xmax= 32, ymin=-Inf, ymax=+Inf), fill = pa[1], alpha=0.2) +
  geom_vline(xintercept = 28, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_rect(aes(xmin= 12, xmax= 13, ymin= -Inf, ymax= +Inf), fill = pa[2], alpha=0.2) +
  geom_vline(xintercept = 12.59, linetype = "dotted", color = pa[2], size = 0.5) +
  xlab("Components") +
  ylab("RMSEP")  + th 

C <- ggplot() +
  geom_ribbon(data = EWT_dcast, aes(x = variable, ymin = nsd, ymax = psd, colour = NULL, fill = Spectra), alpha = 0.2) +
  geom_line(data = EWT_dcast, aes(x = variable, y = mean, colour = Spectra), size = 0.5) +
  scale_fill_manual("Spectra", values= pa) +
  scale_colour_manual("Spectra", values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0), breaks = c(1, 10, 20, 30, 40, 50), labels = c(1, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0.07, 0.1399), expand = c(0, 0)) +
  geom_rect(aes(xmin= 23, xmax= 29, ymin=-Inf, ymax=+Inf), fill = pa[1], alpha=0.2) +
  geom_vline(xintercept = 25, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_rect(aes(xmin= 13, xmax= 14, ymin= -Inf, ymax= +Inf), fill = pa[2], alpha=0.2) +
  geom_vline(xintercept = 13.5, linetype = "dotted", color = pa[2], size = 0.5) +
  xlab("Components") + 
  ylab("RMSEP")  + th 

D <- ggplot(opt_LMA, aes(x= Optimal, fill= Spectra)) +
     geom_histogram(color= "white", alpha=0.8, position = 'identity', binwidth = 1) +
     scale_fill_manual(values= pa) +
     scale_x_continuous(limits = c(1, 50), expand = c(0, 0), breaks = c(1, 10, 20, 30, 40, 50), labels = c(1, 10, 20, 30, 40, 50)) +
     scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(0, 25, 50, 75, 100), labels = c(0, "", 50, "", 100)) +
     geom_vline(xintercept = 29, linetype = "dotted", color = pa[1], size = 0.5) +
     geom_vline(xintercept = 16, linetype = "dotted", color = pa[2], size = 0.5) +
     ylab("Frequency") + xlab(NULL) +
     th2 + theme(legend.position="top") +  facet_wrap(~ Trait)
 
E <- ggplot(opt_WC, aes(x= Optimal, fill= Spectra)) +
  geom_histogram(color= "white", alpha=0.8, position = 'identity', binwidth = 1) +
  scale_fill_manual(values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0), breaks = c(1, 10, 20, 30, 40, 50), labels = c(1, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(0, 25, 50, 75, 100), labels = c(0, "", 50, "", 100)) +
  geom_vline(xintercept = 28, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_vline(xintercept = 13, linetype = "dotted", color = pa[2], size = 0.5) +
  ylab("Frequency") + xlab(NULL) +
  th2 + theme(legend.position="top") +  facet_wrap(~ Trait)

Fa <- ggplot(opt_EWT, aes(x= Optimal, fill= Spectra)) +
  geom_histogram(color= "white", alpha=0.8, position = 'identity', binwidth = 1) +
  scale_fill_manual(values= pa) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0), breaks = c(1, 10, 20, 30, 40, 50), labels = c(1, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(0, 25, 50, 75, 100), labels = c(0, "", 50, "", 100)) +
  geom_vline(xintercept = 25, linetype = "dotted", color = pa[1], size = 0.5) +
  geom_vline(xintercept = 13, linetype = "dotted", color = pa[2], size = 0.5) +
  ylab("Frequency") + xlab(NULL) +
  th2 + theme(legend.position="top") +  facet_wrap(~ Trait)


Figure_3 <- ggarrange(D, E, Fa, A, B, C,
               ncol = 3, nrow = 2, align = "hv", 
               widths = c(2.5, 2.5, 2.5), 
               heights = c(1, 2),
               font.label = list(size = 14, color = "black", face = "plain", family = NULL),
               label.x = 0.90,
               label.y = 0.99,
               common.legend = TRUE)

tiff("Figure_3.tif", width = 21, height = 10, units = "cm", res = 600)

Figure_3

dev.off()
