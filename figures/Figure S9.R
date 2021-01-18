###############################################################
###Figure S9
###############################################################

#Note: inputs come from code_base.R

#Library
library(data.table)
library(ggpubr)
library(ggplot2)

#Load and prepare data
LMA <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/09-Residuals_test/LMA_residuals_testing.csv")
LMA[Spectra == "CWT", Spectra := "Wavelet"]
WC <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/09-Residuals_test/WC_residuals_testing.csv")
WC[Spectra == "CWT", Spectra := "Wavelet"]
EWT <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/09-Residuals_test/EWT_residuals_testing.csv")
EWT[Spectra == "CWT", Spectra := "Wavelet"]

#Data manage
LMA  <- melt(LMA, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
LMA <- LMA[, list(mean(Trait), (mean(Trait) - sd(Trait)), (mean(Trait) + sd(Trait))), by = c("Spectra", "variable")]
colnames(LMA)[3:5] <- c("mean", "sd_lower", "sd_upper")
LMA <- LMA[order(Spectra, variable)]

WC  <- melt(WC, id.vars=c("Spectra", "iteration"),
            measure.vars = .SD,
            value.name = "Trait")
WC <- WC[, list(mean(Trait), (mean(Trait) - sd(Trait)), (mean(Trait) + sd(Trait))), by = c("Spectra", "variable")]
colnames(WC)[3:5] <- c("mean", "sd_lower", "sd_upper")
WC <- WC[order(Spectra, variable)]

EWT  <- melt(EWT, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
EWT <- EWT[, list(mean(Trait), (mean(Trait) - sd(Trait)), (mean(Trait) + sd(Trait))), by = c("Spectra", "variable")]
colnames(EWT)[3:5] <- c("mean", "sd_lower", "sd_upper")
EWT <- EWT[order(Spectra, variable)]

#Traits
observed <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/traits_testing.csv")
colnames(observed)[3:5] <- c("LMA", "WC", "EWT")
observed$LMA <- 10^observed$LMA
observed$WC <- 10^observed$WC
observed$EWT <- 10^observed$EWT
observed[Life_form == "Liana", Life_form := "Lianas"]
observed[Life_form == "Tree", Life_form := "Trees"]
observed$Life_form <- as.factor(observed$Life_form)
observed$Life_form <- factor(observed$Life_form, levels = c("Lianas", "Trees"))


ref_LMA <- cbind(observed[,c(1,3)], LMA[Spectra == "Reflectance"])
ref_WC <- cbind(observed[,c(1,4)], WC[Spectra == "Reflectance"])
ref_EWT <- cbind(observed[,c(1,5)], EWT[Spectra == "Reflectance"]) 
cwt_LMA <- cbind(observed[,c(1,3)], LMA[Spectra == "Wavelet"])
cwt_WC <- cbind(observed[,c(1,4)], WC[Spectra == "Wavelet"])
cwt_EWT <- cbind(observed[,c(1,5)], EWT[Spectra == "Wavelet"])

ref_LMA$Trait <- "LMA"
ref_WC$Trait <- "WC"
ref_EWT$Trait <- "EWT"
cwt_LMA$Trait <- "LMA"
cwt_WC$Trait <- "WC"
cwt_EWT$Trait <- "EWT"


###Plots 
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
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

th2 <- theme_bw(base_size = 11) + theme(plot.background = element_blank(),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        plot.margin = margin(4, 10, 1, 1, "pt"),
                                        axis.text.x = element_text(color = "black", size = tamano2),
                                        axis.text.y = element_text(color = "black", size = tamano2),
                                        strip.text.x = element_text(size = 12, color = "black"),
                                        strip.text.y = element_text(size = 12, color = "black"),
                                        strip.background = element_rect(color= "black", linetype="solid"))

LMA_range <- c(0, 220)
WC_range <- c(30, 95)
EWT_range <- c(0, 320)
size_point <- 1.5

###Plots

hist_a <- ggplot(ref_LMA, aes(x= LMA, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

hist_b <- ggplot(ref_WC, aes(x= WC, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

hist_c <- ggplot(ref_EWT, aes(x= EWT, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

ref_a <- ggplot(ref_LMA, aes(x = LMA, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200), labels = c(0, "", 100, "", 200)) +
  scale_y_continuous(limits = c(-80, 80), expand = c(0, 0), breaks = c(-80, -40, 0, 40, 80), labels = c("", -40, 0, 40, "")) +
  xlab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("LMA Residuals", sep = "")))  +
  th

ref_a_l <- ggplot(ref_LMA, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-80, 80), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

ref_b <- ggplot(ref_WC, aes(x = WC, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0), breaks = c(30, 40, 50, 60, 70, 80, 90), labels = c("", 40, "", 60, "", 80, "")) +
  scale_y_continuous(limits = c(-15, 15), expand = c(0, 0), breaks = c(-15, -10, -5, 0, 5, 10, 15), labels = c("", -10, "", 0, "", 10, "")) +
  xlab(expression(paste("Observed WC (%)", sep = "")))  +   
  ylab(expression(paste("WC Residuals", sep = "")))  +
  th

ref_b_l <- ggplot(ref_WC, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

ref_c <- ggplot(ref_EWT, aes(x = EWT, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-100, 100), expand = c(0, 0)) +
  xlab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("EWT Residuals", sep = "")))  +
  th

ref_c_l <- ggplot(ref_EWT, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-100, 100), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

cwt_a <- ggplot(cwt_LMA, aes(x = LMA, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200), labels = c(0, "", 100, "", 200)) +
  scale_y_continuous(limits = c(-80, 80), expand = c(0, 0), breaks = c(-80, -40, 0, 40, 80), labels = c("", -40, 0, 40, "")) +
  xlab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("LMA Residuals", sep = "")))  +
  th

cwt_a_l <- ggplot(cwt_LMA, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-80, 80), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

cwt_b <- ggplot(cwt_WC, aes(x = WC, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0), breaks = c(30, 40, 50, 60, 70, 80, 90), labels = c("", 40, "", 60, "", 80, "")) +
  scale_y_continuous(limits = c(-15, 15), expand = c(0, 0), breaks = c(-15, -10, -5, 0, 5, 10, 15), labels = c("", -10, "", 0, "", 10, "")) +
  xlab(expression(paste("Observed WC (%)", sep = "")))  +   
  ylab(expression(paste("WC Residuals", sep = "")))  +
  th

cwt_b_l <- ggplot(cwt_WC, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

cwt_c <- ggplot(cwt_EWT, aes(x = EWT, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-100, 100), expand = c(0, 0)) +
  xlab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("EWT Residuals", sep = "")))  +
  th

cwt_c_l <- ggplot(cwt_EWT, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-100, 100), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

tiff("Figure_S9.tif", width = 21, height = 11.67, units = "cm", res = 600)

ggarrange(hist_a, NULL, hist_b, NULL, hist_c, NULL,
          ref_a, ref_a_l, ref_b, ref_b_l, ref_c, ref_c_l,
          cwt_a, cwt_a_l, cwt_b, cwt_b_l, cwt_c, cwt_c_l,
          ncol = 6, nrow = 3,  align = "hv",
          labels = c("", "", "", "", "", "", "a", "", "b", "", "c", "", "d", "", "e", "", "f", ""), 
          font.label = list(size = 14, color = "black", face = "plain", family = NULL),
          label.x = 0.30,
          label.y = 0.99,
          widths = c(2, 1, 2, 1, 2, 1), 
          heights = c(1, 2, 2),
          common.legend = TRUE)

dev.off()
