###############################################################
###Figure 5
###############################################################

#Note: inputs come from code_base.R

###Libraries
library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)
library(ggExtra)
library(plyr)

###Data and preparations
LMA_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/LMA_predict_testing.csv")
LMA_predict[Spectra == "CWT", Spectra := "Wavelet"]
WC_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/WC_predict_testing.csv")
WC_predict[Spectra == "CWT", Spectra := "Wavelet"]
EWT_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/EWT_predict_testing.csv")
EWT_predict[Spectra == "CWT", Spectra := "Wavelet"]

###Data manage
LMA  <- melt(LMA_predict, id.vars=c("Spectra", "iteration"),
                 measure.vars = .SD,
                 value.name = "Trait")
LMA <- LMA[, list(mean(Trait), (mean(Trait) - sd(Trait)), (mean(Trait) + sd(Trait))), by = c("Spectra", "variable")]
colnames(LMA)[3:5] <- c("mean", "sd_lower", "sd_upper")
LMA <- LMA[order(Spectra, variable)]

WC  <- melt(WC_predict, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
WC <- WC[, list(mean(Trait), (mean(Trait) - sd(Trait)), (mean(Trait) + sd(Trait))), by = c("Spectra", "variable")]
colnames(WC)[3:5] <- c("mean", "sd_lower", "sd_upper")
WC <- WC[order(Spectra, variable)]

EWT  <- melt(EWT_predict, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
EWT <- EWT[, list(mean(Trait), (mean(Trait) - sd(Trait)), (mean(Trait) + sd(Trait))), by = c("Spectra", "variable")]
colnames(EWT)[3:5] <- c("mean", "sd_lower", "sd_upper")
EWT <- EWT[order(Spectra, variable)]

observed <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/traits_testing.csv")
colnames(observed)[3:5] <- c("LMA", "WC", "EWT")
observed$LMA <- 10^observed$LMA
observed$WC <- 10^observed$WC
observed$EWT <- 10^observed$EWT
observed[Life_form == "Tree", Life_form := "Trees"]
observed[Life_form == "Liana", Life_form := "Lianas"]
observed$Life_form <- as.factor(observed$Life_form)
observed$Life_form <- factor(observed$Life_form, levels = c("Lianas", "Trees"))

ref_LMA <- cbind(observed[,c(1,3)], LMA[Spectra == "Reflectance"])
ref_WC <- cbind(observed[,c(1,4)], WC[Spectra == "Reflectance"])
ref_EWT <- cbind(observed[,c(1,5)], EWT[Spectra == "Reflectance"]) 
cwt_LMA <- cbind(observed[,c(1,3)], LMA[Spectra == "Wavelet"])
cwt_WC <- cbind(observed[,c(1,4)], WC[Spectra == "Wavelet"])
cwt_EWT <- cbind(observed[,c(1,5)], EWT[Spectra == "Wavelet"])

ref_LMA[, c("fit", "lwr", "upr") := as.data.table((predict(lm(LMA ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
ref_WC[, c("fit", "lwr", "upr") := as.data.table((predict(lm(WC ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
ref_EWT[, c("fit", "lwr", "upr") := as.data.table((predict(lm(EWT ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
cwt_LMA[, c("fit", "lwr", "upr") := as.data.table((predict(lm(LMA ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
cwt_WC[, c("fit", "lwr", "upr") := as.data.table((predict(lm(WC ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
cwt_EWT[, c("fit", "lwr", "upr") := as.data.table((predict(lm(EWT ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]


#Parameters for figure
pa <- c("#e66101", "#5e3c99")
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
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

LMA_range <- c(0, 315)
WC_range <- c(40, 95)
EWT_range <- c(0, 410)
size_point <- 1.5

A <- ggplot(ref_LMA, aes(x = mean, fill = Life_form)) +
     geom_density(alpha = 0.2) +
     scale_fill_manual(values = pa) +
     scale_color_manual(values = pa) +
     scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
     theme_void() +
     theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))


B <- ggplot(cwt_LMA, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

C <- ggplot(ref_LMA, aes(x = mean, y = LMA, fill = Life_form, colour = Life_form)) +
     geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
     geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
     geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
     geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
     scale_fill_manual(values= pa) +
     scale_color_manual(values= pa) +
     scale_x_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
     scale_y_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
     xlab(expression(paste("Predicted LMA (g m"^-2, ")", sep = "")))  +   
     ylab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +
     geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
     geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
     th


D <- ggplot(cwt_LMA, aes(x = mean, y = LMA, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
  scale_y_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
  xlab(expression(paste("Predicted LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th + ylab("")

E <- ggplot(cwt_LMA, aes(x = LMA, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()


Fa <- ggplot(ref_WC, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

G <- ggplot(cwt_WC, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

H <- ggplot(ref_WC, aes(x = mean, y = WC, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  scale_y_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  xlab(expression(paste("Predicted WC (%)", sep = "")))  +   
  ylab(expression(paste("Observed WC (%)", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

I <- ggplot(cwt_WC, aes(x = mean, y = WC, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  scale_y_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  xlab(expression(paste("Predicted WC (%)", sep = "")))  +   
  ylab(expression(paste("Observed WC (%)", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th + ylab("")

J <- ggplot(cwt_WC, aes(x = WC, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

K <- ggplot(ref_EWT, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

L <- ggplot(cwt_EWT, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

M <- ggplot(ref_EWT, aes(x = mean, y = EWT, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  scale_y_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  xlab(expression(paste("Predicted EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

N <- ggplot(cwt_EWT, aes(x = mean, y = EWT, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  scale_y_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  xlab(expression(paste("Predicted EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th + ylab("")

O <- ggplot(cwt_EWT, aes(x = EWT, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

Figure_5 <- ggarrange(A, B, NULL, 
                      C, D, E,
                      Fa, G, NULL,
                      H, I, J,
                      K, L, NULL,
                      M, N, O,
                      labels = c("", "", "", "a", "b", "", "", "", "", "c", "d", "", "", "", "", "e", "f", ""), 
                      font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                      label.x = 0.22,
                      label.y = 0.99,
                      ncol = 3, nrow = 6,  align = "hv", 
                      widths = c(2.5, 2.5, 1), 
                      heights = c(1, 2.5, 1, 2.5, 1, 2.5),
                      common.legend = TRUE)

tiff("Figure_5.tif", width = 20, height = 25, units = "cm", res = 600)

Figure_5

dev.off()
     
