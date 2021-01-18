###############################################################
###Figure S5
###############################################################

#Note: inputs come from code_base.R


###Libraries
library(data.table)
library(ggExtra)
library(plyr)

###Data and preparations
ref_LMA_train <- cbind(observed[,c(1,3)], LMA[Spectra == "Reflectance"])
ref_LMA_train$Process <- "Training"
ref_WC_train <- cbind(observed[,c(1,4)], WC[Spectra == "Reflectance"])
ref_WC_train$Process <- "Training"
ref_EWT_train <- cbind(observed[,c(1,5)], EWT[Spectra == "Reflectance"]) 
ref_EWT_train$Process <- "Training"
cwt_LMA_train <- cbind(observed[,c(1,3)], LMA[Spectra == "Wavelet"])
cwt_LMA_train$Process <- "Training"
cwt_WC_train <- cbind(observed[,c(1,4)], WC[Spectra == "Wavelet"])
cwt_WC_train$Process <- "Training"
cwt_EWT_train <- cbind(observed[,c(1,5)], EWT[Spectra == "Wavelet"])
cwt_EWT_train$Process <- "Training"

ref_LMA_test <- cbind(observed[,c(1,3)], LMA[Spectra == "Reflectance"])
ref_LMA_test$Process <- "Testing"
ref_WC_test <- cbind(observed[,c(1,4)], WC[Spectra == "Reflectance"])
ref_WC_test$Process <- "Testing"
ref_EWT_test <- cbind(observed[,c(1,5)], EWT[Spectra == "Reflectance"]) 
ref_EWT_test$Process <- "Testing"
cwt_LMA_test <- cbind(observed[,c(1,3)], LMA[Spectra == "Wavelet"])
cwt_LMA_test$Process <- "Testing"
cwt_WC_test <- cbind(observed[,c(1,4)], WC[Spectra == "Wavelet"])
cwt_WC_test$Process <- "Testing"
cwt_EWT_test <- cbind(observed[,c(1,5)], EWT[Spectra == "Wavelet"])
cwt_EWT_test$Process <- "Testing"

LMA_train <- rbind(ref_LMA_train, cwt_LMA_train)
WC_train <- rbind(ref_WC_train, cwt_WC_train)
EWT_train <- rbind(ref_EWT_train, cwt_EWT_train)

LMA_test <- rbind(ref_LMA_test, cwt_LMA_test)
WC_test <- rbind(ref_WC_test, cwt_WC_test)
EWT_test <- rbind(ref_EWT_test, cwt_EWT_test)

LMA_train[, c("fit", "lwr", "upr") := as.data.table((predict(lm(LMA ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Spectra"]
WC_train[, c("fit", "lwr", "upr") := as.data.table((predict(lm(WC ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Spectra"]
EWT_train[, c("fit", "lwr", "upr") := as.data.table((predict(lm(EWT ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Spectra"]
LMA_test[, c("fit", "lwr", "upr") := as.data.table((predict(lm(LMA ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Spectra"]
WC_test[, c("fit", "lwr", "upr") := as.data.table((predict(lm(WC ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Spectra"]
EWT_test[, c("fit", "lwr", "upr") := as.data.table((predict(lm(EWT ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Spectra"]

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

A <- ggplot(LMA_train, aes(x = mean, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))


B <- ggplot(LMA_test, aes(x = mean, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

C <- ggplot(LMA_train, aes(x = mean, y = LMA, fill = Spectra, colour = Spectra)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Spectra), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
  scale_y_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
  xlab(expression(paste("Predicted LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Spectra), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Spectra), linetype = "dashed", size= 0.3) +
  th

D <- ggplot(LMA_train, aes(x = LMA, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()


E <- ggplot(LMA_test, aes(x = mean, y = LMA, fill = Spectra, colour = Spectra)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Spectra), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
  scale_y_continuous(limits = LMA_range, expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), labels = c("", 50, "", 150, "", 250, "")) +
  xlab(expression(paste("Predicted LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Spectra), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Spectra), linetype = "dashed", size= 0.3) +
  th + ylab("")

Fa <- ggplot(LMA_test, aes(x = LMA, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()


G <- ggplot(WC_train, aes(x = mean, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

H <- ggplot(WC_test, aes(x = mean, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

I <- ggplot(WC_train, aes(x = mean, y = WC, fill = Spectra, colour = Spectra)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Spectra), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  scale_y_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  xlab(expression(paste("Predicted WC (%)", sep = "")))  +   
  ylab(expression(paste("Observed WC (%)", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Spectra), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Spectra), linetype = "dashed", size= 0.3) +
  th

J <- ggplot(WC_train, aes(x = WC, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

K <- ggplot(WC_test, aes(x = mean, y = WC, fill = Spectra, colour = Spectra)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Spectra), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  scale_y_continuous(limits = WC_range, expand = c(0, 0), breaks = c(40, 50, 60, 70, 80, 90), labels = c("", 50, "", 70, "", 90)) +
  xlab(expression(paste("Predicted WC (%)", sep = "")))  +   
  ylab(expression(paste("Observed WC (%)", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Spectra), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Spectra), linetype = "dashed", size= 0.3) +
  th +  ylab("")

L <- ggplot(WC_test, aes(x = WC, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

M <- ggplot(EWT_train, aes(x = mean, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

N <- ggplot(EWT_test, aes(x = mean, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

O <- ggplot(EWT_train, aes(x = mean, y = EWT, fill = Spectra, colour = Spectra)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Spectra), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  scale_y_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  xlab(expression(paste("Predicted EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Spectra), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Spectra), linetype = "dashed", size= 0.3) +
  th

P <- ggplot(EWT_train, aes(x = EWT, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()


Q <- ggplot(EWT_test, aes(x = mean, y = EWT, fill = Spectra, colour = Spectra)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Spectra), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  scale_y_continuous(limits = EWT_range, expand = c(0, 0), breaks = c(0, 75, 150, 225, 300, 375), labels = c(0, "", 150, "", 300, "")) +
  xlab(expression(paste("Predicted EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Spectra), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Spectra), linetype = "dashed", size= 0.3) +
  th + ylab("")

R <- ggplot(EWT_train, aes(x = EWT, fill = Spectra)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

Figure_5 <- ggarrange(C, E, 
                      I, K, 
                      O, Q, 
                      labels = c("a", "b", "c", "d", "e", "f"), 
                      font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                      label.x = 0.19,
                      label.y = 0.97,
                      ncol = 2, nrow = 3,  align = "hv", 
                      widths = c(2.5, 2.5), 
                      heights = c(2.5, 2.5, 2.5),
                      common.legend = TRUE)

tiff("Figure_5_NEW.tif", width = 19.5, height = 21, units = "cm", res = 600)

Figure_5

dev.off()
