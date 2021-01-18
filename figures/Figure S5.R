###############################################################
###Figure S5
###############################################################

#Note: inputs come from code_base.R

###Libraries
library(data.table)
library(ggExtra)
library(plyr)

###Data and preparations
#Training observations
LMA_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/06-Predict_train/LMA_predict_training.csv")
LMA_predict[Spectra == "CWT", Spectra := "Wavelet"]
WC_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/06-Predict_train/WC_predict_training.csv")
WC_predict[Spectra == "CWT", Spectra := "Wavelet"]
EWT_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/06-Predict_train/EWT_predict_training.csv")
EWT_predict[Spectra == "CWT", Spectra := "Wavelet"]

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

observed <- fread("/home/antguz/Documents/PLSR-models/Data/03-spectra/traits_training.csv")
colnames(observed)[3:5] <- c("LMA", "WC", "EWT")
observed$LMA <- 10^observed$LMA
observed$WC <- 10^observed$WC
observed$EWT <- 10^observed$EWT
observed[Life_form == "Tree", Life_form := "Trees"]
observed[Life_form == "Liana", Life_form := "Lianas"]
observed$Life_form <- as.factor(observed$Life_form)
observed$Life_form <- factor(observed$Life_form, levels = c("Lianas", "Trees"))

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

#Testing observations
LMA_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/LMA_predict_testing.csv")
LMA_predict[Spectra == "CWT", Spectra := "Wavelet"]
WC_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/WC_predict_testing.csv")
WC_predict[Spectra == "CWT", Spectra := "Wavelet"]
EWT_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/EWT_predict_testing.csv")
EWT_predict[Spectra == "CWT", Spectra := "Wavelet"]

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


#Data manage
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

tiff("Figure_S5.tif", width = 19.5, height = 21, units = "cm", res = 600)

Figure_5

dev.off()
