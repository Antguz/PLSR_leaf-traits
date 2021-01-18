###############################################################
###Figure 6
###############################################################

#Note: inputs come from code_base.R

###Library
library(Rmisc)
library(ggplo2)
library(data.table)
library(ggpubr)
library(PupillometryR)

###Load and prepare the data
LMA <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/LMA_performance.csv")
LMA[Spectra == "CWT", Spectra := "Wavelet"]
WC <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/WC_performance.csv")
WC[Spectra == "CWT", Spectra := "Wavelet"]
EWT <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/EWT_performance.csv")
EWT[Spectra == "CWT", Spectra := "Wavelet"]

LMA$Trait <- "LMA"
WC$Trait <- "WC"
EWT$Trait <- "EWT"

###Merge
data <- rbind(LMA, WC, EWT)
data[Life_form == "Liana", Life_form := "Lianas"]
data[Life_form == "Tree", Life_form := "Trees"]

Rsq <- data[, c(1:4, 6, 10)]
Bias <- data[, c(1:4, 7, 10)]
RMSE <- data[, c(1:4, 8, 10)]
RMSE_P <- data[, c(1:4, 9, 10)]

Rsq$Parameter <- "Rsq"
Bias$Parameter <- "Bias"
RMSE$Parameter <- "RMSE"
RMSE_P$Parameter <- "RMSE_P"

colnames(Rsq)[5] <- "Value"
colnames(Bias)[5] <- "Value"
colnames(RMSE)[5] <- "Value"
colnames(RMSE_P)[5] <- "Value"

###final to keep
data <- rbind(Rsq, Bias, RMSE, RMSE_P)
data$Spectra <- as.factor(data$Spectra)
data$Spectra <- factor(data$Spectra, levels = c("Reflectance", "Wavelet"))

###For figure
data_life_form <- subset(data, Life_form != "All")
data_life_form <- data_life_form[Model == "Testing"]
data_life_form$Life_form <- as.factor(data_life_form$Life_form)
data_life_form$Life_form <- factor(data_life_form$Life_form, levels = c("Lianas", "Trees"))

LMA <- data_life_form[Trait == "LMA"]
WC <- data_life_form[Trait == "WC"]
EWT <- data_life_form[Trait == "EWT"]

###For lines
data_all <- subset(data, Life_form == "All")
data_all <- data_all[Model == "Testing"]

LMA_all <- data_all[Trait == "LMA"]
WC_all <- data_all[Trait == "WC"]
EWT_all <- data_all[Trait == "EWT"]

####------------------Figure

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

######----Rsq
LMA_Rsq <- summarySE(LMA[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

LMA_rsq <- summarySE(LMA_all[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra"))

LMA_rsq$lower_ci <- LMA_rsq$Value - LMA_rsq$ci
LMA_rsq$upper_ci <- LMA_rsq$Value + LMA_rsq$ci
LMA_rsq[, 2:8] <- round(LMA_rsq[, 2:8], 4)

A <- ggplot() +
  geom_errorbar(data= LMA_rsq, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "Rsq"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    
  scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(0.55, 0.92), expand=c(0,0), breaks = c(0.6, 0.7, 0.8, 0.9), labels = c(0.6, 0.7, 0.8, 0.9)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank()) +
  facet_grid(. ~ Trait, labeller = label_parsed)

WC_Rsq <- summarySE(WC[Parameter == "Rsq"], measurevar = "Value",
                    groupvars=c("Spectra", "Life_form"))

WC_rsq <- summarySE(WC_all[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra"))

WC_rsq$lower_ci <- WC_rsq$Value - WC_rsq$ci
WC_rsq$upper_ci <- WC_rsq$Value + WC_rsq$ci
WC_rsq[, 2:8] <- round(WC_rsq[, 2:8], 4)

B <- ggplot() +
  geom_errorbar(data= WC_rsq, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "Rsq"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    
  scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(0.55, 0.92), expand=c(0,0), breaks = c(0.6, 0.7, 0.8, 0.9), labels = c(0.6, 0.7, 0.8, 0.9)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank()) +
  facet_grid(. ~ Trait, labeller = label_parsed)

EWT_Rsq <- summarySE(EWT[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

EWT_rsq <- summarySE(EWT_all[Parameter == "Rsq"], measurevar = "Value",
                    groupvars=c("Spectra"))

EWT_rsq$lower_ci <- EWT_rsq$Value - EWT_rsq$ci
EWT_rsq$upper_ci <- EWT_rsq$Value + EWT_rsq$ci
EWT_rsq[, 2:8] <- round(EWT_rsq[, 2:8], 4)

C <- ggplot() +
  geom_errorbar(data= EWT_rsq, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "Rsq"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    
  scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(0, 0.9), expand=c(0,0), breaks = c(0, 0.25, 0.5, 0.75), labels = c("", 0.25, 0.5, 0.75)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank()) +
  facet_grid(. ~ Trait, labeller = label_parsed)

######----Bias
LMA_Bias <- summarySE(LMA[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

LMA_bias <- summarySE(LMA_all[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra"))

LMA_bias$lower_ci <- LMA_bias$Value - LMA_bias$ci
LMA_bias$upper_ci <- LMA_bias$Value + LMA_bias$ci
LMA_bias[, 2:8] <- round(LMA_bias[, 2:8], 4)

D <- ggplot() +
  geom_errorbar(data= LMA_bias, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "Bias"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(-0.06, 0.06), expand=c(0,0), breaks = c(-0.05, 0, 0.05), labels = c(-0.05, 0, 0.05)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

WC_Bias <- summarySE(WC[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

WC_bias <- summarySE(WC_all[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra"))

WC_bias$lower_ci <- WC_bias$Value - WC_bias$ci
WC_bias$upper_ci <- WC_bias$Value + WC_bias$ci
WC_bias[, 2:8] <- round(WC_bias[, 2:8], 4)

E <- ggplot() +
  geom_errorbar(data= WC_bias, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "Bias"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(-0.02, 0.02), expand=c(0,0), breaks = c(-0.01, 0, 0.01), labels = c(-0.01, 0, 0.01)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

EWT_Bias <- summarySE(EWT[Parameter == "Bias"], measurevar = "Value",
                      groupvars=c("Spectra", "Life_form"))

EWT_bias <- summarySE(EWT_all[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra"))

EWT_bias$lower_ci <- EWT_bias$Value - EWT_bias$ci
EWT_bias$upper_ci <- EWT_bias$Value + EWT_bias$ci
EWT_bias[, 2:8] <- round(EWT_bias[, 2:8], 4)

Fa <- ggplot() +
  geom_errorbar(data= EWT_bias, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "Bias"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(-0.055, 0.055), expand=c(0,0), breaks = c(-0.03, 0, 0.03), labels = c(-0.03, 0, 0.03)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())


######----RMSE
LMA_RMSE <- summarySE(LMA[Parameter == "RMSE"], measurevar = "Value",
                      groupvars=c("Spectra", "Life_form"))

LMA_rmse <- summarySE(LMA_all[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra"))


LMA_rmse$lower_ci <- LMA_rmse$Value - LMA_rmse$ci
LMA_rmse$upper_ci <- LMA_rmse$Value + LMA_rmse$ci
LMA_rmse[, 2:8] <- round(LMA_rmse[, 2:8], 4)

G <- ggplot() +
  geom_errorbar(data= LMA_rmse, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "RMSE"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(4, 30), expand=c(0,0), breaks = c(10, 20, 30), labels = c(10, 20, 30)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

WC_RMSE <- summarySE(WC[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

WC_rmse <- summarySE(WC_all[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra"))

WC_rmse$lower_ci <- WC_rmse$Value - WC_rmse$ci
WC_rmse$upper_ci <- WC_rmse$Value + WC_rmse$ci
WC_rmse[, 2:8] <- round(WC_rmse[, 2:8], 4)

H <- ggplot() +
  geom_errorbar(data= WC_rmse, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "RMSE"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(3, 5.55), expand=c(0,0), breaks = c(3, 3.5, 4, 4.5, 5, 5.5), labels = c(3, "", 4, "", 5, "")) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

EWT_RMSE <- summarySE(EWT[Parameter == "RMSE"], measurevar = "Value",
                      groupvars=c("Spectra", "Life_form"))

EWT_rmse <- summarySE(EWT_all[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra"))

EWT_rmse$lower_ci <- EWT_rmse$Value - EWT_rmse$ci
EWT_rmse$upper_ci <- EWT_rmse$Value + EWT_rmse$ci
EWT_rmse[, 2:8] <- round(EWT_rmse[, 2:8], 4)

I <- ggplot() +
  geom_errorbar(data= EWT_rmse, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "RMSE"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(17, 55), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

######----RMSE_P
LMA_RMSE_P <- summarySE(LMA[Parameter == "RMSE_P"], measurevar = "Value",
                        groupvars=c("Spectra", "Life_form"))

LMA_rmsep <- summarySE(LMA_all[Parameter == "RMSE_P"], measurevar = "Value",
                     groupvars=c("Spectra"))

LMA_rmsep$lower_ci <- LMA_rmsep$Value - LMA_rmsep$ci
LMA_rmsep$upper_ci <- LMA_rmsep$Value + LMA_rmsep$ci
LMA_rmsep[, 2:8] <- round(LMA_rmsep[, 2:8], 4)

J <- ggplot() +
  geom_errorbar(data= LMA_rmsep, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "RMSE_P"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(7, 15), expand=c(0,0), breaks = c(7, 9, 11, 13, 15), labels = c(7, "", 11, "", 15)) +
  th + xlab("") + ylab("") 
  theme( axis.text.x = element_blank())

WC_RMSE_P <- summarySE(WC[Parameter == "RMSE_P"], measurevar = "Value",
                       groupvars=c("Spectra", "Life_form"))

WC_rmsep <- summarySE(WC_all[Parameter == "RMSE_P"], measurevar = "Value",
                     groupvars=c("Spectra"))

WC_rmsep$lower_ci <- WC_rmsep$Value - WC_rmsep$ci
WC_rmsep$upper_ci <- WC_rmsep$Value + WC_rmsep$ci
WC_rmsep[, 2:8] <- round(WC_rmsep[, 2:8], 4)

K <- ggplot() +
  geom_errorbar(data= WC_rmsep, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "RMSE_P"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(8, 17.49), expand=c(0,0)) +
  th + xlab("Spectra") + ylab("") 
  theme( axis.text.x = element_blank())

EWT_RMSE_P <- summarySE(EWT[Parameter == "RMSE_P"], measurevar = "Value",
                        groupvars=c("Spectra", "Life_form"))

EWT_rmsep <- summarySE(EWT_all[Parameter == "RMSE_P"], measurevar = "Value",
                     groupvars=c("Spectra"))

EWT_rmsep$lower_ci <- EWT_rmsep$Value - EWT_rmsep$ci
EWT_rmsep$upper_ci <- EWT_rmsep$Value + EWT_rmsep$ci
EWT_rmsep[, 2:8] <- round(EWT_rmsep[, 2:8], 4)

L <- ggplot() +
  geom_errorbar(data= EWT_rmsep, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "RMSE_P"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_fill_manual(values = c("Lianas" = pa[1], "Trees" = pa[2])) +
  scale_y_continuous(limits = c(9, 29), expand=c(0,0)) +
  th + xlab("") + ylab("") 
  theme( axis.text.x = element_blank())


Figure_6 <- ggarrange(A, B, C,
                      D, E, Fa,
                      G, H, I,
                      J, K, L,
                      labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"), 
                      font.label = list(size = 15, color = "black", face = "plain", family = NULL),
                      label.x = 0.26,
                      label.y = 0.99,
                      ncol = 3, nrow = 4,  align = "hv",
                      widths = c(2, 2, 2),
                      heights = c(2, 2, 2, 2),
                      common.legend = TRUE)


tiff("Figure_6a.tif", width = 21, height = 21, units = "cm", res = 600)

Figure_6

dev.off()

