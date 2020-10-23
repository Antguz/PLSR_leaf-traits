###############################################################
###Figure S6
###############################################################

#Note: inputs come from code_base.R

###Library
library(Rmisc)
library(ggplo2)
library(data.table)
library(ggpubr)
library(PupillometryR)

###Load and prepare the data
ref_LMA <- fread("ref_LMA_stats.csv")
ref_WC <- fread("ref_WC_stats.csv")
ref_EWT <- fread("ref_EWT_stats.csv")
cwt_LMA <- fread("cwt_LMA_stats.csv")
cwt_WC <- fread("cwt_WC_stats.csv")
cwt_EWT <- fread("cwt_EWT_stats.csv")

ref_LMA$Trait <- "LMA"
ref_WC$Trait <- "WC"
ref_EWT$Trait <- "EWT"
cwt_LMA$Trait <- "LMA"
cwt_WC$Trait <- "WC"
cwt_EWT$Trait <- "EWT"

ref_LMA$Spectra <- "Reflectance"
ref_WC$Spectra <- "Reflectance"
ref_EWT$Spectra <- "Reflectance"
cwt_LMA$Spectra <- "Wavelet"
cwt_WC$Spectra <- "Wavelet"
cwt_EWT$Spectra <- "Wavelet"


###Merge
data <- rbind(ref_LMA, ref_WC, ref_EWT, cwt_LMA, cwt_WC, cwt_EWT)

Rsq <- data[, c(1:4, 5, 9:10)]
Bias <- data[, c(1:4, 6, 9:10)]
RMSE <- data[, c(1:4, 7, 9:10)]
RMSE_P <- data[, c(1:4, 8, 9:10)]

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
data_life_form <- data_life_form[Process == "Training"]
data_life_form$Life_form <- as.factor(data_life_form$Life_form)
data_life_form$Life_form <- factor(data_life_form$Life_form, levels = c("Lianas", "Trees"))

LMA <- data_life_form[Trait == "LMA"]
WC <- data_life_form[Trait == "WC"]
EWT <- data_life_form[Trait == "EWT"]

###For lines
data_all <- subset(data, Life_form == "All")
data_all <- data_all[Process == "Training"]

LMA_all <- data_all[Trait == "LMA"]
WC_all <- data_all[Trait == "WC"]
EWT_all <- data_all[Trait == "EWT"]

####------------------Figure

pa <- c("#33B09F", "#B66A34")
tamano <- 14
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

######----Rsq
LMA_Rsq <- summarySE(LMA[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

LMA_rsq <- summarySE(LMA_all[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra"))

A <- ggplot() +
  geom_errorbar(data= LMA_rsq, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "Rsq"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(0.7, 0.95), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank()) 
  facet_grid(. ~ Trait, labeller = label_parsed)

WC_Rsq <- summarySE(WC[Parameter == "Rsq"], measurevar = "Value",
                    groupvars=c("Spectra", "Life_form"))

WC_rsq <- summarySE(WC_all[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra"))

B <- ggplot() +
  geom_errorbar(data= WC_rsq, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "Rsq"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(0.7, 0.94), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank()) 
  facet_grid(. ~ Trait, labeller = label_parsed)

EWT_Rsq <- summarySE(EWT[Parameter == "Rsq"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

EWT_rsq <- summarySE(EWT_all[Parameter == "Rsq"], measurevar = "Value",
                    groupvars=c("Spectra"))

C <- ggplot() +
  geom_errorbar(data= EWT_rsq, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "Rsq"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "Rsq"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_Rsq, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(0.2, 0.9), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank()) 
  facet_grid(. ~ Trait, labeller = label_parsed)

######----Bias
LMA_Bias <- summarySE(LMA[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

LMA_bias <- summarySE(LMA_all[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra"))

D <- ggplot() +
  geom_errorbar(data= LMA_bias, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "Bias"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(-0.05, 0.05), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

WC_Bias <- summarySE(WC[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

WC_bias <- summarySE(WC_all[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra"))

E <- ggplot() +
  geom_errorbar(data= WC_bias, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "Bias"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(-0.02, 0.02), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

EWT_Bias <- summarySE(EWT[Parameter == "Bias"], measurevar = "Value",
                      groupvars=c("Spectra", "Life_form"))

EWT_bias <- summarySE(EWT_all[Parameter == "Bias"], measurevar = "Value",
                     groupvars=c("Spectra"))

Fa <- ggplot() +
  geom_errorbar(data= EWT_bias, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "Bias"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "Bias"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_Bias, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(-0.06, 0.06), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())


######----RMSE
LMA_RMSE <- summarySE(LMA[Parameter == "RMSE"], measurevar = "Value",
                      groupvars=c("Spectra", "Life_form"))

LMA_rmse <- summarySE(LMA_all[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra"))

G <- ggplot() +
  geom_errorbar(data= LMA_rmse, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "RMSE"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(0, 33), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

WC_RMSE <- summarySE(WC[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra", "Life_form"))

WC_rmse <- summarySE(WC_all[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra"))

H <- ggplot() +
  geom_errorbar(data= WC_rmse, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "RMSE"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(2.5, 5), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

EWT_RMSE <- summarySE(EWT[Parameter == "RMSE"], measurevar = "Value",
                      groupvars=c("Spectra", "Life_form"))

EWT_rmse <- summarySE(EWT_all[Parameter == "RMSE"], measurevar = "Value",
                     groupvars=c("Spectra"))

I <- ggplot() +
  geom_errorbar(data= EWT_rmse, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "RMSE"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "RMSE"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_RMSE, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(10, 40), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

######----RMSE_P
LMA_RMSE_P <- summarySE(LMA[Parameter == "RMSE_P"], measurevar = "Value",
                        groupvars=c("Spectra", "Life_form"))

LMA_rmsep <- summarySE(LMA_all[Parameter == "RMSE_P"], measurevar = "Value",
                     groupvars=c("Spectra"))

J <- ggplot() +
  geom_errorbar(data= LMA_rmsep, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = LMA[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = LMA[Parameter == "RMSE_P"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = LMA[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = LMA_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = LMA_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = LMA_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(3, 17.5), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

WC_RMSE_P <- summarySE(WC[Parameter == "RMSE_P"], measurevar = "Value",
                       groupvars=c("Spectra", "Life_form"))

WC_rmsep <- summarySE(WC_all[Parameter == "RMSE_P"], measurevar = "Value",
                     groupvars=c("Spectra"))

K <- ggplot() +
  geom_errorbar(data= WC_rmsep, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = WC[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = WC[Parameter == "RMSE_P"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = WC[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = WC_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = WC_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = WC_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(7, 12.5), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())

EWT_RMSE_P <- summarySE(EWT[Parameter == "RMSE_P"], measurevar = "Value",
                        groupvars=c("Spectra", "Life_form"))

EWT_rmsep <- summarySE(EWT_all[Parameter == "RMSE_P"], measurevar = "Value",
                     groupvars=c("Spectra"))

L <- ggplot() +
  geom_errorbar(data= EWT_rmsep, aes(x = Spectra, ymax= Value, ymin= Value), colour = "grey20", linetype = 2) +  
  geom_flat_violin(data = EWT[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .35, colour = "white") +
  geom_point(data = EWT[Parameter == "RMSE_P"], aes(x = as.numeric(Spectra)-.15, y = Value, colour = Life_form), position = position_jitter(width = .05), size = .25, shape = 20) +
  geom_boxplot(data = EWT[Parameter == "RMSE_P"], aes(x = Spectra, y = Value, fill = Life_form), outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  geom_line(data = EWT_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), linetype = 3) +
  geom_point(data = EWT_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form), shape = 18) +
  geom_errorbar(data = EWT_RMSE_P, aes(x = as.numeric(Spectra)+.1, y = Value, group = Life_form, colour = Life_form, ymin = Value-se, ymax = Value+se), width = .05) +    scale_colour_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_fill_manual(values = c("Lianas" = '#33B09F', "Trees" = '#B66A34')) +
  scale_y_continuous(limits = c(7.5, 17.5), expand=c(0,0)) +
  th + xlab("") + ylab("") + theme( axis.text.x = element_blank())


tiff("Figure_S6a.tif", width = 21, height = 21, units = "cm", res = 600)
ggarrange(A, B, C,
          D, E, Fa,
          G, H, I,
          J, K, L,
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"), 
          font.label = list(size = 15, color = "black", face = "plain", family = NULL),
          label.x = 0.24,
          label.y = 0.99,
          ncol = 3, nrow = 4,  align = "hv",
          widths = c(2, 2, 2),
          heights = c(2, 2, 2, 2),
          common.legend = TRUE)
dev.off()
