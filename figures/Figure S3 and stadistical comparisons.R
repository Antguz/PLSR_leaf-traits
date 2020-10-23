###############################################################
### Stadistical comparisons and Figure S2
###############################################################

#Note: Some of these inputs come from code_base.R

#Library 
library(car)
library(lme4)
library(nlme)
library(sjPlot)
library(sjmisc)
library(stargazer)
library(data.table)
library(ggplot2)

#Datasets

data <- fread("01-traits.csv")
data$Life_form <- as.factor(data$Life_form)

###Comparisions between life forms

LMA_lmer <- lmer(log10(LMA) ~ Life_form + (1|Acronym), data = data)
qqnorm(resid(LMA_lmer))
qqline(resid(LMA_lmer))
summary(LMA_lmer)
tab_model(LMA_lmer, p.val = "kr", show.df = TRUE)

WC_lmer <- lmer(log10(WC) ~ Life_form + (1|Acronym), data = data)
qqnorm(resid(WC_lmer))
qqline(resid(WC_lmer))
summary(WC_lmer)
tab_model(WC_lmer, p.val = "kr", show.df = TRUE)

EWT_lmer <- lmer(log10(EWT) ~ Life_form + (1|Acronym), data = data)
qqnorm(resid(EWT_lmer))
qqline(resid(EWT_lmer))
summary(EWT_lmer)
tab_model(EWT_lmer, p.val = "kr", show.df = TRUE)

LMA <- as.data.frame(ranef(LMA_lmer))
LMA$trait <- "LMA"
WC <- as.data.frame(ranef(WC_lmer))
WC$trait <- "WC"
EWT <- as.data.frame(ranef(EWT_lmer))
EWT$trait <- "EWT"

data_model <- rbind(LMA, WC, EWT)
colnames(data_model)[3] <- "Species"

data_model <- merge(data_model, data[,c(1,4)], by.x = "Species", by.y = "Acronym", all.x = TRUE, all.y = FALSE)

order <- c("S. obovata", "C. alata", "B. simarouba", "C. americana", "J. curcas", "S. glandulosum", "B. ungulata", "H. courbaril", "G. sepium", "Q. oleoides", "S. mexicanum", "O. veraguensis", "B. crassifolia", "G. ulmifolia", "C. odorata", "T. americana", "P. aculeata", "C. vitifolium", "S. glauca", "L. speciosa", "R. trinervis", "Forsteronia sp.", "F. spicata", "A. chica", "C. aequinoctialis", "C. diversifolia", "Paulinia sp.", "C. racemosa", "T. volubilis", "H. panamensis", "Heteropterys sp.", "H. reclinata", "G. polygama", "S. atrolineata", "S. schiedeana")

data_model$trait <- as.factor(data_model$trait)
data_model$trait <- factor(data_model$trait, levels = c("LMA", "WC", "EWT"))
data_model$Species <- as.factor(data_model$Species)
data_model$Species <- factor(data_model$Species, levels = order)

pa <- c("#33B09F", "#B66A34")

plot <- ggplot(data_model, aes(x = Species, y= condval, fill = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = 'grey', size = 0.3) +
  geom_errorbar(aes(ymin= condval-condsd, ymax= condval+condsd), colour = '#08306B', width= 0.1) +
  geom_point(size = 2, shape = 21, colour = "white") +
  scale_fill_manual("Life form" , values = pa) +
  scale_y_continuous(limits = c(-0.45, 0.45), expand=c(0,0)) +
  coord_flip() +
  theme_bw(base_size = 14) +
  ylab("Conditional variance-covariance") +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face = "italic")) +
  facet_grid(. ~ trait, labeller = label_parsed)

  
tiff("Figure_S3.tif", width = 19, height = 15, units = "cm", res = 600)

plot

dev.off()


####Comparisions
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

data <- rbind(ref_LMA, ref_WC, ref_EWT, cwt_LMA, cwt_WC, cwt_EWT)
data$Spectra <- as.factor(data$Spectra)
data$Samples <- factor(data$Spectra, levels = c("Reflectance", "Wavelet"))

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
data <- data[Life_form == "All"]

LMA <- data[Trait == "LMA"]
WC <- data[Trait == "WC"]
EWT <- data[Trait == "EWT"]

LMA_training <- LMA[Process == "Training"]
LMA_testing <- LMA[Process == "Testing"]
WC_training <- WC[Process == "Training"]
WC_testing <- WC[Process == "Testing"]
EWT_training <- EWT[Process == "Training"]
EWT_testing <- EWT[Process == "Testing"]


t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_training[Parameter == "Rsq"])
t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_testing[Parameter == "Rsq"])

t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_training[Parameter == "Bias"])
t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_testing[Parameter == "Bias"])

t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_training[Parameter == "RMSE"])
t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_testing[Parameter == "RMSE"])

t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_training[Parameter == "RMSE_P"])
t.test(Value ~ Spectra, alternative = "two.sided", var.equal = TRUE, data = LMA_testing[Parameter == "RMSE_P"])

###Ancova comparisions
#Data with IDs
data <- fread("traits_training.csv")
colnames(data)[3:5] <- c("LMA", "WC", "EWT")
data$LMA <- 10^data$LMA
data$WC <- 10^data$WC
data$EWT <- 10^data$EWT
largo <- nrow(data)

#Predicted values
ref_LMA <- fread("ref_LMA_predict_train.csv")
ref_WC <- fread("ref_WC_predict_train.csv")
ref_EWT <- fread("ref_EWT_predict_train.csv")
cwt_LMA <- fread("cwt_LMA_predict_train.csv")
cwt_WC <- fread("cwt_WC_predict_train.csv")
cwt_EWT <- fread("cwt_EWT_predict_train.csv")

#Creating the datasets for ANCOVA
ref_LMA <- cbind(data[, c(1:2, 6)], rep("Reflectance", largo), rowMeans(ref_LMA))
ref_WC <- cbind(data[, c(1:2, 6)], rep("Reflectance", largo), rowMeans(ref_WC))
ref_EWT <- cbind(data[, c(1:2, 6)], rep("Reflectance", largo), rowMeans(ref_EWT))
cwt_LMA <- cbind(data[, c(1:2, 6)], rep("Wavelet", largo), rowMeans(cwt_LMA))
cwt_WC <- cbind(data[, c(1:2, 6)], rep("Wavelet", largo), rowMeans(cwt_WC))
cwt_EWT <- cbind(data[, c(1:2, 6)], rep("Wavelet", largo), rowMeans(cwt_EWT))

colnames(ref_LMA)[4:5] <- c("Spectra", "Predicted")
colnames(ref_WC)[4:5] <- c("Spectra", "Predicted")
colnames(ref_EWT)[4:5] <- c("Spectra", "Predicted")
colnames(cwt_LMA)[4:5] <- c("Spectra", "Predicted")
colnames(cwt_WC)[4:5] <- c("Spectra", "Predicted")
colnames(cwt_EWT)[4:5] <- c("Spectra", "Predicted")

#Add observed values for comparison      
LMA <- data[, c(1,2,6, 3)]
WC <- data[, c(1,2,6, 4)]
EWT <- data[, c(1,2,6, 5)]

colnames(LMA)[4] <- "Observed"
colnames(WC)[4] <- "Observed"
colnames(EWT)[4] <- "Observed"

#Final tables
ref_LMA <- cbind(ref_LMA, LMA[,4])
ref_WC <- cbind(ref_WC, WC[,4])
ref_EWT <- cbind(ref_EWT, EWT[,4])
cwt_LMA <- cbind(cwt_LMA, LMA[,4])
cwt_WC <- cbind(cwt_WC, WC[,4])
cwt_EWT <- cbind(cwt_EWT, EWT[,4])

LMA <- rbind(ref_LMA, cwt_LMA)
WC <- rbind(ref_WC, cwt_WC)
EWT <- rbind(ref_EWT, cwt_EWT)

###ANCOVA model

summary(aov(Observed ~ Predicted*Life_form*Spectra + Error(Sample/(Predicted*Spectra)), data= LMA))
summary(aov(Observed ~ Predicted*Life_form*Spectra + Error(Sample/(Predicted*Spectra)), data= WC))
summary(aov(Observed ~ Predicted*Life_form*Spectra + Error(Sample/(Predicted*Spectra)), data= EWT))


###Performance comparisons
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

data <- rbind(ref_LMA, ref_WC, ref_EWT, cwt_LMA, cwt_WC, cwt_EWT)
data$Spectra <- as.factor(data$Spectra)
data$Samples <- factor(data$Spectra, levels = c("Reflectance", "Wavelet"))

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
data$Life_form <- as.factor(data$Life_form)
data$Life_form <- factor(data$Life_form, levels = c("Lianas", "Trees"))

data <- data[Life_form != "All"]
data <- data[Process == "Training"]

LMA <- data[Trait == "LMA"]
LMA <- LMA[order(Resample, Life_form)]

LMA_ref <- LMA[Spectra == "Reflectance"]
LMA_ref_lianas <- LMA_ref[Life_form == "Lianas"]
LMA_ref_trees <- LMA_ref[Life_form == "Trees"]

LMA_cwt <- LMA[Spectra == "Wavelet"]
LMA_cwt_lianas <- LMA_cwt[Life_form == "Lianas"]
LMA_cwt_trees <- LMA_cwt[Life_form == "Trees"]

WC <- data[Trait == "WC"]
WC <- WC[order(Resample, Life_form)]

WC_ref <- WC[Spectra == "Reflectance"]
WC_ref_lianas <- WC_ref[Life_form == "Lianas"]
WC_ref_trees <- WC_ref[Life_form == "Trees"]

WC_cwt <- WC[Spectra == "Wavelet"]
WC_cwt_lianas <- WC_cwt[Life_form == "Lianas"]
WC_cwt_trees <- WC_cwt[Life_form == "Trees"]

EWT <- data[Trait == "EWT"]
EWT <- EWT[order(Resample, Life_form)]

EWT_ref <- EWT[Spectra == "Reflectance"]
EWT_ref_lianas <- EWT_ref[Life_form == "Lianas"]
EWT_ref_trees <- EWT_ref[Life_form == "Trees"]

EWT_cwt <- EWT[Spectra == "Wavelet"]
EWT_cwt_lianas <- EWT_cwt[Life_form == "Lianas"]
EWT_cwt_trees <- EWT_cwt[Life_form == "Trees"]

#Rsq
t.test(LMA_ref_lianas[Parameter == "Rsq", Value], LMA_ref_trees[Parameter == "Rsq", Value], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt_lianas[Parameter == "Rsq", Value], LMA_cwt_trees[Parameter == "Rsq", Value], alternative = "two.sided", paired = TRUE)

t.test(WC_ref_lianas[Parameter == "Rsq", Value], WC_ref_trees[Parameter == "Rsq", Value], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt_lianas[Parameter == "Rsq", Value], WC_cwt_trees[Parameter == "Rsq", Value], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref_lianas[Parameter == "Rsq", Value], EWT_ref_trees[Parameter == "Rsq", Value], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt_lianas[Parameter == "Rsq", Value], EWT_cwt_trees[Parameter == "Rsq", Value], alternative = "two.sided", paired = TRUE)

#Bias
t.test(LMA_ref_lianas[Parameter == "Bias", Value], LMA_ref_trees[Parameter == "Bias", Value], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt_lianas[Parameter == "Bias", Value], LMA_cwt_trees[Parameter == "Bias", Value], alternative = "two.sided", paired = TRUE)

t.test(WC_ref_lianas[Parameter == "Bias", Value], WC_ref_trees[Parameter == "Bias", Value], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt_lianas[Parameter == "Bias", Value], WC_cwt_trees[Parameter == "Bias", Value], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref_lianas[Parameter == "Bias", Value], EWT_ref_trees[Parameter == "Bias", Value], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt_lianas[Parameter == "Bias", Value], EWT_cwt_trees[Parameter == "Bias", Value], alternative = "two.sided", paired = TRUE)

#RMSE
t.test(LMA_ref_lianas[Parameter == "RMSE", Value], LMA_ref_trees[Parameter == "RMSE", Value], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt_lianas[Parameter == "RMSE", Value], LMA_cwt_trees[Parameter == "RMSE", Value], alternative = "two.sided", paired = TRUE)

t.test(WC_ref_lianas[Parameter == "RMSE", Value], WC_ref_trees[Parameter == "RMSE", Value], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt_lianas[Parameter == "RMSE", Value], WC_cwt_trees[Parameter == "RMSE", Value], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref_lianas[Parameter == "RMSE", Value], EWT_ref_trees[Parameter == "RMSE", Value], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt_lianas[Parameter == "RMSE", Value], EWT_cwt_trees[Parameter == "RMSE", Value], alternative = "two.sided", paired = TRUE)

#RMSE_P_P
t.test(LMA_ref_lianas[Parameter == "RMSE_P", Value], LMA_ref_trees[Parameter == "RMSE_P", Value], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt_lianas[Parameter == "RMSE_P", Value], LMA_cwt_trees[Parameter == "RMSE_P", Value], alternative = "two.sided", paired = TRUE)

t.test(WC_ref_lianas[Parameter == "RMSE_P", Value], WC_ref_trees[Parameter == "RMSE_P", Value], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt_lianas[Parameter == "RMSE_P", Value], WC_cwt_trees[Parameter == "RMSE_P", Value], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref_lianas[Parameter == "RMSE_P", Value], EWT_ref_trees[Parameter == "RMSE_P", Value], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt_lianas[Parameter == "RMSE_P", Value], EWT_cwt_trees[Parameter == "RMSE_P", Value], alternative = "two.sided", paired = TRUE)


