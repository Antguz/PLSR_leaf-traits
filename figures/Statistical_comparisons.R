###############################################################
### Statistical comparisons
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
library(afex)
library(performance)

###Statistical compassion of Table 2--------------------------------------------
#Load files
LMA <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/LMA_performance.csv")
LMA[Spectra == "CWT", Spectra := "Wavelet"]
WC <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/WC_performance.csv")
WC[Spectra == "CWT", Spectra := "Wavelet"]
EWT <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/EWT_performance.csv")
EWT[Spectra == "CWT", Spectra := "Wavelet"]

LMA <- LMA[Life_form == "All"]
WC <- WC[Life_form == "All"]
EWT <- EWT[Life_form == "All"]

###final to keep
LMA_training <- LMA[Model == "Training"]
LMA_testing <- LMA[Model == "Testing"]
WC_training <- WC[Model == "Training"]
WC_testing <- WC[Model == "Testing"]
EWT_training <- EWT[Model == "Training"]
EWT_testing <- EWT[Model == "Testing"]

#LMA
t.test(LMA_training[Spectra == "Reflectance", Rsq], LMA_training[Spectra == "Wavelet", Rsq], alternative = "two.sided", paired = TRUE) 
t.test(LMA_testing[Spectra == "Reflectance", Rsq], LMA_testing[Spectra == "Wavelet", Rsq], alternative = "two.sided", paired = TRUE) 

t.test(LMA_training[Spectra == "Reflectance", Bias], LMA_training[Spectra == "Wavelet", Bias], alternative = "two.sided", paired = TRUE) 
t.test(LMA_testing[Spectra == "Reflectance", Bias], LMA_testing[Spectra == "Wavelet", Bias], alternative = "two.sided", paired = TRUE) 

t.test(LMA_training[Spectra == "Reflectance", RMSE], LMA_training[Spectra == "Wavelet", RMSE], alternative = "two.sided", paired = TRUE) 
t.test(LMA_testing[Spectra == "Reflectance", RMSE], LMA_testing[Spectra == "Wavelet", RMSE], alternative = "two.sided", paired = TRUE) 

t.test(LMA_training[Spectra == "Reflectance", RMSE_P], LMA_training[Spectra == "Wavelet", RMSE_P], alternative = "two.sided", paired = TRUE) 
t.test(LMA_testing[Spectra == "Reflectance", RMSE_P], LMA_testing[Spectra == "Wavelet", RMSE_P], alternative = "two.sided", paired = TRUE)

#WC
t.test(WC_training[Spectra == "Reflectance", Rsq], WC_training[Spectra == "Wavelet", Rsq], alternative = "two.sided", paired = TRUE) 
t.test(WC_testing[Spectra == "Reflectance", Rsq], WC_testing[Spectra == "Wavelet", Rsq], alternative = "two.sided", paired = TRUE) 

t.test(WC_training[Spectra == "Reflectance", Bias], WC_training[Spectra == "Wavelet", Bias], alternative = "two.sided", paired = TRUE) 
t.test(WC_testing[Spectra == "Reflectance", Bias], WC_testing[Spectra == "Wavelet", Bias], alternative = "two.sided", paired = TRUE) 

t.test(WC_training[Spectra == "Reflectance", RMSE], WC_training[Spectra == "Wavelet", RMSE], alternative = "two.sided", paired = TRUE) 
t.test(WC_testing[Spectra == "Reflectance", RMSE], WC_testing[Spectra == "Wavelet", RMSE], alternative = "two.sided", paired = TRUE) 

t.test(WC_training[Spectra == "Reflectance", RMSE_P], WC_training[Spectra == "Wavelet", RMSE_P], alternative = "two.sided", paired = TRUE) 
t.test(WC_testing[Spectra == "Reflectance", RMSE_P], WC_testing[Spectra == "Wavelet", RMSE_P], alternative = "two.sided", paired = TRUE) 

#EWT
t.test(EWT_training[Spectra == "Reflectance", Rsq], EWT_training[Spectra == "Wavelet", Rsq], alternative = "two.sided", paired = TRUE) 
t.test(EWT_testing[Spectra == "Reflectance", Rsq], EWT_testing[Spectra == "Wavelet", Rsq], alternative = "two.sided", paired = TRUE) 

t.test(EWT_training[Spectra == "Reflectance", Bias], EWT_training[Spectra == "Wavelet", Bias], alternative = "two.sided", paired = TRUE) 
t.test(EWT_testing[Spectra == "Reflectance", Bias], EWT_testing[Spectra == "Wavelet", Bias], alternative = "two.sided", paired = TRUE) 

t.test(EWT_training[Spectra == "Reflectance", RMSE], EWT_training[Spectra == "Wavelet", RMSE], alternative = "two.sided", paired = TRUE) 
t.test(EWT_testing[Spectra == "Reflectance", RMSE], EWT_testing[Spectra == "Wavelet", RMSE], alternative = "two.sided", paired = TRUE) 

t.test(EWT_training[Spectra == "Reflectance", RMSE_P], EWT_training[Spectra == "Wavelet", RMSE_P], alternative = "two.sided", paired = TRUE) 
t.test(EWT_testing[Spectra == "Reflectance", RMSE_P], EWT_testing[Spectra == "Wavelet", RMSE_P], alternative = "two.sided", paired = TRUE) 


###ANCOVA comparisons Table S3--------------------------------------------------
#Training data
LMA_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/06-Predict_train/LMA_predict_training.csv")
LMA_predict[Spectra == "CWT", Spectra := "Wavelet"]
WC_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/06-Predict_train/WC_predict_training.csv")
WC_predict[Spectra == "CWT", Spectra := "Wavelet"]
EWT_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/06-Predict_train/EWT_predict_training.csv")
EWT_predict[Spectra == "CWT", Spectra := "Wavelet"]

#Data manage
LMA  <- data.table::melt(LMA_predict, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
LMA <- LMA[, list(mean(Trait)), by = c("Spectra", "variable")]
colnames(LMA)[3] <- c("mean")
LMA <- LMA[order(Spectra, variable)]

WC  <- data.table::melt(WC_predict, id.vars=c("Spectra", "iteration"),
            measure.vars = .SD,
            value.name = "Trait")
WC <- WC[, list(mean(Trait)), by = c("Spectra", "variable")]
colnames(WC)[3] <- c("mean")
WC <- WC[order(Spectra, variable)]

EWT  <- data.table::melt(EWT_predict, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
EWT <- EWT[, list(mean(Trait)), by = c("Spectra", "variable")]
colnames(EWT)[3] <- c("mean")
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
observed[, Sample := paste0("Sample_", Sample)]

ref_LMA <- cbind(observed[,c(1:2,3)], LMA[Spectra == "Reflectance"])
ref_WC <- cbind(observed[,c(1:2,4)], WC[Spectra == "Reflectance"])
ref_EWT <- cbind(observed[,c(1:2,5)], EWT[Spectra == "Reflectance"]) 
cwt_LMA <- cbind(observed[,c(1:2,3)], LMA[Spectra == "Wavelet"])
cwt_WC <- cbind(observed[,c(1:2,4)], WC[Spectra == "Wavelet"])
cwt_EWT <- cbind(observed[,c(1:2,5)], EWT[Spectra == "Wavelet"])

LMA <- rbind(ref_LMA, cwt_LMA)
colnames(LMA) <- c("Life_form", "Species", "Observed", "Spectra", "Sample", "Predicted")
WC <- rbind(ref_WC, cwt_WC)
colnames(WC) <- c("Life_form", "Species", "Observed", "Spectra", "Sample", "Predicted")
EWT <- rbind(ref_EWT, cwt_EWT)
colnames(EWT) <- c("Life_form", "Species", "Observed", "Spectra", "Sample", "Predicted")

options(contrasts = c("contr.sum","contr.poly"))
#Linear mixed model for training
summary(aov(log10(Observed) ~ log10(Predicted)*Life_form*Spectra + Error(factor(Sample)), data= LMA))
summary(aov(log10(Observed) ~ log10(Predicted)*Life_form*Spectra + Error(factor(Sample)), data= WC))
summary(aov(log10(Observed) ~ log10(Predicted)*Life_form*Spectra + Error(factor(Sample)), data= EWT))

#Testing data------------------
LMA_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/LMA_predict_testing.csv")
LMA_predict[Spectra == "CWT", Spectra := "Wavelet"]
WC_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/WC_predict_testing.csv")
WC_predict[Spectra == "CWT", Spectra := "Wavelet"]
EWT_predict <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/07-Predict_test/EWT_predict_testing.csv")
EWT_predict[Spectra == "CWT", Spectra := "Wavelet"]

#Data manage
LMA  <- melt(LMA_predict, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
LMA <- LMA[, list(mean(Trait)), by = c("Spectra", "variable")]
colnames(LMA)[3] <- c("mean")
LMA <- LMA[order(Spectra, variable)]

WC  <- melt(WC_predict, id.vars=c("Spectra", "iteration"),
            measure.vars = .SD,
            value.name = "Trait")
WC <- WC[, list(mean(Trait)), by = c("Spectra", "variable")]
colnames(WC)[3] <- c("mean")
WC <- WC[order(Spectra, variable)]

EWT  <- melt(EWT_predict, id.vars=c("Spectra", "iteration"),
             measure.vars = .SD,
             value.name = "Trait")
EWT <- EWT[, list(mean(Trait)), by = c("Spectra", "variable")]
colnames(EWT)[3] <- c("mean")
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

ref_LMA <- cbind(observed[,c(1:2,3)], LMA[Spectra == "Reflectance"])
ref_WC <- cbind(observed[,c(1:2,4)], WC[Spectra == "Reflectance"])
ref_EWT <- cbind(observed[,c(1:2,5)], EWT[Spectra == "Reflectance"]) 
cwt_LMA <- cbind(observed[,c(1:2,3)], LMA[Spectra == "Wavelet"])
cwt_WC <- cbind(observed[,c(1:2,4)], WC[Spectra == "Wavelet"])
cwt_EWT <- cbind(observed[,c(1:2,5)], EWT[Spectra == "Wavelet"])

LMA <- rbind(ref_LMA, cwt_LMA)
colnames(LMA) <- c("Life_form", "Species", "Observed", "Spectra", "Sample", "Predicted")
WC <- rbind(ref_WC, cwt_WC)
colnames(WC) <- c("Life_form", "Species", "Observed", "Spectra", "Sample", "Predicted")
EWT <- rbind(ref_EWT, cwt_EWT)
colnames(EWT) <- c("Life_form", "Species", "Observed", "Spectra", "Sample", "Predicted")

###ANCOVA model for training
summary(aov(log10(Observed) ~ log10(Predicted)*Life_form*Spectra + Error(factor(Sample)), data= LMA))
summary(aov(log10(Observed) ~ log10(Predicted)*Life_form*Spectra + Error(factor(Sample)), data= WC))
summary(aov(log10(Observed) ~ log10(Predicted)*Life_form*Spectra + Error(factor(Sample)), data= EWT))

###Performance comparisons Table S4--------------------------------------------
#Load files
LMA <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/LMA_performance.csv")
LMA[Spectra == "CWT", Spectra := "Wavelet"]
WC <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/WC_performance.csv")
WC[Spectra == "CWT", Spectra := "Wavelet"]
EWT <- fread("/home/antguz/Documents/PLSR-models/Data/04-results/02-Performance/EWT_performance.csv")
EWT[Spectra == "CWT", Spectra := "Wavelet"]

LMA <- LMA[Model == "Testing"]
WC <- WC[Model == "Testing"]
EWT <- EWT[Model == "Testing"]

#To compare
LMA_ref <- LMA[Spectra == "Reflectance"]
LMA_cwt <- LMA[Spectra == "Wavelet"]

WC_ref <- WC[Spectra == "Reflectance"]
WC_cwt <- WC[Spectra == "Wavelet"]

EWT_ref <- EWT[Spectra == "Reflectance"]
EWT_cwt <- EWT[Spectra == "Wavelet"]

#Rsq
t.test(LMA_ref[Life_form == "Liana", Rsq], LMA_ref[Life_form == "Tree", Rsq], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt[Life_form == "Liana", Rsq], LMA_cwt[Life_form == "Tree", Rsq], alternative = "two.sided", paired = TRUE)

t.test(WC_ref[Life_form == "Liana", Rsq], WC_ref[Life_form == "Tree", Rsq], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt[Life_form == "Liana", Rsq], WC_cwt[Life_form == "Tree", Rsq], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref[Life_form == "Liana", Rsq], EWT_ref[Life_form == "Tree", Rsq], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt[Life_form == "Liana", Rsq], EWT_cwt[Life_form == "Tree", Rsq], alternative = "two.sided", paired = TRUE)

#Bias
t.test(LMA_ref[Life_form == "Liana", Bias], LMA_ref[Life_form == "Tree", Bias], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt[Life_form == "Liana", Bias], LMA_cwt[Life_form == "Tree", Bias], alternative = "two.sided", paired = TRUE)

t.test(WC_ref[Life_form == "Liana", Bias], WC_ref[Life_form == "Tree", Bias], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt[Life_form == "Liana", Bias], WC_cwt[Life_form == "Tree", Bias], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref[Life_form == "Liana", Bias], EWT_ref[Life_form == "Tree", Bias], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt[Life_form == "Liana", Bias], EWT_cwt[Life_form == "Tree", Bias], alternative = "two.sided", paired = TRUE)

#RMSE
t.test(LMA_ref[Life_form == "Liana", RMSE], LMA_ref[Life_form == "Tree", RMSE], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt[Life_form == "Liana", RMSE], LMA_cwt[Life_form == "Tree", RMSE], alternative = "two.sided", paired = TRUE)

t.test(WC_ref[Life_form == "Liana", RMSE], WC_ref[Life_form == "Tree", RMSE], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt[Life_form == "Liana", RMSE], WC_cwt[Life_form == "Tree", RMSE], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref[Life_form == "Liana", RMSE], EWT_ref[Life_form == "Tree", RMSE], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt[Life_form == "Liana", RMSE], EWT_cwt[Life_form == "Tree", RMSE], alternative = "two.sided", paired = TRUE)

#RMSE_P_P
t.test(LMA_ref[Life_form == "Liana", RMSE_P], LMA_ref[Life_form == "Tree", RMSE_P], alternative = "two.sided", paired = TRUE)
t.test(LMA_cwt[Life_form == "Liana", RMSE_P], LMA_cwt[Life_form == "Tree", RMSE_P], alternative = "two.sided", paired = TRUE)

t.test(WC_ref[Life_form == "Liana", RMSE_P], WC_ref[Life_form == "Tree", RMSE_P], alternative = "two.sided", paired = TRUE)
t.test(WC_cwt[Life_form == "Liana", RMSE_P], WC_cwt[Life_form == "Tree", RMSE_P], alternative = "two.sided", paired = TRUE)

t.test(EWT_ref[Life_form == "Liana", RMSE_P], EWT_ref[Life_form == "Tree", RMSE_P], alternative = "two.sided", paired = TRUE)
t.test(EWT_cwt[Life_form == "Liana", RMSE_P], EWT_cwt[Life_form == "Tree", RMSE_P], alternative = "two.sided", paired = TRUE)

