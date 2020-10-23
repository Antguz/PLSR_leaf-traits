##############################################################################
############ Data processing for the manuscript
##############################################################################

###This scrip represent the main data processing and analysis for the manuscript 
#'Prediction of leaf traits of lianas and trees via the integration of wavelet spectra 
# in the visible-near infrared and thermal infrared domains' by Guzman and Sanchez-Azofeifa. 
# The data for this manuscript is available at https://doi.org/10.7910/DVN/YBYO7W. Please cite this
# manuscript if the following code helps in your research.

###Load libraries

library(signal)
library(prospectr)
library(wmtsa)
library(car)
library(caret)
library(AppliedPredictiveModeling)
library(pls)
library(DescTools)
library(data.table)
library(parallel)
library(plsVarSel)

###Select work directy
setwd("C:/Users/josea/OneDrive/Escritorio/Codes") #This most be corrected by each user

#Note: the numbers created on the files names refers to the level of processing.

###-----------------------------------------01-Resampling-------------------------------------------#####
###The first step is associated with the spectral resampling

data_vis <- read.csv("01-vis_raw-data.csv") #vis-nir
data_lwir <- read.csv("01-lwir_raw-data.csv") #lwri

resample_savgolay <- function(data, by = 0.001, p = 2, n = 25){ #Function to use
  wav <- data$Wavelength
  new.wave <- seq(min(wav), max(wav), by)
  row_length <- length(names(data))-1
  complete.data <- data[1:length(new.wave),]
  complete.data$Wavelength <- new.wave
  
  for(i in 1:row_length) {
    value <- data[,(1+i)]
    res <- resample(value, wav, new.wave)
    complete.data[,(1+i)] <- filter(filt = sgolay(p = p, n = n), x = res)
  }
  complete.data
}

#Aplication of the function
data_vis <- resample_savgolay(data_vis, by = 0.001, p = 2, n = 25) #Resample to 0.001 nm
data_lwir <- resample_savgolay(data_lwir, by = 0.01, p = 2, n = 25) #Resample to 0.01 nm

#Export the products
write.csv(data_vis, "02-vis_ressav.csv", row.names = FALSE)
write.csv(data_lwir, "02-lwir_ressav.csv", row.names = FALSE)

###--------------------------------------03-CWT------------------------------------------------------#####
### The following steps are associated with the application of the continuous wavelet transformation (CWT)

data_vis <- read.csv("02-vis_ressav.csv") #vis-nir
data_lwir <- read.csv("02-lwir_ressav.csv") #lwir

CWT <- function(data, range) { #CWT function to apply, range represent the resulting spectral range to return
  complete <- data
  row_length <- length(names(data))-1
  
  for(i in 1:row_length) {
    value <- data[,(1+i)]
    cwt_value <- wavCWT(value, n.scale= 15, scale.range= c(1, 9), variance= 1, wavelet= "gaussian2")
    cwt_value <- rowSums(as.matrix(cwt_value)[,2:5])
    complete[,(1+i)] <- cwt_value
  }
  complete <- subset(complete, Wavelength >= range[1] & Wavelength <= range[2])
  complete
}

#Aplication of the function
data_vis <- CWT(data_vis, c(0.450, 1.0)) #The application of the function VIS-NIR data
data_lwir <- CWT(data_lwir, c(2.6, 13)) #The application of the function to LWIR data

#Export the products
write.csv(data_vis, "03-vis_cwt.csv", row.names = FALSE)
write.csv(data_lwir, "03-lwir_cwt.csv", row.names = FALSE)

###--------------------------------------04-data organization------------------------------------------------------#####
###The following steps create the data organization for data split.

#Non-transformed spectra data
vis_ref <- fread("02-vis_ressav.csv")
lwir_ref <- fread("02-lwir_ressav.csv")

vis_ref <- subset(vis_ref, Wavelength >= 0.45 & Wavelength <= 1.0) #Subset to a especific spectral range
lwir_ref <- subset(lwir_ref, Wavelength >= 2.6 & Wavelength <= 13) #Subset to a especific spectral range

#Transformed spectra data
vis_cwt <- fread("03-vis_cwt.csv")
lwir_cwt <- fread("03-lwir_cwt.csv")

#Load the file of leaf traits
ID <- fread("01-traits.csv")

#Log transform the leaf traits
ID$logLMA <- log10(ID$LMA)
ID$logWC <- log10(ID$WC)
ID$logEWT <- log10(ID$EWT)

#Data set to use
ID_keep <- ID[, c(1, 5, 12, 13, 14)] #Subset dataset to Life_form, ID_collection, and leaf traits
ID_keep$Sample <- 1:700 #Create a column of sample ID

#Merge spectra data from both sensors
ref <- rbind(vis_ref, lwir_ref, use.names = FALSE)
cwt <- rbind(vis_cwt, lwir_cwt, use.names = FALSE)

#Transpose the spectra data for further processing
ref <- dcast(melt(ref, id.vars = "Wavelength"), variable ~ Wavelength)
cwt <- dcast(melt(cwt, id.vars = "Wavelength"), variable ~ Wavelength)

#Creates datasests for reflectance and wavelet spectra.
ref <- as.data.frame(cbind(ID_keep, ref[,-1]))
cwt <- as.data.frame(cbind(ID_keep, cwt[,-1]))

#Export the products
write.csv(ref, "04-ref.csv", row.names = FALSE)
write.csv(cwt, "04-cwt.csv", row.names = FALSE)

###--------------------------------------------Data split--------------------------------------###
###The following steps are associated with the balanced data split for training and testing

#Note: please keep the following outputs in the environment for futher processing of the PLSR model.

#Create partitions for training and testing
#Random selection of the samples according the sample size of lianas and trees
set.seed(1107)
sample_liana <- sample(1:280, 210) #Sample size of lianas
set.seed(1107)
sample_tree <- sample(1:420, 210) #Sample size of trees

     
data_split_liana <- createDataPartition(ID_keep[Life_form == "Liana"]$ID_collection, p = 0.75, 
                                  list = FALSE, 
                                  times = 1) #Data split of lianas

data_split_tree <- createDataPartition(ID_keep[Life_form == "Tree"]$ID_collection, p = 0.5, 
                                        list = FALSE, 
                                        times = 1) #Data split of trees

data_split <- c(ID_keep[Life_form == "Liana", Sample][data_split_liana], ID_keep[Life_form == "Tree", Sample][data_split_tree]) #Data split of lianas and trees
data_split <- data_split[order(data_split)] #Order of samples according to sample ID

###Reflectance partitions for training and testing
ref_sourceTrain <- ref[data_split,]
ref_sourceTest  <- ref[-data_split,]

###CWT partitions for training and testing
cwt_sourceTrain <- cwt[data_split,]
cwt_sourceTest  <- cwt[-data_split,]

#Check numbers of samples
table(ref_sourceTrain$Life_form)
table(ref_sourceTest$Life_form)

table(cwt_sourceTrain$Life_form)
table(cwt_sourceTest$Life_form)

#Export the samples for training and testing
train_ID <- ID_keep[data_split]
test_ID <- ID_keep[-data_split]
fwrite(train_ID, "traits_training.csv")
fwrite(test_ID, "traits_testing.csv")

###--------------------------------------------Data for model--------------------------------------###
###The following steps are associated data inputs for the PLSR model.

#training
ref_LMA_train <- ref_sourceTrain[, c(3, 7:1596)]
ref_WC_train <- ref_sourceTrain[, c(4, 7:1596)]
ref_EWT_train <- ref_sourceTrain[, c(5, 7:1596)]

cwt_LMA_train <- cwt_sourceTrain[, c(3, 7:1596)]
cwt_WC_train <- cwt_sourceTrain[, c(4, 7:1596)]
cwt_EWT_train <- cwt_sourceTrain[, c(5, 7:1596)]

#testing
ref_LMA_test <- ref_sourceTest[, c(3, 7:1596)]
ref_WC_test <- ref_sourceTest[, c(4, 7:1596)]
ref_EWT_test <- ref_sourceTest[, c(5, 7:1596)]

cwt_LMA_test <- cwt_sourceTest[, c(3, 7:1596)]
cwt_WC_test <- cwt_sourceTest[, c(4, 7:1596)]
cwt_EWT_test <- cwt_sourceTest[, c(5, 7:1596)]

###------------------------------Estimation of the optimal number of components--------------------------------------###
###The following steps are associated with the estimation of the optimal number of components of the section 2.4.1.

#Load the function script
source("optimal_number.R") ###Note this function run in parallel, please take a look to its arguments before to run the code.
#Please also note that this function returns several arguments, for our manuscript we focused our attention to the onesigma method on the frame_comp list

#Function to estimate the mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#LMA optimal number of components bases on reflectance
opt_ref_LMA <- optimal_number(train = ref_LMA_train, model = logLMA ~ ., int_ncomp = 65, iterations = 100, k = 10)
fwrite(opt_ref_LMA$frame_comp, "opt_ref_LMA.csv")
fwrite(as.data.table(opt_ref_LMA$RMSEP_out), "RMSE_ref_LMA.csv")
fwrite(as.data.table(opt_ref_LMA$PRESS), "PRESS_ref_LMA.csv")
onesigma_ref_LMA <- opt_ref_LMA$frame_comp$Onesigma
mode(onesigma_ref_LMA)

#WC optimal number of components bases on reflectance
opt_ref_WC <- optimal_number(ref_WC_train, model = logWC ~ ., int_ncomp = 65, iterations = 100, k = 10)
fwrite(opt_ref_WC$frame_comp, "opt_ref_WC.csv")
fwrite(as.data.table(opt_ref_WC$RMSE), "RMSE_ref_WC.csv")
fwrite(as.data.table(opt_ref_WC$PRESS), "PRESS_ref_WC.csv")
onesigma_ref_WC <- opt_ref_WC$frame_comp$Onesigma
mode(onesigma_ref_WC)

#EWT optimal number of components bases on reflectance
opt_ref_EWT <- optimal_number(ref_EWT_train, model = logEWT ~ ., int_ncomp = 65, iterations = 100, k = 10)
fwrite(opt_ref_EWT$frame_comp, "opt_ref_EWT.csv")
fwrite(as.data.table(opt_ref_EWT$RMSE), "RMSE_ref_EWT.csv")
fwrite(as.data.table(opt_ref_EWT$PRESS), "PRESS_ref_EWT.csv")
onesigma_ref_EWT <- opt_ref_EWT$frame_comp$Onesigma
mode(onesigma_ref_EWT)

#LMA optimal number of components bases on wavelet spectra
opt_cwt_LMA <- optimal_number(train = cwt_LMA_train, model = logLMA ~ ., int_ncomp = 65, iterations = 100, k = 10)
fwrite(opt_cwt_LMA$frame_comp, "opt_cwt_LMA.csv")
fwrite(as.data.table(opt_cwt_LMA$RMSE), "RMSE_cwt_LMA.csv")
fwrite(as.data.table(opt_cwt_LMA$PRESS), "PRESS_cwt_LMA.csv")
onesigma_cwt_LMA <- opt_cwt_LMA$frame_comp$Onesigma
mode(onesigma_cwt_LMA)

#WC optimal number of components bases on wavelet spectra
opt_cwt_WC <- optimal_number(cwt_WC_train, model = logWC ~ ., int_ncomp = 65, iterations = 100, k = 10)
fwrite(opt_cwt_WC$frame_comp, "opt_cwt_WC.csv")
fwrite(as.data.table(opt_cwt_WC$RMSE), "RMSE_cwt_WC.csv")
fwrite(as.data.table(opt_cwt_WC$PRESS), "PRESS_cwt_WC.csv")
onesigma_cwt_WC <- opt_cwt_WC$frame_comp$Onesigma
mode(onesigma_cwt_WC)

#EWT optimal number of components bases on wavelet spectra
opt_cwt_EWT <- optimal_number(cwt_EWT_train, model = logEWT ~ ., int_ncomp = 65, iterations = 100, k = 10)
fwrite(opt_cwt_EWT$frame_comp, "opt_cwt_EWT.csv")
fwrite(as.data.table(opt_cwt_EWT$RMSE), "RMSE_cwt_EWT.csv")
fwrite(as.data.table(opt_cwt_EWT$PRESS), "PRESS_cwt_EWT.csv")
onesigma_cwt_EWT <- opt_cwt_EWT$frame_comp$Onesigma
mode(onesigma_cwt_EWT)

###------------------------------Evaluation of the variability of the predictor variables and the model performance--------------------------------------###
###The following steps are associated with the evaluation of the variability of the predictor variables and the model performance, sections 2.4.2 and 2.4.3.

#Load the function script
source("pls_evaluation.R") ###Note this function run in parallel, please take a look to its arguments before to run the code.
#Please also note that this function returns several arguments.
#On n_comp you can select the number of components to run.
#On resamples you can select the number of iterations to run.
#On split, you can select the number of data used to validation of the training model.
#This function needs the tranining and testing datasets with their IDs.
#The process of training and testing is performed simultaneously.

#Mode 34 for optimal number of components
ref_LMA <- pls_evaluation(train = ref_LMA_train, train_ID = train_ID, test = ref_LMA_test, test_ID = test_ID, model = logLMA ~ ., n_comp = 34, resamples = 1000, split = 0.7)
fwrite(ref_LMA$Coefficients, "ref_LMA_coefficients.csv")
fwrite(ref_LMA$VIP, "ref_LMA_VIP.csv")
fwrite(ref_LMA$Stats, "ref_LMA_stats.csv")
fwrite(ref_LMA$Predict_train, "ref_LMA_predict_train.csv")
fwrite(ref_LMA$Predict_test, "ref_LMA_predict_test.csv")
fwrite(ref_LMA$Residuals_train, "ref_LMA_residuals_train.csv")
fwrite(ref_LMA$Residuals_test, "ref_LMA_residuals_test.csv")

#Mode 30 for optimal number of components
ref_WC <- pls_evaluation(ref_WC_train, train_ID = train_ID, test = ref_WC_test, test_ID = test_ID, model = logWC ~ ., n_comp = 30, resamples = 1000, split = 0.7)
fwrite(ref_WC$Coefficients, "ref_WC_coefficients.csv")
fwrite(ref_WC$VIP, "ref_WC_VIP.csv")
fwrite(ref_WC$Stats, "ref_WC_stats.csv")
fwrite(ref_WC$Predict_train, "ref_WC_predict_train.csv")
fwrite(ref_WC$Predict_test, "ref_WC_predict_test.csv")
fwrite(ref_WC$Residuals_train, "ref_WC_residuals_train.csv")
fwrite(ref_WC$Residuals_test, "ref_WC_residuals_test.csv")

#Mode 24 for optimal number of components
ref_EWT <- pls_evaluation(ref_EWT_train, train_ID = train_ID, test = ref_EWT_test, test_ID = test_ID, model = logEWT ~ ., n_comp = 24, resamples = 1000, split = 0.7)
fwrite(ref_EWT$Coefficients, "ref_EWT_coefficients.csv")
fwrite(ref_EWT$VIP, "ref_EWT_VIP.csv")
fwrite(ref_EWT$Stats, "ref_EWT_stats.csv")
fwrite(ref_EWT$Predict_train, "ref_EWT_predict_train.csv")
fwrite(ref_EWT$Predict_test, "ref_EWT_predict_test.csv")
fwrite(ref_EWT$Residuals_train, "ref_EWT_residuals_train.csv")
fwrite(ref_EWT$Residuals_test, "ref_EWT_residuals_test.csv")

#Mode 15 for optimal number of components
cwt_LMA <- pls_evaluation(cwt_LMA_train, train_ID = train_ID, test = cwt_LMA_test, test_ID = test_ID, model = logLMA ~ ., n_comp = 15, resamples = 1000, split = 0.7)
fwrite(cwt_LMA$Coefficients, "cwt_LMA_coefficients.csv")
fwrite(cwt_LMA$VIP, "cwt_LMA_VIP.csv")
fwrite(cwt_LMA$Stats, "cwt_LMA_stats.csv")
fwrite(cwt_LMA$Predict_train, "cwt_LMA_predict_train.csv")
fwrite(cwt_LMA$Predict_test, "cwt_LMA_predict_test.csv")
fwrite(cwt_LMA$Residuals_train, "cwt_LMA_residuals_train.csv")
fwrite(cwt_LMA$Residuals_test, "cwt_LMA_residuals_test.csv")

#Mode 14 for optimal number of components
cwt_WC <- pls_evaluation(cwt_WC_train, train_ID = train_ID, test = cwt_WC_test, test_ID = test_ID, model = logWC ~ ., n_comp = 14, resamples = 1000, split = 0.7)
fwrite(cwt_WC$Coefficients, "cwt_WC_coefficients.csv")
fwrite(cwt_WC$VIP, "cwt_WC_VIP.csv")
fwrite(cwt_WC$Stats, "cwt_WC_stats.csv")
fwrite(cwt_WC$Predict_train, "cwt_WC_predict_train.csv")
fwrite(cwt_WC$Predict_test, "cwt_WC_predict_test.csv")
fwrite(cwt_WC$Residuals_train, "cwt_WC_residuals_train.csv")
fwrite(cwt_WC$Residuals_test, "cwt_WC_residuals_test.csv")

#Mode 11 for optimal number of components
cwt_EWT <- pls_evaluation(cwt_EWT_train, train_ID = train_ID, test = cwt_EWT_test, test_ID = test_ID, model = logEWT ~ ., n_comp = 11, resamples = 1000, split = 0.7)
fwrite(cwt_EWT$Coefficients, "cwt_EWT_coefficients.csv")
fwrite(cwt_EWT$VIP, "cwt_EWT_VIP.csv")
fwrite(cwt_EWT$Stats, "cwt_EWT_stats.csv")
fwrite(cwt_EWT$Predict_train, "cwt_EWT_predict_train.csv")
fwrite(cwt_EWT$Predict_test, "cwt_EWT_predict_test.csv")
fwrite(cwt_EWT$Residuals_train, "cwt_EWT_residuals_train.csv")
fwrite(cwt_EWT$Residuals_test, "cwt_EWT_residuals_test.csv")

#------------------------------End-------------------------#
#For visualization take a look to the figure scripts.






