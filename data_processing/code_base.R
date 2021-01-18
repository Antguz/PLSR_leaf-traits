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
library(foreach)

###Select work directory
setwd("/home/antguz/Documents/PLSR-models/Data/01-Master data") #This most be corrected by each user

#Note: the numbers created on the files names refers to the level of processing.

###----------------------------------01-Spectral resampling---------------------------------#####
### The fist step is associated with the resampling of the MLWIR spectra to match the 
#   spectral resolution of the VIS-NIR spectrum.  The spectral resampling is conducted using 
#   a gaussian function based on the Full Width Half Maximum values. 

###Arguments
#  data is the MLWIR spectra to filter columns as samples, rows as bands
#  by is a vector number describing the new band spacing

###Function
resample_spectra <- function(data, by){
  wav <- data$Wavelength
  new.wave <- seq(min(wav), max(wav), by)
  row_length <- length(names(data))-1
  complete.data <- data[1:length(new.wave),]
  complete.data$Wavelength <- new.wave
  
  for(i in 1:row_length) {
    value <- data[[(1+i)]]
    res <- resample2(value, wav, new.wave)
    complete.data[,(1+i)] <- res
  }
  complete.data
}

###Apply function
#Read data
data_vis <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/01-vis_raw-data.csv")
data_lwir <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/01-lwir_raw-data.csv") #MLWIR spectra

#Application of the function
data_vis_preprocessed <- resample_spectra(data_vis, by = 0.0033) #It is applied on VIS-NIR
data_vis_preprocessed <- data_vis_preprocessed[c(3:(nrow(data_vis_preprocessed)-2)),] #Delete edge bands

data_lwir_preprocessed <- resample_spectra(data_lwir, by = 0.0033) #Application of MLWIR spectra
data_lwir_preprocessed <- data_lwir_preprocessed[c(3:(nrow(data_lwir_preprocessed)-2)),] #Delete edge bands

#Export the results
write.csv(data_vis_preprocessed, "02-data_vis_preprocessed.csv", row.names = FALSE)
write.csv(data_lwir_preprocessed, "02-data_lwir_preprocessed.csv", row.names = FALSE)

###--------------------------------------02-Spectra datasets------------------------------------------------------#####
### The following steps are associated with creation of the spectra datasets for 
#   predicting leaf traits. Two datasets are used: reflectance spectra and CWT spectra. 
#   The edge of the sensors range is removed to ensure a reduced of noise.

#   data_vis = leaf spectra in the VIS-NIR region with a 3.3 nm band spacing.
#   data_lwir = leaf spectra in the MLWIR region with a 3.3 nm band spacing.
#   range_vis = range of the VIS-NIR region to keep.
#   range_lwir = range of the MLWIR region to keep.
#   CWT = logic. If true it applies the continuum wavelet transformation using 
#   five scales: 1, 2, 3, 8, and 9.
#   The function returns a data.table with samples as rows and wavelength bands as columns.

###Function
spectra_datasets <- function(data_vis, data_lwir, range_vis, range_lwir, by_vis = 0.003, by_lwir = 0.003, CWT) {
  
  #Wavelength
  wavlength_vis <- data_vis[, 1]
  wavlength_lwir <- data_lwir[, 1]
  
  ###Creation of ranges
  vis_keep <- wavlength_vis[[1]] >= range_vis[1] & wavlength_vis[[1]] <= range_vis[2]
  lwir_keep <- wavlength_lwir[[1]] >= range_lwir[1] & wavlength_lwir[[1]] <= range_lwir[2]
  
  #Wavelength names
  wavelength_names <- rbind(wavlength_vis[vis_keep == TRUE, ], wavlength_lwir[lwir_keep == TRUE,])
  wavelength_names <- as.character(wavelength_names[[1]])
  
  ###Total number of wavelength to keep
  total_rows <- length(wavelength_names)
  
  #Number of samples
  n_samples <- ncol(data_vis) - 1 
  
  complete_vis <- data_vis
  complete_lwir <- data_lwir
  
  if(CWT == TRUE) {
    for(i in 1:n_samples) {
      
      ###VIS
      value_vis <- data_vis[[(1+i)]] ###Transform the VIS-NIR spectrum 
      value_vis <- ts(value_vis, start = min(wavlength_vis), deltat = by_vis)
      cwt_value_vis <- wavCWT(value_vis, n.scale= 13, scale.range= c(0.0033, 0.0297), variance= 1, wavelet= "gaussian2") 
      ###This scale range need to be corrected if the user wants other scales
      complete_vis[[(1+i)]] <- rowSums(as.matrix(cwt_value_vis)[,c(1, 2, 3, 8, 9)])
      
      #MLWIR
      value_lwir <- data_lwir[[(1+i)]] ###Transform the lwir spectrum
      value_lwir <- ts(value_lwir, start = min(wavlength_lwir), deltat = by_lwir)
      cwt_value_lwir <- wavCWT(value_lwir, n.scale= 13, scale.range= c(0.0033,  0.0297), variance= 1, wavelet= "gaussian2")
      ###This scale range need to be corrected if the user wants other scales
      complete_lwir[[(1+i)]] <- rowSums(as.matrix(cwt_value_lwir)[, c(1, 2, 3, 8, 9)])
      
    }
  }
  
  complete_vis <- complete_vis[vis_keep,]
  complete_lwir <- complete_lwir[lwir_keep,]
  
  return_frame <- as.data.table(cbind(t(complete_vis), t(complete_lwir)))
  return_frame <- return_frame[-1,]
  
  colnames(return_frame) <- wavelength_names
  
  return(return_frame)
}

###Apply function
#Read leaf spectra
data_vis_preprocessed <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/02-data_vis_preprocessed.csv") #VIS-NIR spectra
data_lwir_preprocessed <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/02-data_lwir_preprocessed.csv") #MLWIR spectra filtered by noise.

#Select spectral range
range_vis <- c(0.450, 1.0)
range_lwir <- c(2.55, 11)

#Application of the function
ref <- spectra_datasets(data_vis_preprocessed, data_lwir_preprocessed, range_vis, range_lwir, CWT = FALSE) #Reflectance spectra
cwt <- spectra_datasets(data_vis_preprocessed, data_lwir_preprocessed, range_vis, range_lwir, CWT = TRUE) #CWT spectra

#Export the products
write.csv(ref, "03-ref.csv", row.names = FALSE)
write.csv(cwt, "03-cwt.csv", row.names = FALSE)

###----------------------------------03-data organization of leaf traits------------------------------#####
### The following steps create organize the leaf traits for data split.

#Load the file of leaf traits and leaf spectra
traits <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/01-traits.csv")

#Load leaf spectra
ref <- fread("03-ref.csv", header = TRUE)
cwt <- fread("03-cwt.csv", header = TRUE)

#Log transform the leaf traits
traits$logLMA <- log10(traits$LMA)
traits$logWC <- log10(traits$WC)
traits$logEWT <- log10(traits$EWT)

#Data set to use
traits_keep <- traits[, c(1, 5, 14, 15, 16)] #Subset dataset to Life_form, ID_collection, and leaf traits
traits_keep$Sample <- 1:700 #Create a column of sample ID

#Creates datasests for reflectance and wavelet spectra.
ref <- as.data.frame(cbind(traits_keep, ref))
cwt <- as.data.frame(cbind(traits_keep, cwt))

#Export the products
write.csv(ref, "04-ref.csv", row.names = FALSE)
write.csv(cwt, "04-cwt.csv", row.names = FALSE)

###--------------------------------------------Data split--------------------------------------###
###The following steps are associated with the balanced data split for training and testing

#Note: please keep the following outputs in the environment for further processing of the PLSR model.

#Create partitions for training and testing
#Random selection of the samples according the sample size of lianas and trees.
set.seed(1107)
sample_liana <- sample(1:280, 210) #Sample size of lianas
set.seed(1107)
sample_tree <- sample(1:420, 210) #Sample size of trees

#Data split of lianas
data_split_liana <- createDataPartition(traits_keep[Life_form == "Liana"]$ID_collection, p = 0.75, 
                                  list = FALSE, 
                                  times = 1) #Data split of lianas

#Data split of trees
data_split_tree <- createDataPartition(traits_keep[Life_form == "Tree"]$ID_collection, p = 0.5, 
                                        list = FALSE, 
                                        times = 1) #Data split of trees

#Merged vector of data split
data_split <- c(traits_keep[Life_form == "Liana", Sample][data_split_liana], traits_keep[Life_form == "Tree", Sample][data_split_tree]) #Data split of lianas and trees
data_split <- data_split[order(data_split)] #Order of samples according to sample ID


###Create data partitions
#Reflectance partitions for training and testing
ref_sourceTrain <- ref[data_split,]
ref_sourceTest  <- ref[-data_split,]

#CWT partitions for training and testing
cwt_sourceTrain <- cwt[data_split,]
cwt_sourceTest  <- cwt[-data_split,]

###Check balance of samples for the training and testing.
#Training
table(ref_sourceTrain$Life_form)
table(ref_sourceTest$Life_form)
#Testing
table(cwt_sourceTrain$Life_form)
table(cwt_sourceTest$Life_form)

###Export the samples 
train_ID <- traits_keep[data_split]
test_ID <- traits_keep[-data_split]
fwrite(train_ID, "traits_training.csv")
fwrite(test_ID, "traits_testing.csv")

###------------------------------Estimation of the optimal number of components--------------------------------------###
### The following steps are associated with the estimation of the optimal number 
#   of components of the section 2.4.1.

#Load the function script
source("optimal_number.R") 
#  Note this function run in parallel, please take a look to its arguments 
#  before to run the code. Please also note that this function returns several arguments, 
#  for our manuscript we focused our attention to the 'onesigma' method on the frame_comp list.

#Function to estimate the mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

###Apply function
#LMA optimal number of components------------------ 
opt_LMA <- optimal_number(model = logLMA ~ ., #Model
                          ref_train = ref_sourceTrain[, c(3, 7:ncol(ref))], #Traits and reflectance spectra
                          cwt_train = cwt_sourceTrain[, c(3, 7:ncol(cwt))], #Traits and wavelet spectra 
                          train_ID = train_ID, #ID of samples using for training
                          int_ncomp = 50, #Maximum number of components
                          iterations = 100, #Number of iterations
                          length.seg = 10, #Segment length for cross-validation
                          threads = 12) #Number of threads

#Add name of trait to the outputs
frame_LMA <- as.data.table(opt_LMA$frame)
frame_LMA$Trait <- "LMA"
RMSEP <- as.data.table(opt_LMA$RMSEP)
RMSEP$Trait <- "LMA"
PRESS <- as.data.table(opt_LMA$PRESS)
PRESS$Trait <- "LMA"

#Export outputs
fwrite(frame_LMA, "opt_LMA.csv")
fwrite(RMSEP, "RMSE_LMA.csv")
fwrite(PRESS, "PRESS_LMA.csv")

#Mode for the optimal number of components based on all the iterations
LMA_optimal <- frame_LMA[, mode(Optimal), by = Spectra]

#WC optimal number of components------------------ 
opt_WC <- optimal_number(model = logWC ~ .,
                          ref_train = ref_sourceTrain[, c(4, 7:ncol(ref))],
                          cwt_train = cwt_sourceTrain[, c(4, 7:ncol(cwt))], 
                          train_ID = train_ID, 
                          int_ncomp = 50, 
                          iterations = 100, 
                          length.seg = 10, 
                          threads = 12)

#Add name of trait to the outputs
frame_WC <- as.data.table(opt_WC$frame)
frame_WC$Trait <- "WC"
RMSEP <- as.data.table(opt_WC$RMSEP)
RMSEP$Trait <- "WC"
PRESS <- as.data.table(opt_WC$PRESS)
PRESS$Trait <- "WC"

#Export outputs
fwrite(frame_WC, "opt_WC.csv")
fwrite(RMSEP, "RMSE_WC.csv")
fwrite(PRESS, "PRESS_WC.csv")

#Mode for the optimal number of components based on all the iterations
WC_optimal <- frame_WC[, mode(Optimal), by = Spectra]

#EWT optimal number of components------------------  
opt_EWT <- optimal_number(model = logEWT ~ .,
                         ref_train = ref_sourceTrain[, c(5, 7:ncol(ref))],
                         cwt_train = cwt_sourceTrain[, c(5, 7:ncol(cwt))], 
                         train_ID = train_ID, 
                         int_ncomp = 50, 
                         iterations = 100, 
                         length.seg = 10, 
                         threads = 12)

#Add name of trait to the outputs
frame_EWT <- as.data.table(opt_EWT$frame)
frame_EWT$Trait <- "EWT"
RMSEP <- as.data.table(opt_EWT$RMSEP)
RMSEP$Trait <- "EWT"
PRESS <- as.data.table(opt_EWT$PRESS)
PRESS$Trait <- "EWT"

#Export outputs
fwrite(frame_EWT, "opt_EWT.csv")
fwrite(RMSEP, "RMSE_EWT.csv")
fwrite(PRESS, "PRESS_EWT.csv")

#Mode for the optimal number of components based on all the iterations
EWT_optimal <- frame_EWT[, mode(Optimal), by = Spectra]


###----------Evaluation of the variability of the predictor variables and the model performance--------------------------------------###
###The following steps are associated with the evaluation of the variability of 
#  the predictor variables and the model performance, sections 2.4.2 and 2.4.3.

#Load the function script
source("pls_evaluation.R") 
###Note this function run in parallel, please take a look to its arguments 
#  before to run the code. Please also note that this function returns several arguments.

###Function arguments
#On n_comp you can select the number of components to run.
#On resamples you can select the number of iterations to run.
#On split, you can select the number of data used to validation of the training model.
#This function needs the training and testing datasets with their IDs.
#The process of training and testing is performed simultaneously.

#Model LMA------------------  
results_LMA <- pls_evaluation(model = logLMA ~ ., 
                          ref = ref[, c(3, 7:ncol(ref))], 
                          cwt = cwt[, c(3, 7:ncol(cwt))], 
                          data_split = data_split, 
                          samples_ID = traits_keep, 
                          sec_split = 0.7, 
                          n_comp_ref = 29, 
                          n_comp_cwt = 16, 
                          iterations = 1000, 
                          threads = 12)

fwrite(results_LMA$Coefficients, "LMA_coefficients.csv")
fwrite(results_LMA$VIP, "LMA_VIP.csv")
fwrite(results_LMA$Performance, "LMA_performance.csv")
fwrite(results_LMA$Predict_training, "LMA_predict_training.csv")
fwrite(results_LMA$Predict_testing, "LMA_predict_testing.csv")
fwrite(results_LMA$Residuals_training, "LMA_residuals_training.csv")
fwrite(results_LMA$Residuals_testing, "LMA_residuals_testing.csv")

#Model of WV------------------  
results_WC <- pls_evaluation(model = logWC ~ ., 
                              ref = ref[, c(4, 7:ncol(ref))], 
                              cwt = cwt[, c(4, 7:ncol(cwt))], 
                              data_split = data_split, 
                              samples_ID = traits_keep, 
                              sec_split = 0.7, 
                              n_comp_ref = 28, 
                              n_comp_cwt = 13, 
                              iterations = 1000, 
                              threads = 12)

fwrite(results_WC$Coefficients, "WC_coefficients.csv")
fwrite(results_WC$VIP, "WC_VIP.csv")
fwrite(results_WC$Performance, "WC_performance.csv")
fwrite(results_WC$Predict_training, "WC_predict_training.csv")
fwrite(results_WC$Predict_testing, "WC_predict_testing.csv")
fwrite(results_WC$Residuals_training, "WC_residuals_training.csv")
fwrite(results_WC$Residuals_testing, "WC_residuals_testing.csv")

#Model for EWT------------------  
results_EWT <- pls_evaluation(model = logEWT ~ ., 
                              ref = ref[, c(5, 7:ncol(ref))], 
                              cwt = cwt[, c(5, 7:ncol(cwt))], 
                              data_split = data_split, 
                              samples_ID = traits_keep, 
                              sec_split = 0.7, 
                              n_comp_ref = 25, 
                              n_comp_cwt = 13, 
                              iterations = 1000, 
                              threads = 12)

fwrite(results_EWT$Coefficients, "EWT_coefficients.csv")
fwrite(results_EWT$VIP, "EWT_VIP.csv")
fwrite(results_EWT$Performance, "EWT_performance.csv")
fwrite(results_EWT$Predict_training, "EWT_predict_training.csv")
fwrite(results_EWT$Predict_testing, "EWT_predict_testing.csv")
fwrite(results_EWT$Residuals_training, "EWT_residuals_training.csv")
fwrite(results_EWT$Residuals_testing, "EWT_residuals_testing.csv")
#------------------------------End-------------------------#
#For visualization take a look to the figure scripts.






