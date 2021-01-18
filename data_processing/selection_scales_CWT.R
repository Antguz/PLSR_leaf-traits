##############################################################################
############ Potential number of scale combinations for the CWT
##############################################################################

###This scrip represent the main data processing and analysis for the manuscript 
#'Prediction of leaf traits of lianas and trees via the integration of wavelet spectra 
# in the visible-near infrared and thermal infrared domains' by Guzman and Sanchez-Azofeifa. 
# The data for this manuscript is available at https://doi.org/10.7910/DVN/YBYO7W. Please cite this
# manuscript if the following code helps in your research.

###Load libraries
library(gtools)
library(abind)
library(wmtsa)
library(data.table)
library(AppliedPredictiveModeling)
library(pls)
library(caret)
library(doSNOW)
library(plsVarSel)

###-----------------------------------------Number of scales-------------------------------------------#####
###Function to estimate the potential number of scale combinations

#Scales is an integer vector describing the number of scales to estimate

#Number of scales
scales <- 1:9

combinations <- function(scales) {
  
  final <- data.table(min(scales):length(scales)) ###Create frame of base
  
  for(i in 2:max(scales)) { ###Estimate combinations
    
    comb <- as.data.frame(permutations(max(scales), i, scales, set = TRUE, repeats.allowed = FALSE))
    comb <- as.data.table(t(apply(comb, 1, function(x) sort(x, decreasing = FALSE))))
    comb <- unique(comb[,]) ##Unique combination
    final <- rbind(final, comb, fill = TRUE) ##Merge observations
  }
  return(final)
}

CWT_scales <- combinations(scales)
CWT_scales <- CWT_scales[1:381,] #Max of 5 combinations

###-----------------------------------------Number of scales-------------------------------------------#####
###Function to estimate the scales of CTW and storage it as a data cube

#data_vis is the leaf reflectance in the VIS-NIR
#data_lwair is the leaf reflectance in the MLWIR
#range_vis and range_lwir are the wavelength range to use
#combinations is a matrix of the combinations of CWT scales estimated above.

#Load spectra
data_vis <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/02-data_vis_preprocessed.csv")
data_lwir <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/02-data_lwir_preprocessed.csv")

range_vis <- c(0.450, 1.0)
range_lwir <- c(2.55, 11)

cwt_cube <- function(data_vis, data_lwir, range_vis, range_lwir, by_vis = 0.0033, by_lwir = 0.0033, combinations) {
  
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
  
  complete <- array(NA, dim=c(total_rows, 1, ncol(combinations))) #Create an empty array
  
  for(i in 1:n_samples) {
    value_vis <- data_vis[[(1+i)]] ###Transform the vis spectrum 
    value_vis <- ts(value_vis, start = min(wavlength_vis), deltat = by_vis)
    cwt_value_vis <- wavCWT(value_vis, n.scale= 13, scale.range= c(0.0033, 0.0297), variance= 1, wavelet= "gaussian2") 
    ###This scale range need to be corrected if the user wants other scales
    cwt_value_vis <- as.matrix(cwt_value_vis)
    cwt_value_vis <- cwt_value_vis[vis_keep,]
    
    value_lwir <- data_lwir[[(1+i)]] ###Transform the lwir spectrum
    value_lwir <- ts(value_lwir, start = min(wavlength_lwir), deltat = by_lwir)
    cwt_value_lwir <- wavCWT(value_lwir, n.scale= 13, scale.range= c(0.0033,  0.0297), variance= 1, wavelet= "gaussian2")
    ###This scale range need to be corrected if the user wants other scales
    cwt_value_lwir <- as.matrix(cwt_value_lwir)
    cwt_value_lwir <- cwt_value_lwir[lwir_keep,]
    
    combine <- rbind(cwt_value_vis, cwt_value_lwir) ###Combine results
    
    to_merge <- array(as.numeric(unlist(combine)), dim=c(nrow(combine), 1, ncol(combinations)))
    
    complete <- abind(complete, to_merge, along = 2)
  }
  
  complete <- complete[,-1,]
  rownames(complete) <- wavelength_names
  
  return(complete)
}

CWT_spectra <- cwt_cube(data_vis, data_lwir, range_vis, range_lwir, combinations = CWT_scales)

###-----------------------------------------Summed_wavelet -------------------------------------------#####
###This is a complementary function to do the summed wavelet transformation

#CWT_spectra is the data-cube of wavelet transformation estimated by 'cwt_cube'
#scales_vector is an integer vector describing the number of scales to sum

summed_wavelet <- function(CWT_spectra, scales_vector) {
  
  wavelength_names <- rownames(CWT_spectra)
  to_sum <- CWT_spectra[,,scales_vector] #Subset scales
  sum_wavelet <- rowSums(to_sum, dims = 2, na.rm = TRUE) #Create the sum
  sum_wavelet <- as.data.table(t(sum_wavelet)) #Transpose the sum for the analysis
  return(sum_wavelet)
}


###------------------------------PLSR models based on wavelet combinations---------------------------#####
##This function run the PLSR models for a given trait and spectra using the wavelet combinations created above

#Load the file of leaf traits
ID <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/01-traits.csv")

#Log transform the leaf traits
logLMA <- data.table(logLMA = log10(ID$LMA))
logWC <- data.table(logWC =log10(ID$WC))
logEWT <- data.table(logEWT = log10(ID$EWT))

#Data set to use
ID_keep <- ID[, c(1, 5, 9, 10, 11)] #Subset dataset to Life_form, ID_collection, and leaf traits
ID_keep$Sample <- 1:700 #Create a column of sample ID

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

###Life forms used for training
life_form_ID <- ID_keep$Life_form[data_split]

###Function----------------------------------------------------------------------------------------

PLSR_wavelet_combinations <- function(model, trait = logLMA, data_split, train_ID_lf = life_form_ID, combinations = CWT_scales, cwt_cube = CWT_spectra, int_ncomp = 50, iterations = 5, length.seg = 10, threads = 12, name_trait) {
  
  trait_train <- trait[data_split]
  trait_test <- trait[-data_split]
  
  #Frame to export
  frame <- data.frame(Scales = NA, 
                      Iteration = NA,
                      Onesigma_comp = NA,
                      training_Rsq = NA, 
                      training_Bias= NA, 
                      training_RMSE = NA,
                      training_RMSE_P = NA,
                      testing_Rsq = NA, 
                      testing_Bias= NA, 
                      testing_RMSE = NA,
                      testing_RMSE_P = NA)
  
  frame <- frame[0,]
  final <- frame
  
  ###Summed-wavelet names
  combinations_names <- with(combinations, paste(V1, V2, V3, V4, V5, V6, V7, V8, V9, sep = ", "))
  
  ###Progress bar
  pb <- txtProgressBar(min = 1, max = (nrow(combinations)*iterations), style = 3) 
  n <- 1
  
  #Loop
  for(i in 1:iterations) {
    
    ##Make cluster
    cl <- parallel::makeCluster(threads, type = "PSOCK")
    pls.options(parallel = cl)
    
    #Frame to complete
    iteration_frame <- frame
    
    #Cross validation
    CVseg <- CVSeg_lifeforms(train_ID_lf, length.seg = length.seg)
    
    #Loop combination
    for(j in 1:nrow(combinations)) {
      
      #Create the summed-wavelet
      spectra <- summed_wavelet(cwt_cube, as.numeric(combinations[j,]))
      
      #data split
      training <- cbind(trait_train, spectra[data_split,])
      testing <- cbind(trait_test, spectra[-data_split,])
      
      ###Progress
      setTxtProgressBar(pb, n)
      n <- n + 1
      
      #Creation of the model
      pls_components <- plsr(model, 
                             data= training, 
                             scale = FALSE,
                             center = TRUE,
                             ncomp= int_ncomp,
                             validation = "CV", 
                             segments = CVseg,
                             trace= FALSE, 
                             method = "oscorespls")
      
      #Onesigma
      n_comp <- selectNcomp(pls_components, "onesigma")
      
      ### Estimate variability in the training model----------------------------------------------------------------------
      pred_training <- as.vector(predict(pls_components, 
                                         newdata= training,
                                         ncomp = n_comp,
                                         type= "response")[,,1])
      
      pred_training <- 10^pred_training
      
      trait_training <- 10^trait_train[[1]]
      
      training_perf <- performance(trait_training, pred_training)
      
      ### Estimate variability in the testing model----------------------------------------------------------------------
      
      pred_testing <- as.vector(predict(pls_components, 
                                        newdata= testing,
                                        ncomp = n_comp,
                                        type= "response")[,,1])
      
      pred_testing <- 10^pred_testing
      
      trait_testing <- 10^trait_test[[1]]
      
      testing_perf <- performance(trait_testing, pred_testing)
      
      ###Fill frame-------------------------------------------------------------------------------------------------------
      
      iteration_frame[j, 1] <- combinations_names[j] #Scale combination
      iteration_frame[j, 2] <- i #Iteration
      iteration_frame[j, 3] <- n_comp #Components
      iteration_frame[j, 4] <- training_perf[2]
      iteration_frame[j, 5] <- training_perf[3]
      iteration_frame[j, 6] <- training_perf[4]
      iteration_frame[j, 7] <- training_perf[5]
      iteration_frame[j, 8] <- testing_perf[2]
      iteration_frame[j, 9] <- testing_perf[3]
      iteration_frame[j, 10] <- testing_perf[4]
      iteration_frame[j, 11] <- testing_perf[5]
      
    }
    
    final <- rbind(final, iteration_frame)
    fwrite(iteration_frame, paste("", "iteration_", i, ".csv", sep = ""))
    
    ###Stop cluster
    stopCluster(cl)
    gc()
    
  }
  
  fwrite(final, paste("", name_trait, "_scales_complete.csv", sep = ""))
  return(final)
}


results <- PLSR_wavelet_combinations(model = logLMA ~ ., 
                                     trait = logLMA, 
                                     data_split = data_split, 
                                     train_ID_lf = life_form_ID,
                                     combinations = CWT_scales, 
                                     cwt_cube = CWT_spectra, 
                                     int_ncomp = 50, 
                                     iterations = 2, 
                                     length.seg = 10, 
                                     threads = 12,
                                     name_trait = "LMA")

results <- PLSR_wavelet_combinations(model = logWC ~ ., 
                                     trait = logWC, 
                                     data_split = data_split, 
                                     train_ID_lf = life_form_ID,
                                     combinations = CWT_scales, 
                                     cwt_cube = CWT_spectra, 
                                     int_ncomp = 50, 
                                     iterations = 1, 
                                     length.seg = 10, 
                                     threads = 12,
                                     name_trait = "WC")

results <- PLSR_wavelet_combinations(model = logEWT ~ ., 
                                     trait = logEWT, 
                                     data_split = data_split, 
                                     train_ID_lf = life_form_ID,
                                     combinations = CWT_scales, 
                                     cwt_cube = CWT_spectra, 
                                     int_ncomp = 50, 
                                     iterations = 1, 
                                     length.seg = 10, 
                                     threads = 12,
                                     name_trait = "EWT")

