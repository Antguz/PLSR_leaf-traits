library(doSNOW)

pls_evaluation <- function(model, ref, cwt, data_split = data_split, samples_ID, sec_split = 0.7, n_comp_ref = 30, n_comp_cwt = 19, iterations = 20, threads = 12) {
  
  ###Subdatasets for training and testing
  ref_training <- ref[data_split,]
  cwt_training <- cwt[data_split,]
  
  ref_testing <- ref[-data_split,]
  cwt_testing <- cwt[-data_split,]
  
  lf_training <- samples_ID[data_split,] 
  lf_testing <- samples_ID[-data_split,]
  
  ###Wavelength names
  wavelength <- colnames(ref)[-1]
  
  ###Progress bar -----------------------------------
  pb <- txtProgressBar(min = 1, max = iterations, style = 3) 
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  ###Parallel---------------------------------------
  cl <- makeCluster(threads)
  doSNOW::registerDoSNOW(cl)
  
  ###Model loop-------------------------------
 
  final <- foreach(i = 1:iterations, .packages= c('base', 'data.table', 'foreach', 'caret', 'pls', 'plsVarSel'), .export= c("performance"), .options.snow = opts) %dopar% {
    
    ###Equal sub_split for training based on Life forms-------------------------------------
    sub_split <- caret::createDataPartition(lf_training$Life_form, list = FALSE, p = sec_split, times = 1)
    
    #Calibration algorithms
    cal_ref <- ref_training[sub_split, ]
    cal_cwt <- cwt_training[sub_split, ]
    
    ### Build PLSR model with training data--------------------------------------------
    pls_ref <- plsr(model, 
                        ncomp = n_comp_ref, 
                        scale = FALSE, 
                        centre = TRUE, 
                        validation= "none", 
                        data= cal_ref, 
                        method = "oscorespls")
    
    pls_cwt <- plsr(model, 
                    ncomp = n_comp_cwt, 
                    scale = FALSE, 
                    centre = TRUE, 
                    validation= "none", 
                    data= cal_cwt, 
                    method = "oscorespls")
    
    ###Store the model arguments
    #Create frame
    name <- paste0("resample_", i)
    frame <- data.table(Spectra = c("Reflectance", "CWT"), iteration = name)
    
    performance_frame <- data.table(Spectra = rep(c(rep("Reflectance", 3), rep("CWT", 3)), 2),
                                    Model = c(rep("Training", 6), rep("Testing", 6)),
                                    Life_form = rep(c("All", "Tree", "Liana"), 4),
                                    iteration = name)
    
    #Save coefficients
    ref_coefs <- as.vector(coef(pls_ref, ncomp = n_comp_ref, intercept=TRUE))
    cwt_coefs <- as.vector(coef(pls_cwt, ncomp = n_comp_cwt, intercept=TRUE))
    coef <- rbind(ref_coefs, cwt_coefs)
    
    frame_coef <- cbind(frame, coef)
    colnames(frame_coef) <- c("Spectra", "iteration", "intercept", wavelength)
    
    #Save VIP
    ref_VIP <- as.vector(VIP(pls_ref, opt.comp = n_comp_ref))
    cwt_VIP <- as.vector(VIP(pls_cwt, opt.comp = n_comp_cwt))
    VIP <- rbind(ref_VIP, cwt_VIP)
    
    frame_VIP <- cbind(frame, VIP)
    colnames(frame_VIP) <- c("Spectra", "iteration", wavelength)
    
    ### Estimate variability in the model----------------------------------------------------------------------
    ##Predict values
    #Training model
    ref_training_predicted_all <- 10^as.vector(predict(pls_ref, newdata= ref_training, ncomp = n_comp_ref, type= "response")[,,1])
    ref_training_predicted_trees <- 10^as.vector(predict(pls_ref, newdata= ref_training[lf_training$Life_form == "Tree",], ncomp = n_comp_ref, type= "response")[,,1])
    ref_training_predicted_lianas <- 10^as.vector(predict(pls_ref, newdata= ref_training[lf_training$Life_form == "Liana",], ncomp = n_comp_ref, type= "response")[,,1])
      
    cwt_training_predicted_all <- 10^as.vector(predict(pls_cwt, newdata= cwt_training, ncomp = n_comp_cwt, type= "response")[,,1])
    cwt_training_predicted_trees <- 10^as.vector(predict(pls_cwt, newdata= cwt_training[lf_training$Life_form == "Tree",], ncomp = n_comp_cwt, type= "response")[,,1])
    cwt_training_predicted_lianas <- 10^as.vector(predict(pls_cwt, newdata= cwt_training[lf_training$Life_form == "Liana",], ncomp = n_comp_cwt, type= "response")[,,1])    
      
    #Testing model
    ref_testing_predicted_all <- 10^as.vector(predict(pls_ref, newdata= ref_testing, ncomp = n_comp_ref, type= "response")[,,1])
    ref_testing_predicted_trees <- 10^as.vector(predict(pls_ref, newdata= ref_testing[lf_testing$Life_form == "Tree",], ncomp = n_comp_ref, type= "response")[,,1])
    ref_testing_predicted_lianas <- 10^as.vector(predict(pls_ref, newdata= ref_testing[lf_testing$Life_form == "Liana",], ncomp = n_comp_ref, type= "response")[,,1])
    
    cwt_testing_predicted_all <- 10^as.vector(predict(pls_cwt, newdata= cwt_testing, ncomp = n_comp_cwt, type= "response")[,,1])
    cwt_testing_predicted_trees <- 10^as.vector(predict(pls_cwt, newdata= cwt_testing[lf_testing$Life_form == "Tree",], ncomp = n_comp_cwt, type= "response")[,,1])
    cwt_testing_predicted_lianas <- 10^as.vector(predict(pls_cwt, newdata= cwt_testing[lf_testing$Life_form == "Liana",], ncomp = n_comp_cwt, type= "response")[,,1])    
    
    ###Evaluate performance
    #Training
    ref_training_performance_all <- performance((10^ref_training[[1]]), ref_training_predicted_all)
    ref_training_performance_trees <- performance((10^ref_training[lf_training$Life_form == "Tree",][[1]]), ref_training_predicted_trees)
    ref_training_performance_lianas <- performance((10^ref_training[lf_training$Life_form == "Liana",][[1]]), ref_training_predicted_lianas)
    
    cwt_training_performance_all <- performance((10^cwt_training[[1]]), cwt_training_predicted_all)
    cwt_training_performance_trees <- performance((10^cwt_training[lf_training$Life_form == "Tree",][[1]]), cwt_training_predicted_trees)
    cwt_training_performance_lianas <- performance((10^cwt_training[lf_training$Life_form == "Liana",][[1]]), cwt_training_predicted_lianas)
    
    #Testing model
    ref_testing_performance_all <- performance((10^ref_testing[[1]]), ref_testing_predicted_all)
    ref_testing_performance_trees <- performance((10^ref_testing[lf_testing$Life_form == "Tree",][[1]]), ref_testing_predicted_trees)
    ref_testing_performance_lianas <- performance((10^ref_testing[lf_testing$Life_form == "Liana",][[1]]), ref_testing_predicted_lianas)
    
    cwt_testing_performance_all <- performance((10^cwt_testing[[1]]), cwt_testing_predicted_all)
    cwt_testing_performance_trees <- performance((10^cwt_testing[lf_testing$Life_form == "Tree",][[1]]), cwt_testing_predicted_trees)
    cwt_testing_performance_lianas <- performance((10^cwt_testing[lf_testing$Life_form == "Liana",][[1]]), cwt_testing_predicted_lianas)
    
    ###Collect values of performance
    performance_merge <- rbind(ref_training_performance_all, ref_training_performance_trees, ref_training_performance_lianas,
                         cwt_training_performance_all, cwt_training_performance_trees, cwt_training_performance_lianas,
                         ref_testing_performance_all, ref_testing_performance_trees, ref_testing_performance_lianas,
                         cwt_testing_performance_all, cwt_testing_performance_trees, cwt_testing_performance_lianas)
    
    performance_frame <- cbind(performance_frame, performance_merge)
    
    ###Collect predicted values
    training_predict <- cbind(frame, rbind(ref_training_predicted_all, cwt_training_predicted_all))
    colnames(training_predict) <- c("Spectra", "iteration", paste0("Sample_", lf_training$Sample))
    
    testing_predict <- cbind(frame, rbind(ref_testing_predicted_all, cwt_testing_predicted_all))
    colnames(testing_predict) <- c("Spectra", "iteration", paste0("Sample_", lf_testing$Sample))
    
    ###Estimated and collect residuals
    ref_training_residuals <- ref_training_predicted_all - (10^ref_training[[1]])
    cwt_training_residuals <- cwt_training_predicted_all - (10^cwt_training[[1]])
    
    ref_testing_residuals <- ref_testing_predicted_all - (10^ref_testing[[1]])
    cwt_testing_residuals <- cwt_testing_predicted_all - (10^ref_testing[[1]])
    
    training_residuals <- cbind(frame, rbind(ref_training_residuals, cwt_training_residuals))
    colnames(training_residuals) <- c("Spectra", "iteration", paste0("Sample_", lf_training$Sample))
    
    testing_residuals <- cbind(frame, rbind(ref_testing_residuals, cwt_testing_residuals))
    colnames(testing_residuals) <- c("Spectra", "iteration", paste0("Sample_", lf_testing$Sample))
    
    ###Return list--------------------------------------------------------------------------
    
    return(list(Coefficients = frame_coef, 
                VIP = frame_VIP, 
                Performance = performance_frame, 
                Predict_training = training_predict,
                Predict_testing = testing_predict, 
                Residuals_training = training_residuals,
                Residuals_testing = testing_residuals))
    }
  
  close(pb)
  stopCluster(cl)
  
  new_final <- combine_custom(final)
  
  return(new_final)
  
}

###Combine-----------------------------------

combine_custom <- function(final) {
  n <- length(final)
  coe <- NULL
  vip <- NULL
  stats <- NULL
  train_pred <- NULL
  test_pred <- NULL
  train_resid <- NULL
  test_resid <- NULL
  
  for (i in seq(n)) {
    coe <- rbind(coe, final[[i]]$Coefficients)
    vip <- rbind(vip, final[[i]]$VIP)
    stats <- rbind(stats, final[[i]]$Performance)
    train_pred <- rbind(train_pred, final[[i]]$Predict_training)
    test_pred <- rbind(test_pred, final[[i]]$Predict_testing)
    train_resid <- rbind(train_resid, final[[i]]$Residuals_training)
    test_resid <- rbind(test_resid, final[[i]]$Residuals_testing)
  }

    return(list(Coefficients = coe, 
                VIP = vip, 
                Performance = stats, 
                Predict_training = train_pred,
                Predict_testing = test_pred, 
                Residuals_training = train_resid,
                Residuals_testing = test_resid))
}

###Model performance-----------------------------------

performance <- function(observed, predicted) {
  
  obser <- observed
  pred <- predicted
  
  rss <- sum((obser - pred) ^ 2)  ## residual sum of squares
  tss <- sum((obser - mean(obser))^2)  ## total sum of squares
  
  n <- length(predicted)
  Rsq <- 1 - (rss/tss)
  Bias <- mean(((obser - pred)/obser))
  RMSE <- sqrt(mean((obser - pred)^2))
  RMSE_P <- (RMSE/(max(obser)-min(obser)))*100
  
  return(round(c(N = n, Rsq = Rsq, Bias = Bias, RMSE = RMSE, RMSE_P = RMSE_P), 6))

}

