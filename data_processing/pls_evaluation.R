library(doSNOW)

pls_evaluation <- function(train, train_ID, test, test_ID, model, n_comp = 30, resamples = 1000, split = 0.7, threads = 4) {
  
  ###Parallel---------------------------------------
  cl <- makeCluster(threads)
  registerDoSNOW(cl)
  
  ###Progress bar -----------------------------------
  pb <- txtProgressBar(min = 1, max = resamples, style = 3) 
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  ###Model loop-------------------------------
 
  final <- foreach(i = 1:resamples, .packages= c('base', 'data.table', 'foreach', 'caret', 'pls', 'plsVarSel'), .export= c("performance"), .options.snow = opts) %dopar% {
    
    ###Equal split for traing based on Life forms-------------------------------------
    data_split <- createDataPartition(train_ID$Life_form, list = FALSE, p = split, times = 1)
    
    cal_data <- train[data_split, ]
    
    val_lianas <- train[train_ID$Life_form == "Liana", ]
    val_trees <- train[train_ID$Life_form == "Tree", ]
    
    ### Build PLSR model with training data--------------------------------------------
    pls_results <- plsr(model, 
                        ncomp = n_comp, 
                        scale = FALSE, 
                        centre = TRUE, 
                        validation= "none", 
                        data= cal_data, 
                        method = "oscorespls")
    
    ###Store the model arguments
    name <- paste("", "resample_", i, sep = "")
    
    frame_coefs <- data.table(name = as.vector(coef(pls_results, ncomp = n_comp, intercept=TRUE)))
    colnames(frame_coefs) <- name
    
    frame_VIP <- data.table(name = as.vector(VIP(pls_results, n_comp)))
    colnames(frame_VIP) <- name
                             
    ### Estimate variability in the model----------------------------------------------------------------------
    pred_val <- as.vector(predict(pls_results, 
                                       newdata= train,
                                       ncomp = n_comp,
                                       type= "response")[,,1])
    pred_val <- 10^pred_val
    
    pred_val_lianas <- as.vector(predict(pls_results, 
                                       newdata= val_lianas,
                                       ncomp = n_comp,
                                       type= "response")[,,1])
    pred_val_lianas <- 10^pred_val_lianas
    
    pred_val_trees <- as.vector(predict(pls_results, 
                                       newdata= val_trees,
                                       ncomp = n_comp,
                                       type= "response")[,,1])
    pred_val_trees <- 10^pred_val_trees
    
    ###Frame to fill
    
    frame_stats <- data.frame(Resample = rep(name, 6),
                              Process = rep(NA, 6),
                              Life_form = rep(NA, 6),
                              Samples = rep(NA, 6),
                              Rsq = rep(NA, 6),
                              Bias= rep(NA, 6),
                              RMSE = rep(NA, 6),
                              RMSE_P = rep(NA, 6))
    
    ###All---------------------------
    # Error statistics
    trait_val <- 10^train[, 1]

    val_perf <- performance(trait_val, pred_val)

    ### Store results of iteration i
    frame_stats[1,2] <- "Training"
    frame_stats[1,3] <- "All"
    frame_stats[1,4] <- val_perf[1]
    frame_stats[1,5] <- val_perf[2]
    frame_stats[1,6] <- val_perf[3]
    frame_stats[1,7] <- val_perf[4]
    frame_stats[1,8] <- val_perf[5]
    
    ###Lianas------------------------
    # Error statistics
    trait_val_lianas <- 10^val_lianas[, 1]
    val_perf_lianas <- performance(trait_val_lianas, pred_val_lianas)

    ### Store results of iteration i
    frame_stats[2,2] <- "Training"
    frame_stats[2,3] <- "Lianas"
    frame_stats[2,4] <- val_perf_lianas[1]
    frame_stats[2,5] <- val_perf_lianas[2]
    frame_stats[2,6] <- val_perf_lianas[3]
    frame_stats[2,7] <- val_perf_lianas[4]
    frame_stats[2,8] <- val_perf_lianas[5]
    
    ###Trees-------------------------
    # Error statistics
    trait_val_trees <- 10^val_trees[, 1]
    val_perf_trees <- performance(trait_val_trees, pred_val_trees)

    ### Store results of iteration i
    frame_stats[3,2] <- "Training"
    frame_stats[3,3] <- "Trees"
    frame_stats[3,4] <- val_perf_trees[1]
    frame_stats[3,5] <- val_perf_trees[2]
    frame_stats[3,6] <- val_perf_trees[3]
    frame_stats[3,7] <- val_perf_trees[4]
    frame_stats[3,8] <- val_perf_trees[5]
    
    ### Predict traits train----------------------------------------------------------------------
    frame_predict_train <- data.table(name = (pred_val))
    colnames(frame_predict_train) <- name
    
    ### Residuals train
    residuals_train <- round((trait_val) - (pred_val), 5)
    residuals_train <- data.table(residuals_train = residuals_train)
    colnames(residuals_train) <- name
    
    ### Estimate variability in the testing model-----------------------------------------------------------------
    
    is_liana <- test_ID$Life_form == "Liana"
    
    test_lianas <- test[is_liana,]
    test_trees <- test[!is_liana,]
    
    pred_test <- as.vector(predict(pls_results, 
                                       newdata= test,
                                       ncomp = n_comp,
                                       type= "response")[,,1])
    pred_test <- 10^pred_test
    
    pred_test_lianas <- as.vector(predict(pls_results, 
                                         newdata= test_lianas,
                                         ncomp = n_comp,
                                         type= "response")[,,1])
    pred_test_lianas <- 10^pred_test_lianas
    
    pred_test_trees <- as.vector(predict(pls_results, 
                                        newdata= test_trees,
                                        ncomp = n_comp,
                                        type= "response")[,,1])
    pred_test_trees <- 10^pred_test_trees
    
    ###All---------------------------
    # Error statistics
    trait_test <- 10^test[, 1]
    test_perf <- performance(trait_test, pred_test)

    ### Store results of iteration i
    frame_stats[4,2] <- "Testing"
    frame_stats[4,3] <- "All"
    frame_stats[4,4] <- test_perf[1]
    frame_stats[4,5] <- test_perf[2]
    frame_stats[4,6] <- test_perf[3]
    frame_stats[4,7] <- test_perf[4]
    frame_stats[4,8] <- test_perf[5]
    
    ###Lianas------------------------
    # Error statistics
    trait_test_lianas <- 10^test_lianas[, 1]
    test_perf_lianas <- performance(trait_test_lianas, pred_test_lianas)
    
    ### Store results of iteration i
    frame_stats[5,2] <- "Testing"
    frame_stats[5,3] <- "Lianas"
    frame_stats[5,4] <- test_perf_lianas[1]
    frame_stats[5,5] <- test_perf_lianas[2]
    frame_stats[5,6] <- test_perf_lianas[3]
    frame_stats[5,7] <- test_perf_lianas[4]
    frame_stats[5,8] <- test_perf_lianas[5]
    
    ###Trees-------------------------
    # Error statistics
    trait_test_trees <- 10^test_trees[, 1]
    test_perf_trees <- performance(trait_test_trees, pred_test_trees)
    
    ### Store results of iteration i
    frame_stats[6,2] <- "Testing"
    frame_stats[6,3] <- "Trees"
    frame_stats[6,4] <- test_perf_trees[1]
    frame_stats[6,5] <- test_perf_trees[2]
    frame_stats[6,6] <- test_perf_trees[3]
    frame_stats[6,7] <- test_perf_trees[4]
    frame_stats[6,8] <- test_perf_trees[5]
    
    ### Predict traits----------------------------------------------------------------------
    
    frame_predict_test <- data.table(name = pred_test)
    colnames(frame_predict_test) <- name
    
    ### Residuals test
    residuals_test <- round(trait_test - pred_test, 5)
    residuals_test <- data.table(residuals_test = residuals_test)
    colnames(residuals_test) <- name
    
    ###Return list--------------------------------------------------------------------------
    
    return(list(Coefficients = frame_coefs, 
                VIP = frame_VIP, 
                Stats = frame_stats, 
                Predict_train = frame_predict_train,
                Predict_test = frame_predict_test, 
                Residuals_train = residuals_train,
                Residuals_test = residuals_test))
    
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
    coe <- cbind(coe, final[[i]]$Coefficients)
    vip <- cbind(vip, final[[i]]$VIP)
    stats <- rbind(stats, final[[i]]$Stats)
    train_pred <- cbind(train_pred, final[[i]]$Predict_train)
    test_pred <- cbind(test_pred, final[[i]]$Predict_test)
    train_resid <- cbind(train_resid, final[[i]]$Residuals_train)
    test_resid <- cbind(test_resid, final[[i]]$Residuals_test)
    
  
  }

    return(list(Coefficients = coe, 
                VIP = vip, 
                Stats = stats, 
                Predict_train = train_pred,
                Predict_test = test_pred, 
                Residuals_train = train_resid,
                Residuals_test = test_resid))
}



###Model performance-----------------------------------

performance <- function(observed, predicted) {
  
  obser <- observed
  pred <- predicted
  
  rss <- sum((pred - obser) ^ 2)  ## residual sum of squares
  tss <- sum((obser - mean(obser))^2)  ## total sum of squares
  
  n <- length(predicted)
  Rsq <- 1 - (rss/tss)
  Bias <- mean(((pred - obser)/obser))
  RMSE <- sqrt(mean((obser - pred)^2))
  RMSE_P <- (RMSE/(max(obser)-min(obser)))*100
  
  return(round(c(N = n, Rsq = Rsq, Bias = Bias, RMSE = RMSE, RMSE_P = RMSE_P), 6))

}
