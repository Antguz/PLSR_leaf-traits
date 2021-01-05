###This function run the estimation of the optimal number of components 
optimal_number <- function(model, ref_train, cwt_train, train_ID, int_ncomp = 50, iterations = 100, length.seg = 10, threads = 12) {
  
  #Frame of performance
  frame <- data.frame(Spectra = NA, 
                      Iteration = NA, 
                      Abs_min = NA, 
                      Optimal = NA)
  
  frame <- frame[0,]
  final <- frame
  
  #RMSE and RMSEP
  fRMSEP <- matrix(NA, ncol = (int_ncomp + 3), nrow = 1)
  names <- c("Spectra", "Iteration", "Intercept", paste0("Comp_", 1:int_ncomp))
  fRMSEP <- as.data.frame(fRMSEP)
  colnames(fRMSEP) <- names
  fRMSEP <- fRMSEP[0,]
  final_fRMSEP <- fRMSEP
  
  fPRESS <- matrix(NA, ncol = (int_ncomp + 2), nrow = 1)
  names <- c("Spectra", "Iteration", paste0("Comp_", 1:int_ncomp))
  fPRESS <- as.data.frame(fPRESS)
  colnames(fPRESS) <- names
  fPRESS <- fPRESS[0,]
  final_fPRESS <- fPRESS
  
  #Progress bar
  pb <- txtProgressBar(min = 1, max = iterations, style = 3) 
  
  #Parallel 
  cl <- makeCluster(threads, type = "PSOCK")
  pls.options(parallel = cl)
  
  #Loop
  for(i in 1:iterations) {
    
    setTxtProgressBar(pb, i)
    
    ###Frames copy
    frame_iteration <- frame
    fRMSEP_iteration <- fRMSEP
    fPRESS_iteration <- fPRESS
    
    ###Cross validation
    CVseg <- CVSeg_lifeforms(train_ID$Life_form,  length.seg =  length.seg)
    
    ###---------------------------------Reflectance
    #Creation of the model
    pls_components_ref <- plsr(model, 
                           data= ref_train, 
                           scale = FALSE,
                           center = TRUE,
                           ncomp= int_ncomp,
                           validation = "CV", 
                           segments = CVseg,
                           trace= FALSE, 
                           method = "oscorespls")
    
    #Spectra
    frame_iteration[1, 1] <- "Reflectance"
    
    #Iteration
    frame_iteration[1, 2] <- i
    
    ###Selection of n components
    #Min absolute
    frame_iteration[1, 3] <- which.min(as.vector(pls_components_ref$validation$PRESS))
    
    #Onesigma
    frame_iteration[1, 4] <- selectNcomp(pls_components_ref, "onesigma")
    
    ###RMSEP
    vec <- as.vector(RMSEP(pls_components_ref)$val[1,,])
    
    fRMSEP_iteration[1, 1] <- "Reflectance"
    fRMSEP_iteration[1, 2] <- i
    fRMSEP_iteration[1, 3:(int_ncomp + 3)] <- vec
    
    ###PRESS
    vec <- as.vector(pls_components_ref$validation$PRESS)
    
    fPRESS_iteration[1, 1] <- "Reflectance"
    fPRESS_iteration[1, 2] <- i
    fPRESS_iteration[1, 3:(int_ncomp + 2)] <- vec
    
    ###---------------------------------CWT
    #Creation of the model
    pls_components_cwt <- plsr(model, 
                               data= cwt_train, 
                               scale = FALSE,
                               center = TRUE,
                               ncomp= int_ncomp,
                               validation = "CV", 
                               segments = CVseg,
                               trace= FALSE, 
                               method = "oscorespls")
    
    #Spectra
    frame_iteration[2, 1] <- "CWT"
    
    #Iteration
    frame_iteration[2, 2] <- i
    
    ###Selection of n components
    #Min absolute
    frame_iteration[2, 3] <- which.min(as.vector(pls_components_cwt$validation$PRESS))
    
    #Onesigma
    frame_iteration[2, 4] <- selectNcomp(pls_components_cwt, "onesigma")
    
    ###RMSEP
    vec <- as.vector(RMSEP(pls_components_cwt)$val[1,,])
    
    fRMSEP_iteration[2, 1] <- "CWT"
    fRMSEP_iteration[2, 2] <- i
    fRMSEP_iteration[2, 3:(int_ncomp + 3)] <- vec
    
    ###PRESS
    vec <- as.vector(pls_components_ref$validation$PRESS)
    
    fPRESS_iteration[2, 1] <- "CWT"
    fPRESS_iteration[2, 2] <- i
    fPRESS_iteration[2, 3:(int_ncomp + 2)] <- vec
    
    ###---------------------------------Save results
    
    final <- rbind(final, frame_iteration)
    final_fRMSEP <- rbind(final_fRMSEP,fRMSEP_iteration)
    final_fPRESS <- rbind(final_fPRESS, fPRESS_iteration)
    
  }
  
  stopCluster(cl)
  
  return(list(frame = final, RMSEP = final_fRMSEP, PRESS = final_fPRESS))
}


####------Generate segments for 10-fold cross-validation using equal removal of lianas and trees------

CVSeg_lifeforms <- function(life_forms, length.seg = 10) {
  
  frame <- data.table(Life_forms = life_forms, sample = 1:length(life_forms))
  
  lianas <- frame[Life_forms == "Liana",]
  trees <- frame[Life_forms == "Tree",]
  
  samples_lianas <- nrow(lianas)
  samples_trees <- nrow(trees)
  
  segments <- length.seg/2
  
  k <- ceiling((samples_lianas+samples_trees)/2 / segments)
  
  incomplete <- k * segments - (samples_lianas+samples_trees)/2
  complete   <- k - incomplete
  
  inds_lianas <- matrix(c(sample(lianas$sample), rep(NA, incomplete)), nrow = segments, byrow = TRUE)
  inds_trees <- matrix(c(sample(trees$sample), rep(NA, incomplete)), nrow = segments, byrow = TRUE)
  
  inds <- rbind(inds_lianas, inds_trees)
  
  res <- lapply(as.data.frame(inds), function(x) c(na.omit(x)))
  attr(res, "incomplete") <- incomplete
  attr(res, "random")
  res
}

