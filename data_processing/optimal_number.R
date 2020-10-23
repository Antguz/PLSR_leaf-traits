###This function run the estimation of the optimal number of components 
optimal_number <- function(train, model, int_ncomp = 65, iterations = 100, k = 10, threads = 4) {
  #Frame
  frame <- data.frame(Iteration = c(1:iterations), Abs_min = rep(NA, iterations), 
                      Permutation = rep(NA, iterations), Onesigma = rep(NA, iterations))
  
  RMSEP_out <- array(data=NA,dim=c(int_ncomp + 1, iterations))
  PRESS <- array(data=NA,dim=c(int_ncomp, iterations))
  
  cl <- makeCluster(threads, type = "PSOCK")
  pls.options(parallel = cl)
  
  pb <- txtProgressBar(min = 1, max = iterations, style = 3) 
  
  #Loop
  for(i in 1:iterations) {
    
    setTxtProgressBar(pb, i)
    
    samples <- nrow(train)
    k <- round(samples/10)
    CVseg <- cvsegments(samples, k = k, type="random")
    
    #Creation of the model
    pls_components <- plsr(model, 
                           data= train, 
                           scale = FALSE,
                           center = TRUE,
                           ncomp= int_ncomp,
                           validation = "CV", 
                           segments = CVseg,
                           trace= FALSE, 
                           method = "oscorespls")
    
    ###Selection of n components
    #Min absolute
    n_comp_min <- which.min(as.vector(pls_components$validation$PRESS))
    frame[i, 2] <- n_comp_min
    
    #Permutation
    n_comp_permutation <- selectNcomp(pls_components, "randomization", nperm = 10)
    frame[i, 3] <- n_comp_permutation
    
    #Onesigma
    n_com_sd <- selectNcomp(pls_components, "onesigma")
    frame[i, 4] <- n_com_sd
    
    vec <- as.vector(RMSEP(pls_components)$val[1,,])
    RMSEP_out[,i] <- vec
    
    PRESS[,i] <- as.vector(pls_components$validation$PRESS)
    
  }
  
  stopCluster(cl)
  
  return(list(frame_comp = frame, RMSEP_out = RMSEP_out, PRESS = PRESS))
}
