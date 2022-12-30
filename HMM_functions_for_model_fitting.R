library(Rcpp)
library(RcppArmadillo)

sourceCpp("C:/Users/FastRun/OneDrive - University of St Andrews/Shared/Hidden Markov Models for Temnothorax Nest Building/Code VP/rcpparmatest.cpp")
########################################################################
# These functions assume a 2-state HMM with multivariate gamma state dependent distributions
# for the dependent variable

# The functions are constructed so that a model with common parameters across the colonies
# and a full model with different parameters can be fitted to the data.
# In addition, a model with 1 or 2 covariates can be fitted.

pn2pw <- function(int, xl, gsd, d, gamma, diff=FALSE) {
                # int are the 4 x 2 intercepts
                # xl is a list matrices related to covariates
                # diff is whether the slope, delta and gamma parameters are treated differently across the colonies (TRUE) or not (FALSE)
                # gamma is a list
                # int, x1, x2, gsd and d are matrices
               
          if (diff==FALSE){
            gamma <- gamma[[1]]
            # Transforming the t.p.m.
            workingGamma  <- NULL
            tempGamma     <- gamma/diag(gamma) # result in R+
            tempGamma     <- log(tempGamma)    # result in all R
            workingGamma  <- as.vector(tempGamma[!diag(2)]) 
            
            xv            <- NULL
            for(j in 1:length(xl)) xv<-c(xv, xl[[j]][1,])
            
            return(c(as.vector(int), xv,
                     log(as.vector(gsd)),log(d[1,2]/d[1,1]), workingGamma))
          }  else{
            workingGamma     <- NULL
              for(i in 1:length(gamma)){
                tempGamma    <- gamma[[i]]/diag(gamma[[i]]) # result in R+
                tempGamma    <- log(tempGamma)    # result in all R
                workingGamma <- c(workingGamma,as.vector(tempGamma[!diag(2)]))
              }
            
            xv            <- NULL
            for(j in 1:length(xl)) xv<-c(xv, as.vector(t(xl[[j]])))
            
            return(c(as.vector(int), xv,
                     log(as.vector(gsd)),log(d[,2]/d[,1]), workingGamma)) 
          }
  
}

pw2pn <- function(parvect, diff = FALSE, l) { # l is the number of covariates
    if (diff==FALSE) {
      int  <-  matrix(parvect[1:8],4,2)
      
      covL <- list()
      for (j in 1:l){
        lb   <- 8 + 2*(j-1) + 1
        ub   <- 8 + 2*j
        covL[[j]] <- matrix(rep(parvect[lb:ub],4), 4, 2, byrow=TRUE)
      }
      # to be on the safe side
      ub   <- 8 + 2*l
      
      gsd  <-  matrix(exp(parvect[(ub+1):(ub+8)]), 4, 2)
      
      # deltas
      d   <- matrix(rep(c(1,exp(parvect[ub+9]))/sum(c(1, exp(parvect[ub+9])))), 4, 2, byrow = TRUE)
      
      # gamma
      
      gamma         <- diag(2) 
      gamma[!gamma] <- exp(parvect[(ub+10):(ub+11)])
      gamma         <- gamma/apply(gamma,1,sum)
      
      gammalist <-list()
      for(i in 1:4) {
        gammalist[[i]] <- gamma
      }
    }  else {
      
      int  <-  matrix(parvect[1:8] ,4, 2)
      
      covL <- list()
      for (j in 1:l){
        lb   <- 8 + 8*(j-1) + 1
        ub   <- 8 + 8*j
        covL[[j]] <- matrix(parvect[lb:ub], 4, 2, byrow=TRUE)
      }
      # to be on the safe side
      ub   <- 8 + 8*l
      
      gsd  <-  matrix(exp(parvect[(ub+1):(ub+8)]), 4, 2) # Different from Theo's function as it returns the parameter and not its log
      
      # deltas
      
      d <- matrix(0,4,2)
      for( i in 1:4) {d[i,]   <- c(1,exp(parvect[ub + 8 +i]))/sum(c(1, exp(parvect[ub + 8 +i])))}
      
      
      # gamma
      
      gammalist <-list()
      for(i in 1:4) {
        gamma         <- diag(2) 
        gamma[!gamma] <- exp(parvect[(ub+12 + 2*(i-1)+ 1): (ub + 12 + 2*i)])
        gamma         <- gamma/apply(gamma,1,sum)
        
        gammalist[[i]] <- gamma
      }
      
    }
  
  return(list(int=int, covL = covL,
              gsd = gsd, delta = d, gamma = gammalist))
}
################################################################
# Calculates a matrix with all probabilities for a given colony k, the function is within a loop
get_allprobs<- function(lpn, k, datalist, covnm){ 
  # prepare the data matrix for each site
  # covnn are the names of the covariates
  
  S1 <- datalist[[k]][which(datalist[[k]][,"Site"]=="1"),]
  S2 <- datalist[[k]][which(datalist[[k]][,"Site"]=="2"),]
  S3 <- datalist[[k]][which(datalist[[k]][,"Site"]=="3"),]
  
  # S1, S2 and S3 should be of the same size
  
  h  <- length(covnm) # the number of covariates
  cn <- rep(0,h)
  
  for(r in 1:h){ cn[r] <- which(colnames(S1)==covnm[r]) }
  
  n <- nrow(S1) #n is the number of observations for each site in colony k
  
  allprobs   <- matrix(rep(NA, 2*n), nrow=n) 
  
  for (j in 1:2){
    
    # 1
    
    m1             <- lpn$int[k,j]
    for (i in 1:h)    {m1 = m1 + lpn$covL[[i]][k,j]*S1[,cn[i]]}
    m1             <- exp(m1)
    
    pdfS1          <- dgamma(S1[,"D"],     shape=m1^2/lpn$gsd[k,j]^2,
                            scale=lpn$gsd[k,j]^2/m1)  
    
    # 2
    m2             <- lpn$int[k,j]
    for (i in 1:h)    {m2 = m2 + lpn$covL[[i]][k,j]*S2[,cn[i]]}
    m2             <- exp(m2)
    
    pdfS2          <- dgamma(S2[,"D"],  shape=m2^2/lpn$gsd[k,j]^2,
                                          scale=lpn$gsd[k,j]^2/m2)
    
    # 3
    m3             <-lpn$int[k,j]
    for (i in 1:h)   {m3 = m3 + lpn$covL[[i]][k,j]*S3[,cn[i]]}
    m3             <- exp(m3)
    
    pdfS3          <- dgamma(S3[,"D"],   shape=m3^2/lpn$gsd[k,j]^2,
                                          scale=lpn$gsd[k,j]^2/m3)
    
    allprobs[,j]   <- pdfS1*pdfS2*pdfS3
  } # j index
  return (allprobs)
}
####################################################################

# function that computes -log(likelihood) for the model - this will later be minimized numerically

mllk <- function(parvect, datalist, diff=FALSE, covnm) {
  # Transform back to natural parameters
  par <- pw2pn(parvect, diff = diff, l = length(covnm))
  
  #Calculate the log Likelihood
  mllkall<-0
  
  for (k in 1:4){ # it is a loop
    
    # Calculate gamma
    gamma<-par$gamma[[k]]
    
    #Calculate initial probability
    delta<-par$delta[[k]]
    
    # Calculate the probabilities in the state-dependent distributions
    allprobs  <-  get_allprobs(lpn=par, k=k, datalist=datalist, covnm = covnm)
    n         <-  nrow(allprobs)
    
    # Initial term
    foo       <-  delta*allprobs[1,] 
    
    lscale    <-  forward(foo,gamma,allprobs,n) # the forward function requires delta*P as input
    
    mllkall   <-  mllkall-lscale
  }
  return(mllkall)
}
####################################################################
# this function runs the numerical maximization

mle <- function(datalist, parvect0, covnm = covnm, hessian=FALSE, diff = FALSE,  ...){   # parvect0 are working parameters
  
  mod      <- nlm(mllk, parvect0, covnm = covnm, datalist=datalist, diff = diff, print.level=0,iterlim=1500,hessian=hessian,...)
  mllk     <- mod$minimum
  
  pn       <- pw2pn(mod$estimate, diff = diff, l= length(covnm)) # convert estimate back to constrained parameter space
  names(pn$covL) <- covnm
  
  
  
  list(
    int   = pn$int,
    covL  = pn$covL,
    gsd   = pn$gsd,
    delta = pn$delta,
    gamma = pn$gamma,
    mllk=mllk,AIC=2*mllk+2*(length(parvect0)), H=mod$hessian)
}

