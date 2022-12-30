##################################################################
# Simulate from the fitted model


HMM.generate_sample  <- function(mod, covnm, datalist)
{
  h  <- length(covnm)         # the number of covariates
  cn <- rep(0,h)
  for(r in 1:h)    { cn[r] <- which(colnames(datalist[[1]])==covnm[r]) }
  # An empty list to be filled out with the simulated data
  sim.d                    <- list()
  for (i in 1:4){
  ns                       <- nrow(datalist[[i]])/3 # number of observations for each site
  mvect                    <- 1:2
  state                    <- numeric(ns)
  state[1]                 <- sample(mvect,1,prob=mod$delta[1,])
  for (y in 2:ns) state[y] <- sample(mvect,1,prob=mod$gamma[[1]][state[y-1],])
  state3                   <- rep(state, each=3) # to account for each site
  
  m1<-g1                   <- numeric(length(state3)) # empty vectors for the mean and the variance of the gamma distribution
  for (j in 1:length(state3)) {
    m1[j]                  <- mod$int[i, state3[j]]
    for (g in 1:h)  {m1[j] <- m1[j] + mod$covL[[g]][i,state3[j]]*datalist[[i]][j,cn[g]]}
    g1[j]                  <- mod$gsd[i,state3[j]]
  }
  
  m1                       <- exp(m1)
  D                        <- rgamma(length(m1), shape = m1^2/g1^2, scale = (g1^2)/m1) 
  Site                     <- rep(1:3, ns)
  sim.d[[i]]               <- cbind(D, Site, datalist0[[i]][,cn])
  }
  
  return(sim.d)
}
