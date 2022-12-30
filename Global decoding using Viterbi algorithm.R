viterbi <- function(mod, datalist, covnm) {
  st  <- list()
  par <- list(int = mod$int, covL = mod$covL, gsd = mod$gsd, 
                             delta = mod$delta, gamma = mod$gamma )
  
  for (k in 1:length(datalist)){
     
    n         <- nrow(datalist[[k]])/3 # this function assumes that the number of observations for each site is equal
    xi        <- matrix(0, n, 2) # this matrix will contain the total probabilities of being in each of the two states, for each observation. The tot prob are a function of the prob of being in each state given the independent and dependent variable values, given the transition prob, and, for the first observation only, given the starting prob
    
    allprobs  <-  get_allprobs(lpn=par, k=k, datalist=datalist, covnm = covnm) # returns probability of the colony being in each state given the variable values only (a product of the pdf at each site)
    
    foo       <- mod$delta[1,]*allprobs[1,] # corrects first probability by incorporating starting state probability
    xi[1,]    <- foo/sum(foo)
    
    for (i in 2:n)
    {
      foo     <- apply(xi[i-1,]*mod$gamma[[1]],2,max)*allprobs[i,] # prob of being in each state given the max(trans_prob_from_t-1 * prob_state_t-1)
      xi[i,]  <- foo/sum(foo)
    }
    iv<-numeric(n)
    iv[n]     <-which.max(xi[n,]) # most likely end state
    for (i in (n-1):1)
      iv[i]   <- which.max(mod$gamma[[1]][,iv[i+1]]*xi[i,]) # reverse algorithm: max(trans_prob_to_t+1 * prob_state_t)
    
    st[[k]]   <- iv
  }
  return(st)
}

####
# Test
# viterbi(m, datalist0, c("stone_dens", "stone_dens2"))
# it seems to work