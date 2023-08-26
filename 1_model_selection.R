
library(hmmTMB)
set.seed(13456)

# Load data
raw <- read.csv("rates_by_ROI_NAs.csv")
data <- raw[, c("ID", "D", "stone_dens", "distance", "timepoints", "colony")]

# Order by ID and time
data <- data[with(data, order(ID, timepoints)),]
data$colony <- factor(data$colony)
hmm_data <- data

# Replace zeros by very small numbers (this is better than zero-inflation to 
# look into the relationship between mean deposition rate and covariates.
# Also, it is consistent with the biological theory behind ant's behaviour, 
# which states that the baseline probability is small but never zero)
ind_zero <- which(hmm_data$D == 0)
hmm_data$D[ind_zero] <- runif(length(ind_zero), 0, min(hmm_data$D[-ind_zero]))

# Define observation model
dists <- list(D = "gamma2")
f1 <- list(D = list(mean = ~ stone_dens + colony))
f2 <- list(D = list(mean = ~ stone_dens + I(stone_dens^2) + colony))
f3 <- list(D = list(mean = ~ stone_dens + distance + colony))
f4 <- list(D = list(mean = ~ stone_dens + I(stone_dens^2) + distance + colony))

# Generate sets of starting values
n_rep <- 200
par0_list <- lapply(1:n_rep, function(i) {
    list(D = list(mean = runif(2, c(0.05, 0.3), c(0.2, 0.6)), 
                  sd = runif(2, c(0.05, 0.3), c(0.2, 0.6))))
})

# Model 1: ~ stone density
mod1_list <- lapply(1:n_rep, function(i) {
    hid <- MarkovChain$new(data = hmm_data, n_states = 2, 
                           initial_state = "stationary")
    obs <- Observation$new(data = hmm_data, dists = dists, formulas = f1, 
                           n_states = 2, par = par0_list[[i]])
    hmm <- HMM$new(obs = obs, hid = hid)
    hmm$fit(silent = TRUE)
    return(hmm)
})

# Model 2: ~ stone density + stone density^2
mod2_list <- lapply(1:n_rep, function(i) {
    hid <- MarkovChain$new(data = hmm_data, n_states = 2, 
                           initial_state = "stationary")
    obs <- Observation$new(data = hmm_data, dists = dists, formulas = f2, 
                           n_states = 2, par = par0_list[[i]])
    hmm <- HMM$new(obs = obs, hid = hid)
    hmm$fit(silent = TRUE)
    return(hmm)
})

# Model 3: ~ stone density + distance
mod3_list <- lapply(1:n_rep, function(i) {
    hid <- MarkovChain$new(data = hmm_data, n_states = 2, 
                           initial_state = "stationary")
    obs <- Observation$new(data = hmm_data, dists = dists, formulas = f3, 
                           n_states = 2, par = par0_list[[i]])
    hmm <- HMM$new(obs = obs, hid = hid)
    hmm$fit(silent = TRUE)
    return(hmm)
})

# Model 4: ~ stone density + stone density^2 + distance
mod4_list <- lapply(1:n_rep, function(i) {
    hid <- MarkovChain$new(data = hmm_data, n_states = 2, 
                           initial_state = "stationary")
    obs <- Observation$new(data = hmm_data, dists = dists, formulas = f4, 
                           n_states = 2, par = par0_list[[i]])
    hmm <- HMM$new(obs = obs, hid = hid)
    hmm$fit(silent = TRUE)
    return(hmm)
})

AIC1 <- sapply(mod1_list, AIC)
AIC2 <- sapply(mod2_list, AIC)
AIC3 <- sapply(mod3_list, AIC)
AIC4 <- sapply(mod4_list, AIC)

best_mod <- mod2_list[[which.min(AIC2)]]
save(best_mod, file = "best_mod.RData")
save(mod1_list, file = "mod1.RData")
save(mod3_list, file = "mod3.RData")
save(mod4_list, file = "mod4.RData")