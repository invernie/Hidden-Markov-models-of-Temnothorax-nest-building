# Load the data
load("best_mod.RData")
hmm <- best_mod
hmm_data <- hmm$obs()$data()

# using Theo's data, see the file 1_model_selection.R
glm_1 <- glm(D ~ colony + stone_dens, family = Gamma(link="log"), data = hmm_data)
glm_2 <- glm(D ~ colony + stone_dens + I(stone_dens^2), family = Gamma(link="log"), data = hmm_data)
glm_3 <- glm(D ~ colony + stone_dens + distance, family = Gamma(link="log"), data = hmm_data)
glm_4 <- glm(D ~ colony + stone_dens + I(stone_dens^2) + distance,  family = Gamma(link="log"), data = hmm_data)

summary(glm_2)

AIC(glm_1, glm_2, glm_3, glm_4) # -134.68
