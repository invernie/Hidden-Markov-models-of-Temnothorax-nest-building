
library(hmmTMB)
library(ggplot2)
theme_set(theme_classic()) + theme_replace(strip.background = element_blank(), strip.text = element_text(face = "bold"));

# Unpack model and data
load("best_mod.RData")
hmm <- best_mod
hmm_data <- hmm$obs()$data()

# Predict parameters
newdata <- data.frame(colony = unique(hmm_data$colony), 
                      stone_dens = mean(hmm_data$stone_dens, na.rm = TRUE))
par <- hmm$predict(what = "obspar", newdata = newdata)

# Weights for state-dependent distributions
v <- hmm$viterbi()
w <- sapply(unique(hmm_data$colony), function(c) {
    ind <- which(hmm_data$colony == c)
    table(v[ind])/length(ind)
})

hmm_data$colony <- as.character(hmm_data$colony)
col_in <- c('R05','R29','R34','R54')
col_out <- c('colony 1','colony 2','colony 3','colony 4')
for (i in 1:4) {
  hmm_data$colony[hmm_data$colony == col_in[i]] <- col_out[i]
}
hmm_data$colony <- as.factor(hmm_data$colony)

# Grid of observed variable (x axis)
grid <- seq(min(hmm_data$D), max(hmm_data$D), length = 100)

# For each colony, compute state-dependent distributions
pdf_ls <- lapply(1:dim(par)[3], function(i) {
    p <- par[,,i]
    shape <- p["D.mean",]^2 / p["D.sd",]^2
    rate <- p["D.mean",] / p["D.sd",]^2
    pdf1 <- w[1,i] * dgamma(grid, shape = shape[1], rate = rate[1])
    pdf2 <- w[2,i] * dgamma(grid, shape = shape[2], rate = rate[2])
    res <- data.frame(D = grid, pdf = c(pdf1, pdf2),
                      state = factor(rep(1:2, each = length(grid))),
                      colony = unique(hmm_data$colony)[i])
    return(res)
})
# Turn into dataframe for ggplot
pdf_df <- do.call(rbind, pdf_ls)

c1 <- 'deepskyblue' 
c2 <- 'darkorange'

p2 <- ggplot(pdf_df, aes(D, pdf)) +
    facet_wrap("colony") +
    geom_histogram(aes(x = D, y = after_stat(density)), 
                   breaks = seq(0, 3.2, by = 0.2),
                   col = "black", bg = "lightgrey", data = hmm_data) +
    geom_line(aes(col = state)) +
    labs(x = "deposition rate", y = "density") +
    scale_color_manual(values = c(c1, c2), name = NULL, labels = c('state 1', 'state 2')) +
    scale_fill_manual(values = c(c1, c2), name = NULL) +
    theme(axis.text.x = element_text(color = "black", size =c(8)),
          axis.text.y = element_text(color = "black", size =c(8)),
          axis.title.x = element_text(color = "black", size =c(10)),
          axis.title.y = element_text(color = "black", size =c(10)),
          panel.spacing = unit(1, "lines"))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

p2
#ggsave(filename = "Figure 5B.png", plot = p2, width = 6, height = 4)
