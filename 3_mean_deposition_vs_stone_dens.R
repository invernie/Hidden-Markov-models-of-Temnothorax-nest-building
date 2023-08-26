
library(hmmTMB)
library(ggplot2)
theme_set(theme_classic()) + theme_replace(strip.background = element_blank(), strip.text = element_text(face = "bold"));


# Unpack model and data
load("best_mod.RData")
hmm <- best_mod
hmm_data <- hmm$obs()$data()

# Loop over colonies
df_list <- lapply(1:4, function(i) {
    # Define grid of stone density
    newdata <- data.frame(stone_dens = seq(0, 1.2, length = 100), 
                          colony = unique(hmm_data$colony)[i])
    
    # Predict mean deposition rate for this colony
    pred <- hmm$predict(what = "obspar", newdata = newdata, n_post = 1000)
    
    # Store estimate and confidence bands
    pred_df <- cbind(as.data.frame.table(pred$mean[1,,]),
                     as.data.frame.table(pred$lcl[1,,])[,3],
                     as.data.frame.table(pred$ucl[1,,])[,3])
    names(pred_df) <- c("state", "stone_dens", "mean", "lcl", "ucl")
    
    # Format dataframe for plotting
    pred_df <- pred_df[order(pred_df$state),]
    pred_df$stone_dens <- newdata$stone_dens
    pred_df$colony <- unique(hmm_data$colony)[i]
    return(pred_df)
})
# Combine dataframes for all colonies
df <- do.call(rbind, df_list)

df$colony <- as.character(df$colony)
col_in <- c('R05','R29','R34','R54')
col_out <- c('colony 1','colony 2','colony 3','colony 4')
for (i in 1:4) {
  df$colony[as.character(df$colony) == col_in[i]] <- col_out[i]
}
df$colony <- as.factor(df$colony)

c1 <- 'deepskyblue' 
c2 <- 'darkorange'

p <- ggplot(df, aes(stone_dens, mean, col = state)) + 
    geom_line() + 
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = state), 
                alpha = 0.1, col = NA) +
    facet_wrap("colony") +
    labs(x = "stone density", y = "mean deposition rate") +
    #scale_color_manual(values = hmmTMB:::hmmTMB_cols, name = NULL) +
    #scale_fill_manual(values = hmmTMB:::hmmTMB_cols, name = NULL)
    scale_color_manual(values = c(c1, c2), name = NULL) +
    scale_fill_manual(values = c(c1, c2), name = NULL) +
    theme(axis.text.x = element_text(color = "black", size =c(8)),
          axis.text.y = element_text(color = "black", size =c(8)),
          axis.title.x = element_text(color = "black", size =c(10)),
          axis.title.y = element_text(color = "black", size =c(10)),
          panel.spacing = unit(1, "lines"))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
                                     
p
#ggsave(filename = "Figure7.png", plot = p, width = 6, height = 4)
