library(hmmTMB)
library(ggplot2)
theme_set(theme_classic()) + theme_replace(strip.background = element_blank(), strip.text = element_text(face = "bold"));

# Unpack model and data
load("best_mod.RData")
hmm <- best_mod
hmm_data_ <- hmm$obs()$data()

hmm_data_$colony <- as.character(hmm_data_$colony)
col_in <- c('R05','R29','R34','R54')
col_out <- c('colony 1','colony 2','colony 3','colony 4')
for (i in 1:4) {
  hmm_data_$colony[hmm_data_$colony == col_in[i]] <- col_out[i]
}
hmm_data_$colony <- as.factor(hmm_data_$colony)

hmm_data_$state <- as.factor(hmm$viterbi())

c1 <- 'deepskyblue' 
c2 <- 'darkorange'


p3 <- ggplot(data = hmm_data_, aes(timepoints, D, fill = state)) +
  facet_wrap("colony") +
  geom_bar(stat="identity", col = 'white', alpha = 0.75) +
  labs(x = "timepoints", y = "deposition rate") +
  scale_color_manual(values = c(c1, c2), name = NULL) +
  scale_fill_manual(values = c(c1, c2), name = NULL, labels = c('state 1', 'state 2')) +
  theme(axis.text.x = element_text(color = "black", size =c(8)),
        axis.text.y = element_text(color = "black", size =c(8)),
        axis.title.x = element_text(color = "black", size =c(10)),
        axis.title.y = element_text(color = "black", size =c(10)),
        panel.spacing = unit(1, "lines")) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

p3

ggsave(filename = "C:/Users/FastRun/OneDrive/Publications/Using Hidden Markov Models to study ant collective behaviour/submission AB/Appendix 2.png", 
    plot = p3, units = 'in', width=6.5, height=5)
