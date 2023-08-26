library(tidyverse)
library(ggplot2)
library(readr)

rates <- read_csv("C:/Users/FastRun/OneDrive/Research/Projects/Experimental/Building_data_analysis/rates.csv")
rates$numericID <- factor(rates$ID, labels = c("colony 1", "colony 2", "colony 3", "colony 4"))

# Normalize deposition and pick-up rates by their mean within each colony
rates2 <- rates %>%
  group_by(numericID) %>%
  mutate(norm_D = (D - mean(D)) / sd(D),
         norm_P = (P - mean(P)) / sd(P))


# Create scatterplot using ggplot2 and facet_wrap
theme_set(theme_classic()) + theme_replace(strip.background = element_blank(), strip.text = element_text(face = "bold"));

scatter_plot <- ggplot(rates2, aes(x = norm_D, y = norm_P, color = timepoints)) +
  geom_point() +
  facet_wrap(~ numericID, nrow = 2) + 
  labs(x = "normalized deposition rate", y = "normalized pick-up rate", title = "") +
  scale_color_viridis_c(name = "time from start of\nactivity (min)") +
  theme(axis.text.x = element_text(color = "black", size =c(8)),
        axis.text.y = element_text(color = "black", size =c(8)),
        axis.title.x = element_text(color = "black", size =c(10)),
        axis.title.y = element_text(color = "black", size =c(10)),
        panel.spacing = unit(1, "lines")) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

# Display the scatterplot
print(scatter_plot)

ggsave(filename = "C:/Users/FastRun/OneDrive/Research/Publications/Using Hidden Markov Models to study ant collective behaviour/submission AB/Figure 4.png", 
       plot = scatter_plot, units = 'in', width=6.5, height=5)


# # Display the scatterplot with lines
# print(scatter_plot_with_lines)