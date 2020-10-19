# setup
rm(list = ls())

library(dplye)
library(ggpubr)
lancet_palette <- get_palette(palette = "lancet", 10)


# generate X
df <- as.data.frame(x <- seq(- 10, 10, by = 0.1)) %>%
  dplyr::mutate(y_noromal = pnorm(x)) %>%
  dplyr::mutate(y_plogis = plogis(x))

# plot normal and logistic
df %>% ggplot2::ggplot(aes(df)) +
  geom_point(aes(x = x, y =y_noromal, colour = "Normal")) +
  geom_point(aes(x = x, y =y_plogis, colour = "Logistic")) + 
  theme_bw()

