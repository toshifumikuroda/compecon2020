# set up
rm(list = ls())

library(gtsummary)
librry(tidyverse)
library(psych)
library(AER)

# データ読み込み
dmtn980 <- readr::read_csv('./data/DMTn980.csv')

# subsampleの作成
dmtn980 <- dmtn980 %>% dplyr::mutate(
  subsample = ifelse(WHOLE == 5, 5,
                     ifelse(FIRST == 1, 1, 
                            ifelse(SECOND == 1, 2, 
                                   ifelse(THIRD == 1, 3, 
                                          ifelse(FOURTH == 1, 4, 0))))))
# table 2
table2 <- dmtn980 %>% select(c(MALE, COLLEGEG, ENVIRO, AGE, INCOME, subsample)) %>%
  tidyr::drop_na(any_of("ENVIRO")) %>%
  group_by(subsample) %>%
  summarise(n = n(),
           male = mean(MALE, na.rm = TRUE),
           college = mean(COLLEGEG, na.rm = TRUE),
           strong_env = mean(ENVIRO, na.rm = TRUE),
           age = mean(AGE, na.rm = TRUE),
           income = mean(INCOME, na.rm = TRUE)
           )

# table 2 with xi test
dmtn980 %>% 
  tidyr::drop_na(any_of("ENVIRO")) %>%
  dplyr::select(c(MALE, COLLEGEG, ENVIRO, AGE, INCOME, subsample)) %>%
  gtsummary::tbl_summary(by = subsample,
                         missing = "no",
                         digits = list("INCOME" ~ -2),
                         statistic = list("AGE" ~ "{mean}",
                                          "ENVIRO" ~ "{p}", "INCOME" ~ "{mean}", 
                                          "MALE" ~ "{p}", "COLLEGEG" ~ "{p}")) %>%
  gtsummary::add_p()

# table 3
dmtn980 %>% tidyr::drop_na(any_of("ENVIRO")) %>%
  group_by(subsample, AMOUNT) %>%
  dplyr::summarize(
    accept = mean(VOTE)
  ) %>%
  tidyr::spread(subsample, accept)
  
# regress
logit <- dmtn980 %>% 
  dplyr::filter(MINUTES>15&MINUTES<120) %>%
  tidyr::drop_na(any_of("ENVIRO")) %>%
  glm(formula = VOTE ~ 0 + WHOLE + FIRST + SECOND + THIRD + FOURTH +
                 AMOUNTW + AMOUNT1 + AMOUNT2 + AMOUNT3 + AMOUNT4,
      data = .,
      family = binomial(link="logit"))

# view
summary(logit)

# WTPまとめ
wtp <- list()
for (i in 1:5) {
  wtp[i] <- (coef(logit)[i]) / (coef(logit)[5+i] ) * (-1)
}
wtp_col <- unlist(wtp)

# Adding Up Test
wtp_whole <- deltaMethod(logit, "-WHOLE/AMOUNTW")
wtp_adding <-deltaMethod(logit, "-FIRST/AMOUNT1 - SECOND/AMOUNT2 - THIRD/AMOUNT3 - FOURTH/AMOUNT4")
wtp <- as.data.frame(rbind(wtp_whole, wtp_adding))
rownames(wtp) <- c("Whole", "Adding Up")
t(wtp)
wtp_zero <- deltaMethod(logit, "WHOLE/AMOUNTW - FIRST/AMOUNT1 - SECOND/AMOUNT2 - THIRD/AMOUNT3 - FOURTH/AMOUNT4")
rownames(wtp_zero) <- "iszero"

# Bar Plot
x <- c(1,2)
wtp <- cbind(wtp, x)
colnames(wtp) <- c("Estimate", "SE", "lower", "upper", "x")

ggplot2::ggplot(wtp) + 
  geom_bar(aes(x = x, y = Estimate), stat="identity", fill="skyblue") +
  geom_errorbar(aes(x = x, ymin = lower, ymax = upper), width=0.4, colour="orange") +
  ggtitle("Adding Up Test") + 
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
