# Setup
## ライブラリ読み込み
library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(hdm)
library(huxtable)
library(AER)
library(xtable)

## 初期設定
rm(list = ls())
author <- "Toshi"
## ファイルはメニューからupload済みとする

## unzip
for (i in 1:5) {
  zipfilename <- paste("./data/24_AJR/maketable", toString(i), ".zip", sep="")
  unzip(zipfile = zipfilename, exdir = "./data/24_AJR/")
  }

# Figure 1
table1_df <- foreign::read.dta("./data/24_AJR/maketable1.dta")
colnames(table1_df)
figure1 <- table1_df %>% dplyr::filter(baseco==1) %>%
  ggplot2::ggplot(aes(x = logem4, y = logpgp95, label = shortnam)) +
  ggplot2::geom_text() +
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black", size = 1) +
  xlab("Log European settler mortality") + 
  ylab("log GDP per capita, PPP, in 1995") +
  theme_classic() +
  annotate("text",label = paste0("made by ", author), x=-Inf, y=-Inf, hjust=0,vjust=0)

ggsave(file = "./figuretable/AJR_figure1.png", plot = figure1, dpi = 100, width = 9.6, height = 5.4)

# Table 1
colvars <- c("logpgp95", "loghjypl", "avexpr", "cons00a", "cons1", "democ00a", "euro1900", "logem4")

table1_col1 <- table1_df %>%
  dplyr::select(any_of(colvars)) %>%
  group_by(n = n()) %>%
  dplyr::summarise_all(
    list(mean), na.rm = TRUE
  )

table1_col2 <- table1_df %>%
  dplyr::filter(baseco==1) %>%
  dplyr::select(any_of(colvars)) %>%
  group_by(n = n()) %>%
  dplyr::summarise_all(
    list(mean), na.rm = TRUE
  )

table1_col3 <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) > logem4
                ) %>%
  dplyr::select(any_of(colvars)) %>%
  group_by(n = n()) %>%
  dplyr::summarise_all(
    list(mean), na.rm = TRUE
  )

table1_col4 <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.25, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.50, na.rm = TRUE) > logem4) %>%
  dplyr::select(any_of(colvars)) %>%
  group_by(n = n()) %>%
  dplyr::summarise_all(
    list(mean), na.rm = TRUE
  )
table1_col5 <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & 
                  quantile(logem4, 0.50, na.rm = TRUE) <= logem4 &
                  quantile(logem4, 0.75, na.rm = TRUE) > logem4) %>%
  dplyr::select(any_of(colvars)) %>%
  group_by(n = n()) %>%
  dplyr::summarise_all(
    list(mean), na.rm = TRUE
  )
table1_col6 <- table1_df %>%
  dplyr::filter(!is.na(logem4)) %>%
  dplyr::filter(baseco==1 & quantile(logem4, 0.75, na.rm = TRUE) <= logem4) %>%
  dplyr::select(any_of(colvars)) %>%
  group_by(n = n()) %>%
  dplyr::summarise_all(
    list(mean), na.rm = TRUE
  )

table1 <- t(as.data.frame(rbind(table1_col1, table1_col2, table1_col3, table1_col4, table1_col5, table1_col6)))
rownames(table1) <- c("Number of observations", "log GDP per capita, PPP, in 1995",
              "log output per worker in 1988", 
              "Average protection against expropriation risk, 1985-1995", 
              "Constraint on executive, 1900", 
              "Constraint on executive in first year of independence", 
              "Democracy in 1900", 
              "European settlement in 1900",
              "Log European settler mortality")

colnames(table1) <- c("Whole world", "Base sample", "0~25%", "25%~50%", "50%~75%", "75%~100%")

print(
  xtable(table1, align=c("l", "r", "r", "r", "r", "r", "r"), 
         caption=paste0("Table 1 - Descritive Statisticss made by ",author)),
  format.args = list(big.mark = " ", decimal.mark = ","),
  type = "html",
  file = "figuretable/24_AJR_table1.html")

## Col 3 - 5 の閾値
exp(quantile(table1_df$logem4, 0.25, na.rm = TRUE))
exp(quantile(table1_df$logem4, 0.50, na.rm = TRUE))
exp(quantile(table1_df$logem4, 0.75, na.rm = TRUE))

# Table 2
table2_df <- foreign::read.dta("./data/24_AJR/maketable2.dta")

## table 2 formula objects
table2_xvars <- c("avexpr", "lat_abst", "africa + asia + other")
table2_yvars <- c("logpgp95", "loghjypl")

## Column 1
xvars_col1 <- paste(table2_xvars[1], collapse = " + ")
model_col1 <- paste("logpgp95", xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)
table2_col1 <- estimatr::lm_robust(data = table2_df, formula = model_col1, se_type = "stata")

## Column 2
table2_col2 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col1, se_type = "stata")

## Column 3
xvars_col3 <- paste(table2_xvars[1:2], collapse = " + ")
model_col3 <- paste("logpgp95", xvars_col3, sep = " ~ ")
model_col3 <- as.formula(model_col3)
table2_col3 <- estimatr::lm_robust(data = table2_df, formula = model_col3, se_type = "stata")

## Column 4
xvars_col4 <- paste(table2_xvars, collapse = " + ")
model_col4 <- paste("logpgp95", xvars_col4, sep = " ~ ")
model_col4 <- as.formula(model_col4)
table2_col4 <- estimatr::lm_robust(data = table2_df, formula = model_col4, se_type = "stata")

## Column 5
table2_col5 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col3, se_type = "stata")

## Column 6
table2_col6 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col4, se_type = "stata")

## Column 7
model_col7 <- paste("loghjypl", xvars_col1, sep = " ~ ")
model_col7 <- as.formula(model_col7)
table2_col7 <- estimatr::lm_robust(data = table2_df, formula = model_col7, se_type = "stata")

## Column 8
table2_col8 <- estimatr::lm_robust(data = table2_df %>% dplyr::filter(baseco==1), formula = model_col7, se_type = "stata")

table2 <- huxtable::huxreg("Whole world (1)" = table2_col1, 
                 "Base sample (2)" = table2_col2, 
                 "Whole world (3)" = table2_col3, 
                 "Whole world (4)" = table2_col4,
                 "Base sample (5)" = table2_col5,
                 "Base sample (6)" = table2_col6,
                 "Whole world (7)" = table2_col7,
                 "Base sample (8)" = table2_col8,
                 coefs = c("Average protection \n against expropriation risk,\n 1985 ~ 1995" = "avexpr",
                           "Latitude" = "lat_abst",
                           "Asia dummy" = "asia",
                           "Africa dummy" = "africa",
                           "Other continent dummy" = "other"),
                 omit_coefs = c("(Intercept)"),
                 statistics = c('R2'= 'r.squared', 'Number of observations' = 'nobs'),
                 stars = NULL,
                 number_format = 2) %>%
  huxtable::add_rows(huxtable::hux("", "Dependent variable is log GDP per capita in 1995", "", "", "", "", "", 
            "Dependent variable is log output per worker in 1988", ""), after = 1) %>%
  huxtable::merge_cells(2, 2:7) %>%
  huxtable::merge_cells(2, 8:9) %>%
  huxtable::set_caption(paste0("OLS Regressions made by ",author))

cat(huxtable::to_html(table2), 
          file = "figuretable/24_AJR_table2.html")

# Figure 2
figure2 <- table2_df %>% dplyr::filter(baseco==1) %>% 
  ggplot2::ggplot(aes(x = avexpr , y = logpgp95, label = shortnam)) +
  ggplot2::geom_text() +
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black", size = 1) +
  xlab("Average Expropritation Risk 1985 ~ 95") + 
  ylab("log GDP per capita, PPP, in 1995") +
  theme_classic() +
  annotate("text",label = paste0("made by ", author), x=-Inf, y=-Inf, hjust=0,vjust=0)
ggsave(file = "./figuretable/AJR_figure2.png", plot = figure2, dpi = 100, width = 9.6, height = 5.4)

# Figure 3
table3_df <- foreign::read.dta("./data/24_AJR/maketable3.dta") %>%
  dplyr::filter(excolony==1&!is.na(extmort4)) %>%
  dplyr::mutate(euro1900 = euro1900 / 100)

figure3 <- table3_df %>% dplyr::filter(!is.na(logpgp95)) %>% 
  ggplot2::ggplot(aes(x = logem4 , y = avexpr)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, colour = "black", size = 1) +
  xlab("Log European settler mortality") + 
  ylab("Average Expropritation Risk 1985 ~ 95") +
  theme_classic() +
  annotate("text",label = paste0("made by ", author), x=-Inf, y=-Inf, hjust=0,vjust=0)
ggsave(file = "./figuretable/AJR_figure3.png", plot = figure3, dpi = 100, width = 9.6, height = 5.4)

# Table 3
## Panel A
i <- 0
table3_col_odd <- list()
table3_col_even <-list()
for (xvar in c("cons00a", "democ00a", "indtime + cons1", "euro1900")) {
  i <- i + 1
  ## Column odd
  xvars_col_odd <- paste(xvar, collapse = " + ")
  model_col_odd <- paste("avexpr", xvars_col_odd, sep = " ~ ")
  model_col_odd <- as.formula(model_col_odd)
  table3_col_odd[[i]] <- lm(data = table3_df, formula = model_col_odd)
  #table3_col_odd[[i]] <- estimatr::lm_robust(data = table3_df, formula = model_col_odd, se_type = "stata")
  ## Columns odd
  xvars_col_even <- paste(c(xvar, "lat_abst"), collapse = " + ")
  model_col_even <- paste("avexpr", xvars_col_even, sep = " ~ ")
  model_col_even <- as.formula(model_col_even)
  table3_col_even[[i]] <- lm(data = table3_df, formula = model_col_even)
  #table3_col_even[[i]] <- estimatr::lm_robust(data = table3_df, formula = model_col_even, se_type = "stata")
}

## Column odd
xvars_col_odd <- paste("logem4", collapse = " + ")
model_col_odd <- paste("avexpr", xvars_col_odd, sep = " ~ ")
model_col_odd <- as.formula(model_col_odd)
table3_col_odd[[5]] <- lm(data = table3_df %>% dplyr::filter(!is.na(logpgp95)), formula = model_col_odd)
## Columns odd
xvars_col_even <- paste(c("logem4", "lat_abst"), collapse = " + ")
model_col_even <- paste("avexpr", xvars_col_even, sep = " ~ ")
model_col_even <- as.formula(model_col_even)
table3_col_even[[5]] <- lm(data = table3_df %>% dplyr::filter(!is.na(logpgp95)), formula = model_col_even)

table3_a <- huxtable::huxreg(table3_col_odd[[1]], table3_col_even[[1]],
                           table3_col_odd[[2]], table3_col_even[[2]],
                           table3_col_odd[[3]], table3_col_even[[3]],
                           table3_col_odd[[4]], table3_col_even[[4]],
                           table3_col_odd[[5]], table3_col_even[[5]],
                           coefs = c("Constraint on executive in\n 1900" = "cons00a",
                                     "Democracy in 1900" = "democ00a", 
                                     "Constraint on executive in first\n year of independence" = "cons1", 
                                     "European settlements in 1900" = "euro1900", 
                                     "Log European settler mortality" = "logem4",
                                     "Latitude" = "lat_abst"
                                     ),
                           omit_coefs = c("(Intercept)"),
                           statistics = c('R2'= 'r.squared', 'Number of observations' = 'nobs'),
                           stars = NULL,
                           number_format = 2) %>%
  huxtable::add_rows(huxtable::hux("Panel A", "Dependent Variable Is Average Protection Against Expropriation Risk in 1985–1995",
                                   "", "", "", "", "", "", "", "", ""), after = 1) %>%
  huxtable::merge_cells(2, 2:10) %>%
  huxtable::set_caption(paste0("Determinents of Institutions made by ",author))

cat(huxtable::to_html(table3_a), 
    file = "figuretable/24_AJR_table3_a.html")

## Panel B
model_col <- c(
  "cons00a ~ euro1900",
  "cons00a ~ euro1900 + lat_abst",
  "cons00a ~ logem4",
  "cons00a ~ logem4 + lat_abst",
  "democ00a ~ euro1900",
  "democ00a ~ euro1900 + lat_abst",
  "democ00a ~ logem4",
  "democ00a ~ logem4 + lat_abst",
  "euro1900 ~ logem4",
  "euro1900 ~ logem4 + lat_abst"
  )
table3_panel_b <- list()
for (i in 1:2) {
  table3_panel_b[[i]] <- estimatr::lm_robust(data = table3_df %>% dplyr::filter(!is.na(logpgp95)), formula = as.formula(model_col[i]), se_type = "stata")
}
for (i in 3:4) {
  table3_panel_b[[i]] <- estimatr::lm_robust(data = table3_df, formula = as.formula(model_col[i]), se_type = "stata")
}
for (i in 5:10) {
  table3_panel_b[[i]] <- estimatr::lm_robust(data = table3_df %>% dplyr::filter(!is.na(logpgp95)), formula = as.formula(model_col[i]), se_type = "stata")
}

table3_b <- huxtable::huxreg(table3_panel_b[[1]], table3_panel_b[[2]],
                             table3_panel_b[[3]], table3_panel_b[[4]],
                             table3_panel_b[[5]], table3_panel_b[[6]],
                             table3_panel_b[[7]], table3_panel_b[[8]],
                             table3_panel_b[[9]], table3_panel_b[[10]],
                             coefs = c("European settlements in 1900" = "euro1900",
                                       "Log European settler mortality" = "logem4", 
                                       "Latitude" = "lat_abst"
                             ),
                             omit_coefs = c("(Intercept)"),
                             statistics = c('R2'= 'r.squared', 'Number of observations' = 'nobs'),
                             stars = NULL,
                             number_format = 2) %>%
  huxtable::add_rows(huxtable::hux("Panel B", "Dependent Variable Is Constraint\n on Executive in 1900", "", "", "",
                                   "Dependent Variable Is\n Democracy in 1900", "", "", "", 
                                   "Dependent Variable Is\n European Settlements\n in 1900", ""), after = 1) %>%
  huxtable::merge_cells(2, 2:5) %>%
  huxtable::merge_cells(2, 6:9) %>%
  huxtable::merge_cells(2, 10:11) %>%
  huxtable::set_caption(paste0("Determinents of Institutions made by ",author))
cat(huxtable::to_html(table3_b), 
   file = "figuretable/24_AJR_table3_b.html")

# Table 4
## prepare data
table4_df <- foreign::read.dta("./data/24_AJR/maketable4.dta") %>%
  dplyr::filter(baseco==1) %>%
  dplyr::mutate(other_cont = ifelse(shortnam=="AUS" | shortnam=="MLT" | shortnam=="NZL", 1, 0))

## Panel A
table4_a_col1 <- AER::ivreg(formula = "logpgp95 ~ avexpr | logem4", data = table4_df)
table4_a_col2 <- AER::ivreg(formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", data = table4_df)
table4_a_col3 <- AER::ivreg(formula = "logpgp95 ~ avexpr | logem4", data = table4_df %>% dplyr::filter(rich4!=1))
table4_a_col4 <- AER::ivreg(formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", data = table4_df %>% dplyr::filter(rich4!=1))
table4_a_col5 <- AER::ivreg(formula = "logpgp95 ~ avexpr | logem4", data = table4_df %>% dplyr::filter(africa!=1))
table4_a_col6 <- AER::ivreg(formula = "logpgp95 ~ avexpr + lat_abst| logem4 + lat_abst", data = table4_df %>% dplyr::filter(africa!=1))
table4_a_col7 <- AER::ivreg(formula = "logpgp95 ~ avexpr + asia + africa + other_cont | logem4 + africa + asia + other_cont", data = table4_df)
table4_a_col8 <- AER::ivreg(formula = "logpgp95 ~ avexpr + lat_abst + asia + africa + other_cont | logem4 + lat_abst + africa + asia + other_cont", data = table4_df)
table4_a_col9 <- AER::ivreg(formula = "loghjypl ~ avexpr | logem4", data = table4_df)

table4_a <- huxtable::huxreg("Base sample" = table4_a_col1, table4_a_col2, 
                             "Base sample\n without Neo-Europes" = table4_a_col3, table4_a_col4, 
                             "Base sample\n without Africa" = table4_a_col5, table4_a_col6, 
                             "Base sample\n with continent dummies" = table4_a_col7, table4_a_col8, 
                             "Base sample of \n log output per worker" = table4_a_col9, 
                             coefs = c("Average protection against\n expropriation risk 1985–1995" = "avexpr",
                                       "Latitude" = "lat_abst",
                                       "Asia dummy" = "asia",
                                       "Africa dummy" = "africa",
                                       "Other continent dummy" = "other_cont"),
                             stars = NULL,
                             number_format = 2) %>%
  huxtable::merge_cells(1, 2:3) %>%
  huxtable::merge_cells(1, 4:5) %>%
  huxtable::merge_cells(1, 6:7) %>%
  huxtable::merge_cells(1, 8:9) %>%
  huxtable::set_caption(paste0("IV REGRESSIONS OF LOG GDP PER CAPITA made by ",author))
cat(huxtable::to_html(table4_a), 
    file = "figuretable/24_AJR_table4_a.html")

## Panel B
table4_b_col1 <- lm(formula = "avexpr ~ logem4", data = table4_df)
table4_b_col2 <- lm(formula = "avexpr ~ logem4 + lat_abst", data = table4_df)
table4_b_col3 <- lm(formula = "avexpr ~ logem4", data = table4_df %>% dplyr::filter(rich4!=1))
table4_b_col4 <- lm(formula = "avexpr ~ logem4 + lat_abst", data = table4_df %>% dplyr::filter(rich4!=1))
table4_b_col5 <- lm(formula = "avexpr ~ logem4", data = table4_df %>% dplyr::filter(africa!=1))
table4_b_col6 <- lm(formula = "avexpr ~ logem4 + lat_abst", data = table4_df %>% dplyr::filter(africa!=1))
table4_b_col7 <- lm(formula = "avexpr ~ logem4 + asia + africa + other_cont", data = table4_df)
table4_b_col8 <- lm(formula = "avexpr ~ logem4 + lat_abst + asia + africa + other_cont", data = table4_df)
table4_b_col9 <- lm(formula = "avexpr ~ logem4", data = table4_df)

table4_b <- huxtable::huxreg("Base sample" = table4_b_col1, table4_b_col2, 
                             "Base sample\n without Neo-Europes" = table4_b_col3, table4_b_col4, 
                             "Base sample\n without Africa" = table4_b_col5, table4_b_col6, 
                             "Base sample\n with continent dummies" = table4_b_col7, table4_b_col8, 
                             "Base sample of \n log output per worker" = table4_b_col9, 
                             coefs = c("Log European settler mortality" = "logem4",
                                       "Latitude" = "lat_abst",
                                       "Asia dummy" = "asia",
                                       "Africa dummy" = "africa",
                                       "Other continent dummy" = "other_cont"),
                             omit_coefs = c("(Intercept)"),
                             statistics = c('R2'= 'r.squared', 'Number of observations' = 'nobs'),
                             stars = NULL,
                             number_format = 2) %>%
  huxtable::merge_cells(1, 2:3) %>%
  huxtable::merge_cells(1, 4:5) %>%
  huxtable::merge_cells(1, 6:7) %>%
  huxtable::merge_cells(1, 8:9) %>%
  huxtable::set_caption(paste0("Panel B: First Stage for Average Protection Against Expropriation Risk in 1985 ~ 1995 made by ",author))
cat(huxtable::to_html(table4_b), 
    file = "figuretable/24_AJR_table4_b.html")

## Panel C
table4_c_col1 <- lm(formula = "logpgp95 ~ avexpr", data = table4_df)
table4_c_col2 <- lm(formula = "logpgp95 ~ avexpr + lat_abst", data = table4_df)
table4_c_col3 <- lm(formula = "logpgp95 ~ avexpr", data = table4_df %>% dplyr::filter(rich4!=1))
table4_c_col4 <- lm(formula = "logpgp95 ~ avexpr + lat_abst", data = table4_df %>% dplyr::filter(rich4!=1))
table4_c_col5 <- lm(formula = "logpgp95 ~ avexpr", data = table4_df %>% dplyr::filter(africa!=1))
table4_c_col6 <- lm(formula = "logpgp95 ~ avexpr + lat_abst", data = table4_df %>% dplyr::filter(africa!=1))
table4_c_col7 <- lm(formula = "logpgp95 ~ avexpr + asia + africa + other_cont", data = table4_df)
table4_c_col8 <- lm(formula = "logpgp95 ~ avexpr + lat_abst + asia + africa + other_cont", data = table4_df)
table4_c_col9 <- lm(formula = "loghjypl ~ avexpr", data = table4_df)

table4_c <- huxtable::huxreg("Base sample" = table4_c_col1, table4_c_col2, 
                             "Base sample\n without Neo-Europes" = table4_c_col3, table4_c_col4, 
                             "Base sample\n without Africa" = table4_c_col5, table4_c_col6, 
                             "Base sample\n with continent dummies" = table4_c_col7, table4_c_col8, 
                             "Base sample of \n log output per worker" = table4_c_col9, 
                             coefs = c("Average protection against\n expropriation risk 1985 ~ 1995" = "avexpr",
                                       "Latitude" = "lat_abst",
                                       "Asia dummy" = "asia",
                                       "Africa dummy" = "africa",
                                       "Other continent dummy" = "other_cont"),
                             omit_coefs = c("(Intercept)"),
                             statistics = c('R2'= 'r.squared', 'Number of observations' = 'nobs'),
                             stars = NULL,
                             number_format = 2) %>%
  huxtable::merge_cells(1, 2:3) %>%
  huxtable::merge_cells(1, 4:5) %>%
  huxtable::merge_cells(1, 6:7) %>%
  huxtable::merge_cells(1, 8:9) %>%
  huxtable::set_caption(paste0("Panel C: Ordinary Least Squares made by ",author))
cat(huxtable::to_html(table4_c), 
    file = "figuretable/24_AJR_table4_c.html")
