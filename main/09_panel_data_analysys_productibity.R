# Replicate Bloom and Reenen (2007) to understand panel data estimation for productivity

# setup
## Clear memory
rm(list=ls())

## load library
library(tidyverse)
library(psych)
library(estimatr)
library(huxtable)

## file downloads 
#download.file("https://stacks.stanford.edu/file/druid:xh580yd6172/realdata.csv",
#              "./data/realdata.csv")
## load csv
realdata <- readr::read_csv("./data/realdata.csv")

# Figure 1
realdata %>% ggplot2::ggplot(aes(x=amanagement))+
  geom_histogram(aes(y = ..density..), bins = 50)+
  facet_wrap(cty ~ ., nrow = 2)+
  scale_y_continuous(breaks=seq(0,1.2,0.2)) +
  labs(title="Distribution of Management Scores by Country",x="Management Score", y = "Density")+
  theme_classic()

# Management table
psych::describeBy(realdata$management, group = realdata$cty)


# Table 1 
## Col1
realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(ls ~ zmanagement + le + le_fr + le_gr + le_uncons + le_us + uncons + cty + factor(year), data =.,  clusters = code)

## can't replicate by using cty and factor(year)
### define cyy* as set variavles
cyy <- paste("cyy", 1:44, sep = "")

### define xvars
col1 <- c("zmanagement", "le", "le_fr", "le_gr", "le_uncons", "le_us", "uncons")

### paste to model 1 formula
xvars_col1 <- paste(c(col1, cyy), collapse = " + ")
model_col1 <- paste("ls", xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)

### regress
table1_1 <- 
  realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col1, data =.,  clusters = code)

### regress again to use stata's cluster robust option
table1_1 <- 
  realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col1, data =.,  clusters = code, se_type = "stata")


## Col2
### define xvars
model_base <- c("zmanagement", "le", "lp", "lm", 
                "le_fr", "le_gr", "le_uncons", "le_us", 
                "lp_fr", "lp_gr", "lp_uncons", "lp_us", 
                "lm_fr", "lm_gr", "lm_un", "lm_us", 
                "uncons")

### paste to model 2 formula
xvars_col2 <- paste(c(model_base, cyy), collapse = " + ")
model_col2 <- paste("ls", xvars_col2, sep = " ~ ")
model_col2 <- as.formula(model_col2)

### regress
table1_2 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(model_col2, data =.,  clusters = code, se_type = "stata")

## Col3
### define set variables
# controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs", "factor(sic3)") # this is the paper's model
controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs", "factor(sic3)") # this is a model in a class

### paste to model 3 formula
#xvars_col3 <- paste(c(model_base, cyy, controls), collapse = " + ")
#model_col3 <- paste("ls", xvars_col3, sep = " ~ ")
#model_col3 <- as.formula(model_col3)

### regress col3
#table1_3 <- realdata %>%
#  tidyr::drop_na(lm, lp) %>%
#  estimatr::lm_robust(formula = model_col3, data =.,  clusters = code, se_type = "stata")

## Col4
###redefine controls to avoid memory overflow
#controls <- c("lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "lhrs")
### define noise variables
noise <- c("gender", "sen1", "tenurepost", "countries", 
           "day2", "day3", "day4", "day5", "timelocal", "duration", "reli", 
           "aa1", "aa2", "aa3", "aa4", "aa5", "aa6", "aa7", "aa8", "aa9", 
           "aa10", "aa11", "aa12", "aa13", "aa14", "aa15", "aa16")

### paste to model 4 formula
#xvars_col4 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
#model_col4 <- paste("ls", xvars_col4, sep = " ~ ")
#model_col4 <- as.formula(model_col4)

### regress col4
#table1_4 <- realdata %>%
#  tidyr::drop_na(lm, lp) %>%
#  estimatr::lm_robust(formula = model_col4, data =.,  clusters = code, se_type = "stata")

## Col6
### paste to model 6 formula
xvars_col6 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col6 <- paste("roce", xvars_col6, sep = " ~ ")
model_col6 <- as.formula(model_col6)

table1_6 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col6, 
            data =.,  clusters = code, se_type = "stata")

## Col7
### paste to model 7 formula
xvars_col7 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col7 <- paste("lq", xvars_col7, sep = " ~ ")
model_col7 <- as.formula(model_col7)

table1_7 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col7, 
            data =.,  clusters = code, se_type = "stata")

## Col9
### paste to model 9 formula
xvars_col9 <- paste(c(model_base, noise, cyy, controls), collapse = " + ")
model_col9 <- paste("dsales", xvars_col6, sep = " ~ ")
model_col9 <- as.formula(model_col9)

table1_9 <- realdata %>%
  tidyr::drop_na(lm, lp) %>%
  estimatr::lm_robust(formula = model_col9, 
            data =.,  clusters = code,se_type = "stata")


### heteroveneity coefficients
input_heteros <- generics::tidy(table1_2) %>% 
  filter(str_detect(term, "_")) %>% 
  pull(term)

### sic coefficients
sics <- generics::tidy(table1_6) %>% 
  dplyr::filter(str_detect(term, "sic")) %>% 
  pull(term)

huxtable::huxreg("(1)"=table1_1, "(2)"=table1_2, "(6)"=table1_6, "(7)"=table1_7, "(9)"=table1_9,
                 coefs = c("Management z-score" = "zmanagement", 
                           "Ln(Labor)" = "le", "Ln(Capital)" = "lp", "Ln(Materials)" = "lm"),
       omit_coefs = c("(Intercept)", "uncons", cyy, input_heteros, sics, noise, controls),
       statistics = c('# observations' = 'nobs'),
       stars = NULL) %>%
  add_rows(hux("Estimation method ", "OLS", "OLS", "OLS", "OLS", "OLS"), after = 1) %>%  
  add_rows(hux("Firms", "All", "All", "All", "All", "All"), after = 2 ) %>%  
  add_rows(hux("Dependent variable ", "Sales", "Sales", "Profitability", "Tobin’s av. Q", "Sales growth"), after = 3) %>%  
  add_rows(hux("Country, time, and industry dummies", "Yes", "Yes", "Yes", "Yes", "Yes"), after = nrow(.) - 1) %>%  
  add_rows(hux("General controls", "No", "No", "Yes", "Yes", "Yes"), after = nrow(.) - 1) %>% 
  add_rows(hux("Noise controls", "No", "No", "Yes", "Yes", "Yes"), after = nrow(.) - 1)

table1 <- huxtable::huxreg("(1)"=table1_1, "(2)"=table1_2, "(6)"=table1_6, "(7)"=table1_7, "(9)"=table1_9,
                 coefs = c("Management z-score" = "zmanagement", 
                           "Ln(Labor)" = "le", "Ln(Capital)" = "lp", "Ln(Materials)" = "lm"),
                 omit_coefs = c("(Intercept)", "uncons", cyy, input_heteros, sics, noise, controls),
                 statistics = c('# observations' = 'nobs'),
                 stars = NULL) %>%
  add_rows(hux("Estimation method ", "OLS", "OLS", "OLS", "OLS", "OLS"), after = 1) %>%  
  add_rows(hux("Firms", "All", "All", "All", "All", "All"), after = 2 ) %>%  
  add_rows(hux("Dependent variable ", "Sales", "Sales", "Profitability", "Tobin’s av. Q", "Sales growth"), after = 3) %>%  
  add_rows(hux("Country, time, and industry dummies", "Yes", "Yes", "Yes", "Yes", "Yes"), after = nrow(.) - 1) %>%  
  add_rows(hux("General controls", "No", "No", "Yes", "Yes", "Yes"), after = nrow(.) - 1) %>% 
  add_rows(hux("Noise controls", "No", "No", "Yes", "Yes", "Yes"), after = nrow(.) - 1)

huxtable::quick_xlsx(table1, file = "./figuretable/table1.xslx")

# Table 2
table2_data <- realdata %>% 
  rowwise() %>%
  dplyr::mutate(zcapital = mean(c(zlean1, zlean2, zperf2))) %>%
  dplyr::mutate(zhuman = mean(c(ztalent1, ztalent6, ztalent7))) %>%
  dplyr::mutate(zhuman_zcapital = zhuman-zcapital) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year))

table2_1 <- table2_data %>%
  lm_robust(zhuman ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
     data =., se_type = "stata")

table2_2 <- table2_data %>%
  lm_robust(zcapital ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
            data =., se_type = "stata")

table2_3 <- table2_data %>%
  lm_robust(zhuman_zcapital ~ ldegree + ldegreemiss + cceurope + ccgermany + ccus, 
            data =., se_type = "stata")

table2_4 <- table2_data %>%
  lm_robust(zhuman_zcapital ~ ldegree + ldegreemiss + lempm + lfirmage + public + cceurope + ccgermany + ccus + factor(sic3), 
            data =., se_type = "stata")

table2_5 <- table2_data %>%
  lm_robust(zhuman_zcapital ~ law + lempm + public + cceurope + ccgermany + ccus + factor(sic3), 
            data =., se_type = "stata")

### heteroveneity coefficients
sics <- generics::tidy(table2_5) %>% 
  bind_rows(tidy(table2_4)) %>%
  filter(str_detect(term, "sic")) %>% 
  pull(term)

### sic coefficients
ccs <- generics::tidy(table2_5) %>% 
  bind_rows(tidy(table2_4)) %>%
  dplyr::filter(str_detect(term, "cc")) %>% 
  pull(term)

### table2
huxtable::huxreg(table2_1, table2_2, table2_3, table2_4, table2_5,
                 omit_coefs = c("(Intercept)", "uncons", "ldegreemiss", "lempm", "lfirmage", "public", sics, ccs),
                 statistics = c('# observations' = 'nobs') )%>%
  add_rows(hux("Dependent variable", "Human capital management", 
               "Fixed capital management", 
               "Human capital - fixed capital management", 
               "Human capital - fixed capital management", 
               "Human capital - fixed capital management"), after = nrow(.) - 6) %>%  
  add_rows(hux("General controls", "No", "No", "No", "Yes", "Yes"), after = nrow(.) - 2) %>% 
  add_rows(hux("Industry controls", "No", "No", "No", "Yes", "Yes"), after = nrow(.) - 2)

# Table 3
table3_control <- c("lempm", "lfirmage", "public", "ldegree", "ldegreemiss", "mba", "mbamiss", "uncons", "ccfrance", "ccgermany", "ccuk", "factor(sic3)") 

## col1
### paste to model 1 formula
xvars_table3_col1 <- paste(c("lindiopen9599", "factor(cty)"), collapse = " + ")
model_table3_col1 <- paste("zmanagement", xvars_table3_col1, sep = " ~ ")
model_table3_col1 <- as.formula(model_table3_col1)

## regress 
table3_1 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col1, data =.,  clusters = oecdind_cty, se_type = "stata")

## col2
### paste to model 1 formula
xvars_table3_col2 <- paste(c("lindiopen9599", "factor(sic3)", table3_control, noise), collapse = " + ")
model_table3_col2 <- paste("zmanagement", xvars_table3_col2, sep = " ~ ")
model_table3_col2 <- as.formula(model_table3_col2)

## regress 
table3_2 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col2, data =.,  clusters = oecdind_cty, se_type = "stata")


## col3
### paste to model 2 formula
xvars_table3_col3 <- paste(c("lerner", "factor(cty)"), collapse = " + ")
model_table3_col3 <- paste("zmanagement", xvars_table3_col3, sep = " ~ ")
model_table3_col3 <- as.formula(model_table3_col3)

## regress 
table3_3 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col3, data =.,  clusters = csic3, se_type = "stata")

## col4
### paste to model 4 formula
xvars_table3_col4 <- paste(c("lerner", "factor(sic3)", table3_control, noise), collapse = " + ")
model_table3_col4 <- paste("zmanagement", xvars_table3_col4, sep = " ~ ")
model_table3_col4 <- as.formula(model_table3_col4)

## regress 
table3_4 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col4, data =.,  clusters = csic3, se_type = "stata")

## col5
### paste to model 2 formula
xvars_table3_col5 <- paste(c("competition", "competitionmiss", "factor(cty)"), collapse = " + ")
model_table3_col5 <- paste("zmanagement", xvars_table3_col5, sep = " ~ ")
model_table3_col5 <- as.formula(model_table3_col5)

## regress 
table3_5 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col5, data =.,  clusters = csic3, se_type = "stata")

## col6
### paste to model 6 formula
xvars_table3_col6 <- paste(c("competition", "competitionmiss", "factor(sic3)", table3_control, noise), collapse = " + ")
model_table3_col6 <- paste("zmanagement", xvars_table3_col6, sep = " ~ ")
model_table3_col6 <- as.formula(model_table3_col6)

## regress 
table3_6 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col6, data =.,  clusters = csic3, se_type = "stata")

## col7
### paste to model 2 formula
xvars_table3_col7 <- paste(c("lindiopen9599", "lerner", "competition", "competitionmiss", "factor(cty)"), collapse = " + ")
model_table3_col7 <- paste("zmanagement", xvars_table3_col7, sep = " ~ ")
model_table3_col7 <- as.formula(model_table3_col7)

## regress 
table3_7 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col7, data =.,  clusters = csic3, se_type = "stata")

## col8
### paste to model 8 formula
xvars_table3_col8 <- paste(c("lindiopen9599", "lerner", "competition", "competitionmiss", "factor(sic3)", table3_control, noise), collapse = " + ")
model_table3_col8 <- paste("zmanagement", xvars_table3_col8, sep = " ~ ")
model_table3_col8 <- as.formula(model_table3_col8)

## regress 
table3_8 <- realdata %>%
  dplyr::group_by(code) %>%
  dplyr::filter(year == max(year)) %>%
  estimatr::lm_robust(model_table3_col8, data =.,  clusters = csic3, se_type = "stata")

### heteroveneity coefficients
sics <- generics::tidy(table3_8) %>% 
  bind_rows(tidy(table3_1)) %>%
  bind_rows(tidy(table3_2)) %>%
  filter(str_detect(term, "sic")) %>% 
  pull(term)

### sic coefficients
ctys <- generics::tidy(table3_8) %>% 
  bind_rows(tidy(table3_1)) %>%
  bind_rows(tidy(table3_2)) %>%
  dplyr::filter(str_detect(term, "cty")) %>% 
  pull(term)

### table3
huxtable::huxreg(table3_1, table3_2, table3_3, table3_4, table3_5, table3_6, table3_7, table3_8, 
                 omit_coefs = c("(Intercept)", "competitionmiss", table3_control, noise, sics, ctys),
                 statistics = c('# observations' = 'nobs') ) %>%
  add_rows(hux("Estimation method ", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS", "OLS"), after = nrow(.) - 8) %>%  
  add_rows(hux("Dependent variable ", "Management z-score", "Management z-score", "Management z-score", "Management z-score", 
               "Management z-score", "Management z-score", "Management z-score", "Management z-score"), after = nrow(.) - 8) %>%  
  add_rows(hux("General controls", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"), after = nrow(.) - 2)
