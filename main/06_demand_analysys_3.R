# Setup
## Clear memory
rm(list=ls())

## Load library
library(tidyverse)
library(psych)
library(foreign)
library(estimatr)

# Load Data
giving <- foreign::read.dta(file = "./data/05_ols/AERtables1-5.dta")

# Table 1
## All
Table_1_all <- giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  psych::describe() %>%
  dplyr::select(mean, sd) %>%
  print(.,digits=3)

## Treatment
Table_1_treatment <- giving %>%
  dplyr::filter(treatment==1) %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  psych::describe() %>%
  dplyr::select(mean, sd) %>%
  print(.,digits=3)

## Control
Table_1_control <- giving %>%
  dplyr::filter(control==1) %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple) %>%
  psych::describe() %>%
  dplyr::select(mean, sd) %>%
  print(.,digits=3)

ttest <- giving %>%
  dplyr::select(MRM2, HPA, freq, years, dormant, female, couple, treatment) %>%
  dplyr::summarise_each(funs(t.test(.[treatment == 1], .[control == 1])$statistic), vars = MRM2:couple) %>%
  tidyr::gather(key, value) %>%
  dplyr::mutate("T statistics" = value) %>%
  dplyr::select("T statistics")

Table_1 <- cbind(Table_1_all, Table_1_treatment, Table_1_control, ttest)
colnames(Table1) <- paste(c("All","All","T","T","C","C",""), colnames(Table_1), sep="_") 
Table1

# Table 2
## Control
Table_2_control <- giving %>%
  dplyr::filter(treatment==0) %>%
  dplyr::select(gave, amount) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  psych::describe() %>%
  dplyr::select(n, mean, se) %>%
  print(.,digits=3)

## Treatment
Table_2_treatment <- giving %>%
  dplyr::filter(treatment==1) %>%
  dplyr::select(gave, amount) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  psych::describe() %>%
  dplyr::select(n, mean, se) %>%
  print(.,digits=3)

## match1
Table_2_match1 <- giving %>%
  dplyr::filter(ratio==1) %>%
  dplyr::select(gave, amount) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  psych::describe() %>%
  dplyr::select(n, mean, se) %>%
  print(.,digits=3)

## match2
Table_2_match2 <- giving %>%
  dplyr::filter(ratio==2) %>%
  dplyr::select(gave, amount) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  psych::describe() %>%
  dplyr::select(n, mean, se) %>%
  print(.,digits=3)

## match3
Table_2_match3 <- giving %>%
  dplyr::filter(ratio==3) %>%
  dplyr::select(gave, amount) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  psych::describe() %>%
  dplyr::select(n, mean, se) %>%
  print(.,digits=3)

##cbind
Table_2 <- cbind(Table_2_control, Table_2_treatment, 
                 Table_2_match1, Table_2_match2, Table_2_match3)
colnames(Table_2) <- paste(
  c("Control","Control","Control",
    "Treatment","Treatment","Treatment",
    "1:1","1:1","1:1",
    "1:2","1:2","1:2",
    "1:3","1:3","1:3")
  ,colnames(Table_2), sep="_") 
Table_2

ttest_2 <- giving %>%
  dplyr::select(gave, amount, treatment) %>%
  dplyr::mutate(doller_cond_giving = ifelse(gave==1, amount, NA)) %>%
  dplyr::summarise_each(funs(t.test(.[treatment == 1], .[treatment == 0])$statistic), vars = gave, amount,doller_cond_giving) %>%
  tidyr::gather(key, value) %>%
  dplyr::mutate("T statistics" = value) %>%
  dplyr::select("T statistics")
ttest_2

#Table3
giving %>%
  lm(formula = amount ~ treatment)

Table3_1 <- giving %>%
  lm(formula = amount ~ treatment)
summary(Table3_1)

Table3_2 <- giving %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_2)

Table3_3 <- giving %>%
  dplyr::filter(dormant == 1) %>%
  lm(formula = amount ~ treatment)
summary(Table3_3)

Table3_4 <- giving %>%
  dplyr::filter(dormant == 1) %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_4)

Table3_5 <- giving %>%
  dplyr::filter(dormant == 0) %>%
  lm(formula = amount ~ treatment)
summary(Table3_5)

Table3_6 <- giving %>%
  dplyr::filter(dormant == 0) %>%
  lm(formula = amount ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_6)

Table3_7 <- giving %>% 
  lm(formula = amountchange ~ treatment)
summary(Table3_7)

Table3_8 <- giving %>%
  lm(formula = amountchange ~ 
       treatment + treatment : ratio2 + treatment : ratio3 +
       treatment : size25 + treatment : size50 + treatment : size50 + treatment : size100 +
       treatment : askd2 + treatment : askd3)
summary(Table3_8)

library(huxtable)
huxreg(Table3_1, Table3_2, Table3_3, Table3_4, Table3_5, Table3_6, Table3_7, Table3_8,
       statistics = c(N = "nobs", R2 = "r.squared"))

library(jtools)
export_summs(Table3_1, Table3_2, scale = TRUE)

# 付加価値
## 価格の作成
linear_1 <- giving %>% 
  dplyr::mutate(price = 1/(1+ratio)) %>%
  lm(formula = amountchange ~ price)
summary(linear_1)

## 両側対数モデル
log_1 <- giving %>% 
  dplyr::mutate(price = 1/(1+ratio)) %>%
  dplyr::mutate(logprice = log(price)) %>%
  dplyr::mutate(logamount = log(1+amount)) %>%
  lm(formula = logamount ~ logprice)
summary(log_1)

## プロット
giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  ggplot2::ggplot(aes(x = amount, y = price)) +
  geom_point(color = "black", fill = "lightgray") + 
  stat_smooth(colour = "red", size = 1) + 
  theme_classic()

## プロット2
giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  dplyr::mutate(logprice = log(price)) %>%
  dplyr::mutate(logamount = log(1+amount)) %>%
  ggplot2::ggplot(aes(x = logamount, y = logprice)) +
  geom_point(color = "black", fill = "lightgray") + 
  stat_smooth(colour = "red", size = 1) + 
  theme_classic()


test <- giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  lm(formula = amount ~ price + price * red0 + price * redcty + price * nonlit + 
       price * cases + price * MRM2 + price * HPA + price * freq + 
       price * years + price * dormant + price * female + price * couple)
summary(test)

giving_edited <- giving %>%
  dplyr::mutate(price = 1/(1+ratio)) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(., red0, redcty, nonlit, cases) %>%
  dplyr::mutate(region = cur_group_id())
summary(giving_edited$region)

fixed_effect <- giving_edited %>%
  lm_robust(formula = amount ~ price,
            clusters = region)
summary(fixed_effect)   

giving_edited_aggregated <- giving_edited %>%
  dplyr::group_by(., red0, redcty, nonlit, cases) %>%
  dplyr::summarise(aggregate_amount = sum(amount), aggregate_price = mean(price)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(region = row_number())

aggregate <- giving_edited_aggregated %>%
lm(formula = aggregate_amount ~ aggregate_price)
summary(aggregate)   


fatalities_mod_hc <- lm_robust(fatal_rate ~ beertax + state + year, data = Fatalities)
