# Setup
## ライブラリ読み込み
library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(xtable)
library(Matching)
library(gridExtra)

## 初期設定
rm(list = ls())
load("data/21_jensenius/AEJApp-2014-0201_Dataset/devDTA.Rdata")

## 変数名確認
names(devDTA)

# 観察数確認
table(devDTA$AC_type_1976) # 一般区(GEN)、特定カースト区(SC)、特定部族区(ST)
table(devDTA$AC_type_noST) # ST以外
devDTA %>% tidyr::drop_na(P_elecVD01) %>%
  dplyr::select(AC_type_noST) %>%
  table() # 欠落変数のないサンプルサイズを確認

# Table 1
outcomeindex<-c(35, 60, 52, 58, 64, 68, 72, 76, 63, 53, 59, 67, 71, 75, 79) # 結果変数の列番号
table_1 <- c() # 結果を入れる空オブジェクト
devDTAer <- devDTA %>%
  tidyr::drop_na(AC_type_noST)
for(i in outcomeindex) { 
  mymatrix <- devDTAer %>%
    dplyr::group_by(AC_type_noST) %>%
    tidyr::drop_na(colnames(devDTAer)[i]) %>%
    dplyr::summarise_at(colnames(devDTAer)[i], mean)
  row_12 <- t(mymatrix[2])
  myOLS <- estimatr::lm_robust(data = devDTAer, 
                               formula = as.formula(paste(c(toString(colnames(devDTAer)[i]),"~ AC_type_noST"), collapse="")), 
                             clusters = State_number_2001,
                             se_type = "stata")
  row_3 <- round(myOLS$coef[2],1)
  row_4 <- ifelse(myOLS[["p.value"]][2]<0.01, "<0.01", 	round(myOLS[["p.value"]][2],2))
  row <- cbind(row_12, row_3, row_4)
  table_1 <- rbind(table_1, row)
  }
colnames(table_1) <- c("Mean general", "Mean reserved", "Difference", "P-value")
row.names(table_1)<-c("Percentage of SCs", "Literacy rate", " Employment Rate", "Agricultural laborers", "Electricity in village", "School in village ","Medical facility in village","Comm. channel in village",
                       "Literacy gap", " Employment gap", "Agricultural laborers gap",   "Electricity in village gap", "School in village gap","Medical facility in village gap","Comm. channel in village gap")

## export figure 1 as html
print(
  xtable(table_1, align=c("l", "r", "r", "r", "r"), caption="Difference in general and SC-reserved constituencies in 2001"),
  format.args = list(big.mark = " ", decimal.mark = ","),
  type = "html",
  file = "figuretable/21_jensenius_table1.html")

# Figure 2
myplot <- devDTAer %>% 
  dplyr::group_by(AC_type_noST) %>%
  dplyr::summarise(
    Plit71_nonSC = mean(Plit71_nonSC),
    Plit_nonSC_7 = mean(Plit_nonSC_7),
    Plit71_SC = mean(Plit71_SC),
    Plit_SC_7 = mean(Plit_SC_7)
  ) %>% dplyr::ungroup() %>%
  tidyr::gather(key = variables, value = outcome, -AC_type_noST) %>%
  dplyr::mutate(year = ifelse(grepl("71", variables, fixed = TRUE), 1971, 2001)) %>%
  dplyr::mutate(type = ifelse(grepl("non", variables, fixed = TRUE), "Non-SC", "SC"))

ggplot2::ggplot(data =myplot) +
  geom_line(aes(x = year, y = outcome, color = AC_type_noST)) +
  facet_wrap(~ type, nrow = 1) +
  ggtitle("Change in Literacy Rates between 1971 and 2001") +
  ylab("Literacy Rate") +
  theme_classic()

# Figure 3 & table 2
## マッチング用データ作成
matchdta <- devDTA %>% 
  tidyr::drop_na(SC_percent71_true, State_no_2001_old, AC_type_noST, Plit71_SC, Plit_SC) %>% # 欠損値のあるサンプルをDrop
  dplyr::mutate(Tr = ifelse(AC_type_noST=="SC", 1, 0)) # Tr変数を作成
X_match <- matchdta %>%
  dplyr::select(State_no_2001_old, DELIM_district_no, PC_no_1976, SC_percent71_true) # マッチに使う変数だけを抽出
## マッチング
Matched_norep <- Matching::Match(Y=matchdta$Plit, Tr=matchdta$Tr, X=X_match, estimand="ATT", exact=c(TRUE, TRUE, TRUE, FALSE), replace=FALSE) # マッチ
Matched_norep_withcaliper <- Matching::Match(Y=matchdta$Plit, Tr=matchdta$Tr, X=X_match, estimand="ATT", exact=c(TRUE, TRUE, TRUE, FALSE), replace=FALSE, caliper=c(0,0,0,.5)) # caliper付きマッチ

## マッチ結果のバランスチェック
bal.out_norep <- MatchBalance(
  Tr ~ Pop_tot1971+ P_ST71 +Plit71_nonSC+Plit71_SC+ P_W71_nonSC + P_W71_SC +P_al71_nonSC+P_al71_SC, 
  match.out=Matched_norep, nboots=1000, data=matchdta)
bal.out_norep_withcaliper <- MatchBalance(
  Tr ~ Pop_tot1971+ P_ST71 +Plit71_nonSC+Plit71_SC+ P_W71_nonSC + P_W71_SC +P_al71_nonSC+P_al71_SC, 
  match.out=Matched_norep_withcaliper, nboots=1000, data=matchdta)

# 表2の作成
table2 <- c()
for(i in 1:length(bal.out_norep)) {
  row <- cbind(
  round(bal.out_norep$BeforeMatching[[i]]$tt$p.value,2),
  round(bal.out_norep$BeforeMatching[[i]]$ks$ks.boot.pvalue,2),
  round(bal.out_norep$AfterMatching[[i]]$tt$p.value,2),
  round(bal.out_norep$AfterMatching[[i]]$ks$ks.boot.pvalue,2),
  round(bal.out_norep_withcaliper$AfterMatching[[i]]$tt$p.value,2),
  round(bal.out_norep_withcaliper$AfterMatching[[i]]$ks$ks.boot.pvalue,2)
  )
  table2 <- rbind(table2, row)
}
colnames(table2) <- c("Before matching", "Before matching", "After matching", "After matching", "Matching(Caliper)", "Matching(Caliper)")
rownames(table2) <- c("Population size", "Percentage of STs", "Literacy rate (non-SCs)", "Literacy rate (SCs)", "Employment (non-SCs)", "Employment (SCs)", "Agricultural laborers (non-SCs)", "Agricultural laborers (SCs)")
table2 #表2の表示

## export figure 1 as html
print(
  xtable(table2, align=c("l", "r", "r", "r", "r", "r", "r"), 
         caption="Balance on a Selection of 1971 Variables"),
  format.args = list(big.mark = " ", decimal.mark = ","),
  type = "html",
  file = "figuretable/21_jensenius_table2.html")

## Figure 3
### match
treatedDTA <- matchdta %>% # SC州のデータ
  dplyr::slice(Matched_norep$index.treated)
controlDTA <- matchdta %>% # 一般州のデータ
  dplyr::slice(Matched_norep$index.control)
matched <- rbind(treatedDTA, controlDTA)	

### with caliper
treatedDTA_withcaliper <- matchdta %>% # SC州のデータ
  dplyr::slice(Matched_norep_withcaliper$index.treated)
controlDTA_withcaliper <- matchdta %>% # 一般州のデータ
  dplyr::slice(Matched_norep_withcaliper$index.control)
matched_withcaliper <- rbind(treatedDTA_withcaliper, controlDTA_withcaliper)		

## Plot Figure 3
fig3_before <- ggplot2::ggplot(data = devDTAer) +
  geom_density(aes(x = SC_percent71_true, color = AC_type_noST)) +
  ggtitle("Before matching") +
  xlab("Percentage of SCs in constituency") +
  xlim(0,60) +
  theme_classic()

fig3_match <- ggplot2::ggplot(data = matched) +
  geom_density(aes(x = SC_percent71_true, color = AC_type_noST)) +
  ggtitle("After matching") +
  xlab("Percentage of SCs in constituency") +
  xlim(0,60) +
  theme_classic()

fig3_caliper <- ggplot2::ggplot(data = matched_withcaliper) +
  geom_density(aes(x = SC_percent71_true, color = AC_type_noST)) +
  ggtitle("After matching with caliper") +
  xlab("Percentage of SCs in constituency") +
  xlim(0,60) +
  theme_classic()

## gridExtraを使って三枚の図をまとめて出力
gridExtra::grid.arrange(fig3_before, fig3_match, fig3_caliper, ncol = 3)

# Table 3
table_3 <- c()
for(i in outcomeindex[2:length(outcomeindex)]) { # 結果変数についてのループ
  row <- c()
  mathed_reg <- matched %>% # データの絞り込み
    tidyr::drop_na(c(colnames(matched)[i], AC_type_noST))
  OLS <- estimatr::lm_robust(data = mathed_reg, 
                               formula = as.formula(paste(c(toString(colnames(mathed_reg)[i]),"~ AC_type_noST"), collapse="")), 
                               clusters = State_no_2001_old,
                               se_type = "stata") #lm_robustを利用した回帰による差の検定
  row_1 <- round(OLS$coef[2],2) # 二群の差の平均値
  row_2 <- ifelse(OLS[["p.value"]][2]<0.01, "<0.01", 	round(OLS[["p.value"]][2],2)) # 検定統計量
  
  matched_withcaliper_reg <- matched_withcaliper %>% # データの絞り込み for with calieperでのマッチ
    tidyr::drop_na(c(colnames(outcomeindex)[i], AC_type_noST))
  OLS_withcaliper <- estimatr::lm_robust(data = matched_withcaliper_reg, 
                             formula = as.formula(paste(c(toString(colnames(mathed_reg)[i]),"~ AC_type_noST"), collapse="")), 
                             clusters = State_no_2001_old,
                             se_type = "stata")
  row_3 <- round(OLS_withcaliper$coef[2],2)
  row_4 <- ifelse(OLS_withcaliper[["p.value"]][2]<0.01, "<0.01", 	round(OLS_withcaliper[["p.value"]][2],2))
  
  OLS_with_reg <- estimatr::lm_robust(data = matched_withcaliper_reg, 
                                         formula = as.formula(paste(c(toString(colnames(mathed_reg)[i]),
                                                                      "~ AC_type_noST + SC_pop71_true"), collapse="")), 
                                         clusters = State_no_2001_old,
                                         se_type = "stata")
  row_5 <- round(OLS_with_reg$coef[2],2)
  row_6 <- ifelse(OLS_with_reg[["p.value"]][2]<0.01, "<0.01", 	round(OLS_with_reg[["p.value"]][2],2))
  row <- cbind(row_1, row_2, row_3, row_4, row_5, row_6)
  table_3 <- rbind(table_3, row)
}
row.names(table_3)<-c("Literacy rate ", "Employment rate ", "Agricultural laborers", 
                       "Electricity in village", "School in village ","Medical facility in village","Comm. channel in village", 
                       "Literacy gap", "Employment gap", "Agricultural laborers gap", 
                       "Electricity in village gap", "School in village gap","Medical facility in village gap","Comm. channel in village gap")
colnames(table_3)<-c("Difference", "P-value", "Difference", "P-value", "Difference", "P-value")

print(
  xtable(table_3, align=c("l", "r", "r", "r", "r", "r", "r"), 
         caption="Matching Estimates of Percentage Point Differences on 2001 Data between General and Reserved Constituencies"),
  format.args = list(big.mark = " ", decimal.mark = ","),
  type = "html",
  file = "figuretable/21_jensenius_table3.html")
