# Setup
## ライブラリ読み込み
library(dplyr)
library(tidyr)
library(ggplot2)
library(hdm)
library(AER)
library(xtable)
## glm net
# install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)

## 初期設定
rm(list = ls())
author <- "Toshi"
set.seed(12345)

# glm
## glmnetを用いて一段階推定をし、得られた予測値で二段階を推定します
## Data Set
data(AJR); 
data <- AJR

## glmnetは説明変数をmatrixとして与える必要があるのでxを作成します
ajr_iv_model_1 <- model.matrix( ~ 0 + logMort + Latitude + Latitude2 + Africa +  Asia + Namer + Samer, data)

## glmnetに与える非説明変数(操作変数)をvectorとして作成します
ajr_iv_model_iv <- data$Exprop

## データの分割数をnfoldで5に指定してクロスバリデーションします
fit_iv_1 = glmnet::cv.glmnet(x = ajr_iv_model_1, y = ajr_iv_model_iv, nfold = 5)

## 最小となるλの図示
plot(fit_iv_1)

## 推定結果を見やすくします
coef_names_fit_iv_1 <- coef(fit_iv_1, s = "lambda.min")
row.names(coef_names_fit_iv_1)[coef_names_fit_iv_1@i+1]

## Expropの予測値を作成します
prediction_ajr_iv_model_1 <- predict(fit_iv_1, newx = ajr_iv_model_1, s = "lambda.min")
colnames(prediction_ajr_iv_model_1) <- "prediction_ajr_iv_model_1"
data <- cbind(data, prediction_ajr_iv_model_1) #元のデータに突合します

## 2SLS
tsls_ajr_iv_model_1 <- lm(formula = "GDP ~ prediction_ajr_iv_model_1 + Latitude + Latitude2 + Africa +  Asia + Namer + Samer", data = data)
summary(tsls_ajr_iv_model_1)

# 1段階目を二次の交差項を入れたモデルとして推定してみます
## define quad
ajr_iv_model_2 <- model.matrix( ~ (logMort + Latitude + Africa +  Asia + Namer + Samer)^2, data)

## fit
fit_iv_2 = glmnet::cv.glmnet(x = ajr_iv_model_2, y = ajr_iv_model_iv, nfold = 5)

## plot fit
plot(fit_iv_2)

## coef
coef_names_fit_iv_2 <- coef(fit_iv_2, s = "lambda.min")
row.names(coef_names_fit_iv_2)[coef_names_fit_iv_2@i+1]

## fitted value
prediction_ajr_iv_model_2 <- predict(fit_iv_2, newx = ajr_iv_model_2, s = "lambda.min")
colnames(prediction_ajr_iv_model_2) <- "prediction_ajr_iv_model_2"
data <- cbind(data, prediction_ajr_iv_model_2)

## 2SLS
tsls_ajr_iv_model_2 <- lm(formula = "GDP ~ prediction_ajr_iv_model_2 + (Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2", data = data)
summary(tsls_ajr_iv_model_2)

# LASSOでは無く、Ridge回帰を利用する場合
## fit
fit_iv_2_ridge = glmnet::cv.glmnet(x = ajr_iv_model_2, y = ajr_iv_model_iv, nfold = 5, alpha = 0)

## plot fit
plot(fit_iv_2_ridge)

## coef
coef_names_fit_iv_2_ridge <- coef(fit_iv_2_ridge, s = "lambda.min")
row.names(coef_names_fit_iv_2_ridge)[coef_names_fit_iv_2_ridge@i+1]

## fitted value
prediction_ajr_iv_model_2_ridge <- predict(fit_iv_2_ridge, newx = ajr_iv_model_2, s = "lambda.min")
colnames(prediction_ajr_iv_model_2_ridge) <- "prediction_ajr_iv_model_2_ridge"
data <- cbind(data, prediction_ajr_iv_model_2_ridge)

## 2SLS
tsls_ajr_iv_model_2_ridge <- lm(formula = "GDP ~ prediction_ajr_iv_model_2_ridge + (Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2", data = data)
summary(tsls_ajr_iv_model_2_ridge)

# DML
## setup
## load Rscript for Double ML
setwd("./data/24_AJR/Econometrics Journal_updated_01_07_18/AJR/")
source("ML_Functions.R")  
source("Moment_Functions.R")  
setwd("../../../../")

## 説明変数x, xl, 操作変数z, 内生変数d、非説明変数yの定義
y  <- "GDP"
d  <- "Exprop"
z  <- "logMort"
x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"

## DMLコマンドに与える機械学習方法を指定します
RLasso       <- list(intercept = TRUE) # lassoに定数項ありモデルを指定します
arguments    <- list(RLasso=RLasso) # DMLにLassoを用いることを指定します
methods      <- c("RLasso") #同上
split        <- 100 # ブートストラップ回数を指定します

## Bootstrapping
r <- c()
for (k in 1:split) {
  dml <- DoubleML(data=data, y=y, d=d, z=z, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="IV", arguments=arguments, silent=TRUE, trim=NULL)
  r <- rbind(r, dml[1,])
}

## 表にまとめてfiguretableフォルダに保存します
result           <- matrix(0, 2, 1)
colnames(result) <- cbind(t(methods))
rownames(result) <- cbind("Coefficient", "se")

result[1,] <- quantile(r, probs=0.5)
result[2,] <- sd(r)/sqrt(length(r))
result_table <- round(result, digits = 2)

print(
  xtable(result_table, align=c("l", "r"), 
         caption=paste0("Estimated effect of institutions on output using LASSO and DNN made by ",author),
         format.args = list(big.mark = " ", decimal.mark = ",")),
  type = "html",
  file = "figuretable/26_LASSO_table5.html"
)
