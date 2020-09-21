# 初期設定
rm(list = ls())
library(tidyverse)
library(matlib) # need to install
library(foreach)  # need to install

# モデルパラメータの設定
case <- list()
case[[1]] <- c(10,2,-2,2) # demand x = 10 - 2p, supply x = -2 + 2p
case[[2]] <- c(6,0.5,-2,2) # demand x = 6 - 0.5p, supply x = -2 + 2p
case[[3]] <- c(10,2,-8,2)  # demand x = 10 - 2p, supply x = -8 + 2p
case[[4]] <- c(6,0.5,-8,2) # demand x = 6 - 0.5p, supply x = -8 + 2p

# 以下のシミュレーションでループして結果を保存するための準備
p <- list()
x <- list()
mod <- list()
shock_x <- list()
shock_y <- list()

# モンテカルロシミュレーション
try <- 100 # モデル毎のシミュレーション回数を設定
foreach(model=1:4) %do% { # モデル毎のループ
  foreach(i=1:try) %do% { # シミュレーション
    parameter <- case[[model]] # パラメータを読み込む
    p[[i+(model-1)*try]] <- (parameter[1]-parameter[3])/(parameter[2]+parameter[4]) # 均衡価格を計算する
    x[[i+(model-1)*try]] <- (parameter[1]*parameter[4] + parameter[3]*parameter[2])/(parameter[4]+parameter[2]) # 均衡数量を計算する
    mod[[i+(model-1)*try]] <- model # モデル番号を記録する
    shock_x[[i+(model-1)*try]]  <- rnorm(1 ,mean = 0, sd = 0.5) # 誤差項を正規分布からドローして需要に足す
    shock_y[[i+(model-1)*try]]  <- rnorm(1 ,mean = 0, sd = 0.5) # 誤差項を正規分布からドローして価格に足す
  }
}

# ループでリストに納めたデータをベクトルに戻す
p <- unlist(p, use.names = TRUE)
x <- unlist(x, use.names = FALSE)
mod <- unlist(mod, use.names = FALSE)
shock_x <- unlist(shock_x, use.names = TRUE)
shock_y <- unlist(shock_y, use.names = TRUE)

# 誤差とモデルの解を足し合わせて観察データを作る
x_obs <- x + shock_x
p_obs <- p + shock_y

# 各変数を一つのデータフレームに納める
observation <- data.frame(cbind(p,x, mod,shock_x, shock_y, x_obs, p_obs))

# 観察された価格と数量のプロットと近似曲線
observation %>%
  ggplot2::ggplot(aes(x = x_obs, y = p_obs)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm") +
  ggtitle("Plot of regression x on p") + 
  xlim(0,6) + ylim(0,7)

# b = 2の需要曲線
observation %>% filter(mod %in% c(1,3)) %>%
  ggplot2::ggplot(aes(x = x_obs, y = p_obs)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm") +
  ggtitle("Plot of Demand Function x = 10 - 2p") + 
  xlim(0,6) + ylim(0,7)

# b=0.5の需要曲線
observation %>% filter(mod %in% c(2,4)) %>%
  ggplot2::ggplot(aes(x = x_obs, y = p_obs)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm") +
  ggtitle("Plot of Demand Function  x = 6 - 0.5p") + 
  xlim(0,6) + ylim(0,7)

# b = -2, d = 2の供給曲線
observation %>% filter(mod %in% c(1,2)) %>%
  ggplot2::ggplot(aes(x = x_obs, y = p_obs)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm") +
  ggtitle("Plot of Supply Function  x = -2 + 2p") + 
  xlim(0,6) + ylim(0,7)

# b = -8, d = 2の供給曲線
observation %>% filter(mod %in% c(3,4)) %>%
  ggplot2::ggplot(aes(x = x_obs, y = p_obs)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm") +
  ggtitle("Plot of Supply Function  x = -8 + 2p") + 
  xlim(0,6) + ylim(0,7)
