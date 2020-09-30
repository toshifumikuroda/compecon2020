＃ 14 合成対照群法
install.packages("Synth") # 一度だけ実行すれば良い

# 変数の削除
rm(list=ls())

# ライブラリ読み込み
library(Synth)
library(tidyverse)
library(foreach)

# データダウンロード 1度だけ実行すれば良い
download.file("https://www.nstac.go.jp/SSDSE/data/2020/SSDSE-2020B.csv", "./data/SSDSE-2020B.csv")

## データ読み込み
names <-names(readr::read_csv("./data/SSDSE-2020B.csv",n_max = 0))
varname <- readr::read_csv("./data/SSDSE-2020B.csv",n_max = 1, locale = readr::locale(encoding = "SHIFT-JIS"))
SSDSE_2020B <-readr::read_csv("./data/SSDSE-2020B.csv",
                              skip =2, 
                              locale = readr::locale(encoding = "SHIFT-JIS"),
                              col_names = names)

## 変数確認
varname$L3221
# R11000	埼玉県
# R12000	千葉県
# R13000	東京都
# R14000	神奈川県

## データ確認
head(SSDSE_2020B)

# タバコデータ読み込み
tobacco_panel <- readr::read_csv("./data/tobacco_panel.csv")

# データ突合
SSDSE_2020B <- SSDSE_2020B %>% 
  dplyr::left_join(tobacco_panel, by = c("Year", "Code"))

# パラレルトレンドの検討
## 消費に占めるタバコの比率 & 東京都ダミーの作成
SSDSE_2020B <- SSDSE_2020B %>%
  tidyr::drop_na() %>%
  dplyr::mutate(tobacco_rate = tobacco / L3221) %>%
  dplyr::mutate(unit_num = as.numeric(factor(Code))) %>%
  as.data.frame()

## タバコ支出比率のプロット
SSDSE_2020B %>% 
  dplyr::filter(Prefecture %in% c("東京都", "千葉県", "埼玉県", "神奈川県")) %>%
  ggplot2::ggplot((aes(x = Year, y = tobacco_rate, colour = factor(unit_num)))) +
  geom_line()

## 死亡数
SSDSE_2020B %>% 
  dplyr::filter(Prefecture %in% c("東京都", "千葉県", "埼玉県", "神奈川県")) %>%
  ggplot2::ggplot((aes(x = Year, y = A4200, colour = factor(unit_num)))) +
  geom_line()

## 大学学生数
SSDSE_2020B %>% 
  dplyr::filter(Prefecture %in% c("東京都", "千葉県", "埼玉県", "神奈川県")) %>%
  ggplot2::ggplot((aes(x = Year, y = E6302, colour = factor(unit_num)))) +
  geom_line()

## 食料費（二人以上の世帯）
SSDSE_2020B %>% 
  dplyr::filter(Prefecture %in% c("東京都", "千葉県", "埼玉県", "神奈川県")) %>%
  ggplot2::ggplot((aes(x = Year, y = L322101, colour = factor(unit_num)))) +
  geom_line()

## DD分析
controls <- SSDSE_2020B %>% 
  dplyr::filter(Prefecture %in% c("千葉県", "埼玉県", "神奈川県")) %>%
  dplyr::group_by(Year) %>%
  dplyr::select(-Code, -Prefecture) %>%
  dplyr::summarize_all(mean) %>%
  dplyr::mutate(Code = "R9999") %>%
  dplyr::mutate(Prefecture = "DDcontrol")
DD <- rbind(SSDSE_2020B, controls)

## タバコ支出比率のプロット
DD %>% 
  dplyr::filter(Prefecture %in% c("東京都", "DDcontrol")) %>%
  ggplot2::ggplot((aes(x = Year, y = tobacco_rate, colour = factor(unit_num)))) +
  geom_line()


# 合成対照群の作成
synth_data <- Synth::dataprep(foo = SSDSE_2020B,
                       predictors = c("A4200", "E6302", "L322101"
                       ),
                       dependent = "tobacco_rate",
                       unit.variable = "unit_num",
                       time.variable = "Year",
                       treatment.identifier = 13,
                       controls.identifier = c(11,12,14),
                       time.optimize.ssr = c(2007:2017),
                       time.plot = c(2007:2017),
                       time.predictors.prior = c(2007:2016)
)

## plot in levels (treated and synthetic)
synth_out <- Synth::synth(synth_data)
## plot in levels (treated and synthetic)
Synth::path.plot(dataprep.res = synth_data, synth.res = synth_out)

synth_data <- Synth::dataprep(foo = SSDSE_2020B,
                       predictors = c("A110101", "A110102", "A420001", "A420002", "A9201", 
                                      "E4601","E4602", "E6302", "F3101", "F3102", "F3103", "F3104", 
                                      "L322101", "L322102", "L322103", "L322104", "L322105", "L322106", 
                                      "L322107", "L322108", "L322109", "L322110"
                       ),
                       dependent = "tobacco_rate",
                       unit.variable = "unit_num",
                       time.variable = "Year",
                       treatment.identifier = 13,
                       controls.identifier = c(c(1:12),c(14:47)),
                       time.optimize.ssr = c(2007:2017),
                       time.plot = c(2007:2017),
                       time.predictors.prior = c(2007:2016)
)
synth_out <- synth(synth_data)
Synth::path.plot(dataprep.res = synth_data, synth.res = synth_out)





