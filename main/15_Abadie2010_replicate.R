# setup
rm(list = ls())

# ライブラリの読み込み
library(ggsci)
library(tidyverse)

# データ読み込み
tobacco <- readr::read_csv("./data/The_Tax_Burden_on_Tobacco__1970-2018.csv")
tobacco_long <- tobacco %>% dplyr::select(c("LocationAbbr", "LocationDesc",  "Year", "SubMeasureDesc", "Data_Value")) %>%
  tidyr::spread(., SubMeasureDesc,  Data_Value) %>%
  dplyr::filter(Year <= 2000)

names(tobacco_long) <- c("stateabbr", "state", "year", "average_cost_per_pack", "cigarette_consumption", "taxrate",
                         "tax_amount", "taxrevenue", "state_tax_per_pack")
# figure 1
other_state <- tobacco_long %>%
  dplyr::filter(stateabbr != "CA") %>%
  dplyr::select(year, cigarette_consumption) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize_all(mean) %>%
  dplyr::mutate(state = "others")

DD <- tobacco_long %>%
  dplyr::filter(stateabbr == "CA") %>%
  dplyr::bind_rows(., other_state)

DD %>% ggplot2::ggplot(aes(x = year, y = cigarette_consumption, group = state, colour = state)) +
  geom_line() + 
  geom_point() +
  theme(legend.position = "bottom") + 
  ylim(0,140) +
  geom_vline(xintercept = 1989) +
  geom_text(aes(x = 1983, y = 40,  label = "Proposition 99 →")) +
  theme_bw() + scale_color_lancet()

# figure 2
## join data

### GDP per capita
gdp_per_capita_1997 <- readr::read_csv("./data/bea/SAGDP2N__ALL_AREAS_1997_2019.csv")%>%
  tidyr::gather(year, value, starts_with("19"), starts_with("20")) %>%
  dplyr::mutate(value = value %>% as.numeric,
                date = paste0(year, "-01-01") %>% as.Date) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(LineCode == 1& Region != "NA") %>%
  dplyr::select(GeoName, year, value)

gdp_per_capita_1963  <- readr::read_csv("./data/bea/SAGDP2S__ALL_AREAS_1963_1997.csv") %>%
  tidyr::gather(year, value, starts_with("19"), starts_with("20")) %>%
  dplyr::mutate(value = value %>% as.numeric,
                date = paste0(year, "-01-01") %>% as.Date) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(LineCode == 1& Region != "NA") %>%
  dplyr::select(GeoName, year, value)

gdp_per_capita <- rbind(gdp_per_capita_1963, gdp_per_capita_1997) %>%
  dplyr::rename(state = GeoName) %>%
  dplyr::mutate(gdp_per_capita = value, year = as.numeric(year)) %>%
  dplyr::select(state, year, gdp_per_capita)

gdp_per_capita <- readr::read_csv("./data/CPI.csv") %>%
  dplyr::rename(year = Year, cpi = Annual) %>%
  dplyr::right_join(gdp_per_capita, by= c("year")) %>%
  dplyr::mutate(real_gdp_per_capita = gdp_per_capita/cpi)

rm(gdp_per_capita_1977, gdp_per_capita_1997)

### SEER Population
### yourth_population_rate_by_state.csv is too large to edit in R Studio Cloud Free. I edit it in local R.
### Prease see 15_seer_converter.R 
yourth_rate <- readr::read_csv("./data/yourth_population_rate_by_state.csv")
tobacco_long <- yourth_rate %>%
  dplyr::rename(stateabbr = state) %>%
  dplyr::select(stateabbr, year, yourth_rate) %>%
  dplyr::right_join(tobacco_long, by= c("stateabbr", "year")) 
  
### beer
### beer data is manually copied from 
beer <- readr::read_csv("./data/beer.csv")
tobacco_long <- beer %>%
  dplyr::mutate(state = State, year = Year, beer = Beer) %>%
  dplyr::select(state, year, beer) %>%
  dplyr::right_join(tobacco_long, by= c("state", "year")) 

# 

# (7) Proposition99の分析：集計による分析
## データの準備

### Common Trend Assumptionの為に分析から特定の州を外す
### タバコの税金が1988年以降50セント以上上がった州のリスト
### Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington
skip_state <- c(3,9,10,22,21,23,31,33,48)

### Cigarデータセットの読み込み
### skip_stateに含まれる州のデータを削除
Cigar <- Cigar %>%
  filter(!state %in% skip_state,
         year >= 70) %>%
  mutate(area = if_else(state == 5, "CA", "Rest of US"))

## 前後比較による分析
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  spread(state, sales)

## 前後比較のプロット
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = period,
             shape = state,
             linetype = state)) +
  geom_point(size = 2) +
  geom_line(aes(group = state)) +
  ylim(0, NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm")) +
  scale_x_discrete(name ="Period",limits=c("before","after"))


## タバコの売上のトレンドを示すプロット
Cigar %>%
  mutate(state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(year,state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = year,
             shape = state,
             linetype = state)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = 88, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm"))

# (8) DIDのためのデータを準備
## カリフォルニア州とその他という2グループのデータ
Cigar_did_sum <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, year, year_dummy, ca) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

## カリフォルニア州とその他の州という州ごとでのデータ
Cigar_did_data <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, ca, year, year_dummy, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

# (9) カリフォルニア州とその他というグループでの分析
## 2グループでのデータでの分析
Cigar_did_sum_reg <- Cigar_did_sum %>%
  lm(data = ., sales ~ ca + post + ca:post + year_dummy) %>%
  tidy() %>%
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))

## 2グループでのデータでの分析(log)
Cigar_did_sum_logreg <- Cigar_did_sum %>%
  lm(data = ., log(sales) ~ ca + post + ca:post + year_dummy) %>%
  tidy() %>%
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))

# (10) 州ごとのデータでの分析

## 州ごとのデータでの分析
Cigar_did_data_cluster <- Cigar_did_data %>%
  miceadds::lm.cluster(data = .,
                       sales ~ ca + state + post + ca:post + year_dummy,
                       cluster = "state") %>%
  summary()

## 結果の抽出
did_cluster_result <- Cigar_did_data_cluster[row.names(Cigar_did_data_cluster) == "ca:post",]
did_cluster_result

# (11) CausalImpactを利用した分析

## CigarデータをCausalImpact用に整形
### 目的変数としてカリフォルニア州の売上 だけ抜き出す
Y <- Cigar %>% filter(state == 5) %>% pull(sales)

### 共変量として他の州の売上を抜き出し整形
X_sales <- Cigar %>%
  filter(state != 5) %>%
  select(state, sales, year) %>%
  spread(state,sales)

### 介入が行われるデータを示す
pre_period <- c(1:NROW(X_sales))[X_sales$year < 88]
post_period <- c(1:NROW(X_sales))[X_sales$year >= 88]

### 目的変数と共変量をバインドする
CI_data <- cbind(Y,X_sales) %>% select(-year)

## CausalImpactによる分析
impact <- CausalImpact::CausalImpact(CI_data,
                                     pre.period = c(min(pre_period), max(pre_period)),
                                     post.period = c(min(post_period), max(post_period)))
## 結果のplot
plot(impact)
