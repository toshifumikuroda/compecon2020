# setup
rm(list = ls())
## ggsciのインストール
#install.packages("ggsci")
library(ggsci)
library(tidyverse)
library(psych)


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
## GDP per capita
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
  dplyr::select(GeoName, year, value) %>%
  dplyr::filter(year != 1997)

gdp_per_capita <- rbind(gdp_per_capita_1963, gdp_per_capita_1997) %>%
  dplyr::rename(state = GeoName) %>%
  dplyr::mutate(gdp_per_capita = value, year = as.numeric(year)) %>%
  dplyr::select(state, year, gdp_per_capita)

gdp_per_capita <- readr::read_csv("./data/CPI.csv") %>%
  dplyr::rename(year = Year, cpi = Annual) %>%
  dplyr::right_join(gdp_per_capita, by= c("year")) %>%
  dplyr::mutate(real_gdp_per_capita = log(gdp_per_capita/cpi))

rm(gdp_per_capita_1963, gdp_per_capita_1997)

tobacco_long <- gdp_per_capita %>%
  dplyr::right_join(tobacco_long, by= c("state", "year")) 

### SEER Population
### yourth_population_rate_by_state.csv is too large to edit in R Studio Cloud Free. I edit it in local R.
### Prease see 15_seer_converter.R 
yourth_rate <- readr::read_csv("./data/yourth_population_rate_by_state.csv")

tobacco_long <- yourth_rate %>%
  dplyr::select(stateabbr, year, yourth_rate) %>%
  dplyr::right_join(tobacco_long, by= c("stateabbr", "year")) 

### beer
### beer data is manually copied from 
beer <- readr::read_csv("./data/beer.csv")
tobacco_long <- beer %>%
  dplyr::rename(state = State, year = Year, beer = Beer) %>%
  dplyr::select(state, year, beer) %>%
  dplyr::right_join(tobacco_long, by= c("state", "year")) 


## table 1
skip_state <- c("Massachusetts", "Arizona", "Oregon", "Florida", 
                "Alaska", "Hawaii", "Maryland", "Michigan", "New Jersey", "New York", "Washington",
                "District of Columbia")

### CA
table1_california_1 <- tobacco_long %>% 
  dplyr::filter(stateabbr == "CA") %>%
  dplyr::select(real_gdp_per_capita, yourth_rate, average_cost_per_pack, beer) %>%
  tidyr::drop_na() %>%
  dplyr::summarise_all(mean) %>% as.vector()

table1_california_2 <- tobacco_long %>% 
  dplyr::filter(stateabbr == "CA") %>%
  dplyr::filter(year %in% c(1988, 1980, 1975)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(mean = mean(cigarette_consumption)) %>%
  dplyr::mutate(varname = paste("Cigarette sales per year", year, sep=" "))
table1_california_2_1 <- table1_california_2$mean
names(table1_california_2_1) <- table1_california_2$varname
table1_california <- cbind(table1_california_1, t(table1_california_2_1))

### Control
table1_others_1 <- tobacco_long %>% 
  dplyr::filter(!state %in% c("California",skip_state)) %>%
  dplyr::select(real_gdp_per_capita, yourth_rate, average_cost_per_pack, beer) %>%
  tidyr::drop_na() %>%
  dplyr::summarise_all(mean) %>% as.vector()

table1_others_2 <- tobacco_long %>% 
  dplyr::filter(!state %in% c("California",skip_state)) %>%
  dplyr::filter(year %in% c(1988, 1980, 1975)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(mean = mean(cigarette_consumption)) %>%
  dplyr::mutate(varname = paste("Cigarette sales per year", year, sep=" "))

table1_others_2_1 <- table1_others_2$mean
names(table1_others_2_1) <- table1_others_2$varname
table1_others <- cbind(table1_others_1, t(table1_others_2_1))

## combine
table1_real <-rbind(table1_california, table1_others)


## synth
### unit num
tobacco_long_synth <- tobacco_long %>%
  dplyr::mutate(unit_num = as.numeric(factor(state))) %>%
  as.data.frame()

skip_state_num <- tobacco_long_synth %>% 
  filter(!state %in% c("California", skip_state)) %>% 
  select(unit_num) %>% unique() %>% as.matrix()

### synth
synth_data <- Synth::dataprep(foo = tobacco_long_synth,
                              predictors = c("real_gdp_per_capita", "yourth_rate", "average_cost_per_pack", "beer"),
                              dependent = "cigarette_consumption",
                              unit.variable = "unit_num",
                              time.variable = "year",
                              treatment.identifier = 5,
                              controls.identifier = skip_state_num,
                              time.optimize.ssr = c(1977:1990),
                              time.plot = c(1977:2000),
                              time.predictors.prior = c(1977:1989)
)
synth_out <- Synth::synth(synth_data)

weight <- as.data.frame(synth_out$solution.w)
weight <- cbind(unit_num = as.numeric(rownames(weight)), weight)
rownames(weight) <- 1:nrow(weight)

table2 <- tobacco_long_synth %>% 
  dplyr::filter(year == 1997) %>%
  dplyr::left_join(weight, by = "unit_num") %>%
  dplyr::select(state, w.weight)

## figure 2
Synth::path.plot(dataprep.res = synth_data,synth.res = synth_out)

## figure 3
Synth::gaps.plot(dataprep.res = synth_data,synth.res = synth_out)

## treatment effect
gaps<- synth_data$Y1plot-(synth_data$Y0plot%*%synth_out$solution.w)
gaps <- cbind(year = as.numeric(rownames(gaps)), gaps)
rownames(gaps) <- 1:nrow(gaps)
gaps <- as.data.frame(gaps)

gaps %>% dplyr::filter(year >= 1990) %>%
  dplyr::select("5") %>%
  dplyr::summarise_all(mean)
