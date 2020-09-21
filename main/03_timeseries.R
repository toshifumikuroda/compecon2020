# set up
rm(list = ls())
library(tidyverse)
library(scales)
# read csv
timeseries <- readr::read_csv("./data/03_timeseries.csv")

# Part 1
## plot GDP_nominal and GDP_real
figure_GDP_nominal_1 <- timeseries %>% 
  dplyr::mutate(GDP_real = GDP_real/1000) %>%
  dplyr::mutate(GDP_nominal = GDP_nominal/1000) %>%
  ggplot2::ggplot(aes(x = year)) +
  geom_line(aes(y = GDP_nominal, colour="blue"))+
  geom_line(aes(y = GDP_real, colour="red")) +
  xlab("Year") +
  ylab("GDP (JPY Trillion)") +
  scale_color_discrete(name = "GDP", labels = c("Nominal", "Real")) +
  theme_classic()
figure_GDP_nominal_1

## plot GDP_nominal and GDP_real
figure_GDP_nominal_2 <- timeseries %>% 
  dplyr::mutate(GDP_real = GDP_real/1000) %>%
  dplyr::mutate(GDP_nominal = GDP_nominal/1000) %>%
  tidyr::gather(key = "Type", value = "GDP", GDP_nominal, GDP_real) %>%
  ggplot2::ggplot(aes(x = year, y = GDP, colour = Type)) +
  geom_line() +
  xlab("Year") +
  ylab("GDP (JPY billion)") +
  theme_classic()
figure_GDP_nominal_2



## gen GDP_defrator = GDP_nominal / GDP_real
timeseries <- timeseries %>%
  dplyr::mutate(gdp_deflator = GDP_nominal / GDP_real * 100)

## plot GDP_defrator & CPI
figure_price <- timeseries %>% 
  tidyr::gather(key = "Type", value = "Price", CPI, gdp_deflator) %>%
  ggplot2::ggplot(aes(x = year, y = Price, colour = Type)) +
  geom_line() +
  xlab("Year") +
  ylab("Price") +
  theme_classic() +
  scale_color_discrete(name = "Price", labels = c("CPI", "GDP Deflator"))
figure_price

## plot Unemployment
figure_unemployment <- timeseries %>% 
  ggplot2::ggplot(aes(x = year, y = Unemployment_rate)) +
  geom_line() +
  xlab("Year") +
  ylab("Unemployment Rate") +
  theme_classic()
figure_unemployment

# Part2
## gen inflation rate of GDP def % CPI
timeseries <- timeseries %>%
  dplyr::mutate(inflation_rate_gdpdef = (gdp_deflator/lag(gdp_deflator)-1)) %>%
  dplyr::mutate(inflation_rate_cpi = (CPI/lag(CPI)-1))
  
## plot scatter of inflation on GDPgap
timeseries %>% ggplot2::ggplot(aes(x = GDP_gap,  y = inflation_rate_gdpdef)) +
  geom_point() +
  xlab("GDP Gap") +
  ylab("Inflation(GDP Deflator)") +
  theme_classic()

### geom_path(1) : GDP_def GDP_gap
timeseries %>% ggplot2::ggplot(aes(x = GDP_gap,  y = inflation_rate_gdpdef)) +
  geom_path() +
  xlab("GDP Gap") +
  ylab("Inflation(GDP Deflator)") +
  theme_classic()

### geom_path(2) : CPI GDP_gap
timeseries %>% ggplot2::ggplot(aes(x = GDP_gap,  y = inflation_rate_cpi)) +
  geom_path() +
  xlab("GDP Gap") +
  ylab("Inflation(CPI)") +
  theme_classic()

### geom_path(3) : GDP_def Unemplotment
timeseries %>% ggplot2::ggplot(aes(x = Unemployment_rate,  y = inflation_rate_gdpdef)) +
  geom_path() +
  xlab("Unemployment Rate") +
  ylab("Inflation(GDP Deflator)") +
  theme_classic()

### geom_path(4) : CPI Unemplotment
timeseries %>% ggplot2::ggplot(aes(x = Unemployment_rate,  y = inflation_rate_cpi)) +
  geom_path() +
  xlab("Unemployment Rate") +
  ylab("Inflation(CPI)") +
  theme_classic()

### geom_path(5) : for SNS
timeseries %>% dplyr::mutate(ages = format(as.integer(year/10),3)) %>%
  ggplot2::ggplot(aes(x = Unemployment_rate,  y = inflation_rate_cpi, colour = ages, shape= ages)) +
  geom_path() +
  geom_point() +
  xlab("Unemployment Rate") +
  ylab("Inflation(CPI)") +
  theme_classic() + 
  scale_color_manual(breaks = c("198", "199", "200", "201"),
                     values=c("red", "blue", "green", "purple"))

## regress inflation on GDPgap
phillips_ols_1 <- lm(inflation_rate_cpi ~ Unemployment_rate, data=timeseries)
phillips_ols_1$coefficients
phillips_ols_1$fitted.values
summary(phillips_ols_1)[["coefficients"]][, "t value"]

## geom_point with lm
timeseries %>% ggplot2::ggplot(aes(x = Unemployment_rate,  y = inflation_rate_cpi)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) +
  xlab("Unemployment Rate") +
  ylab("Inflation(CPI)") +
  theme_classic()

## geom_point with lm for SNS
timeseries %>% dplyr::mutate(ages = format(as.integer(year/10),3)) %>%
  ggplot2::ggplot(aes(x = Unemployment_rate,  y = inflation_rate_cpi, colour = ages, shape= ages)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, size = 0.5, linetype="dashed") +
  xlab("Unemployment Rate") +
  ylab("Inflation(CPI)") +
  theme_classic() + 
  scale_color_manual(breaks = c("198", "199", "200", "201"),
                     values=c("red", "blue", "green", "purple"))

# Part3 is data autocorrelated?
## Plot Residuals
timeseries$phillips_ols_1_residual <- NA
timeseries$phillips_ols_1_residual[which(!is.na(timeseries$inflation_rate_cpi))] <- phillips_ols_1$resid
timeseries %>% 
  ggplot2::ggplot(aes(x = year,  y = phillips_ols_1_residual)) +
  geom_path() +
  stat_smooth(method = "lm", se = FALSE, colour = "red", size = 1) +
  xlab("Year") +  ylab("OLS Residuals") +
  theme_classic()

## Is redisuals are white noize ?
timeseries %>% 
  dplyr::mutate(whitenoize=rnorm(n(), mean = 0, sd = summary(phillips_ols_1)$sigma)) %>%
  ggplot2::ggplot(aes(x = year,  y = whitenoize))+
  geom_path(linetype="dashed") +
  geom_path(aes(x = year,  y = phillips_ols_1_residual), colour = "red") +
  geom_line(aes(x = year, y = 0)) +
  xlab("Year") +  ylab("OLS Residuals") +
  theme_classic()

## autocorration
acf(na.omit(timeseries$phillips_ols_1_residual), lag.max = 10, plot = T)

# Part4 HAC standard error
library(sandwich)
library(lmtest)
phillips_nw_ols_1 <- sandwich::NeweyWest(phillips_ols_1, lag = 1, prewhite = F, adjust = T)
ttest_1 <- lmtest::coeftest(phillips_ols_1, vcov = phillips_nw_ols_1)

phillips_ols_2 <- lm(inflation_rate_gdpdef ~ Unemployment_rate, data=timeseries)
phillips_nw_ols_2 <- sandwich::NeweyWest(phillips_ols_2, lag = 1, prewhite = F, adjust = T)
ttest_2 <- lmtest::coeftest(phillips_ols_2, vcov = phillips_nw_ols_2)

phillips_ols_3 <- lm(inflation_rate_cpi ~ GDP_gap, data=timeseries)
phillips_nw_ols_3 <- sandwich::NeweyWest(phillips_ols_3, lag = 1, prewhite = F, adjust = T)
ttest_3<- lmtest::coeftest(phillips_ols_3, vcov = phillips_nw_ols_3)


phillips_ols_4 <- lm(inflation_rate_gdpdef ~ GDP_gap, data=timeseries)
phillips_nw_ols_4 <- sandwich::NeweyWest(phillips_ols_4, lag = 1, prewhite = F, adjust = T)
ttest_4 <- lmtest::coeftest(phillips_ols_4, vcov = phillips_nw_ols_4)

results <- as.data.frame(rbind(ttest_1, ttest_2, ttest_3, ttest_4)) %>%
  dplyr::filter(row_number() %% 2 == 0)
row.names(results) <- c("CPI on Unemployment", "GDP Deflator on Unemployment",
                     "CPI on GDP Gap", "GDP Deflator on GDP Gap")
results
