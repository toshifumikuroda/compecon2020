rm(list = ls())
# import tidyverse
library(tidyverse)

# Create Data Folder
dir.create("data")

# download covid19 data
download.file("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv",
              "./data/COVID-19.csv")

# read covid19 data
covid_19 <- readr::read_csv("./data/COVID-19.csv")

# histgram
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  ggplot2::ggplot(aes(x=days)) + 
  geom_histogram(binwidth = 1,color = "black", fill = "lightgray") +
  theme_classic()

# histgram of female
covid_19 %>%  dplyr::filter(性別=="女性") %>% 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>% 
  ggplot2::ggplot(aes(x=days)) +   geom_histogram(binwidth = 1, color="black", fill="lightgray") +  theme_classic()

# histgram by sex
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  ggplot2::ggplot(aes(x=days, fill = 性別)) + 
  geom_histogram(binwidth = 1) +
  theme_classic()

# confirm 性別
covid_19 %>% dplyr::group_by(性別) %>%
  summarize()

# genretate sex columns
covid_19 %>% 
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  dplyr::mutate(sex = recode(性別, "不明" = "Unknown", "男性" = "♂", "女性" = "♀", "男児" = "♂")) %>%
  ggplot2::ggplot(aes(x=days, fill = sex)) + 
  geom_histogram(binwidth = 1) +
  theme_classic()

# confirm 年代
covid_19 %>% dplyr::group_by(年代) %>%
  summarize()

# scatter plot
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  dplyr::mutate(week = format(days, "%W")) %>%
  dplyr::mutate(ages = recode(年代, "0-10" = "0", "不明" = "Unknown", "100" = "99+", .missing = "NA")) %>%
  dplyr::group_by(ages,week) %>%
  dplyr::mutate(case = n()) %>%
  ggplot2::ggplot(aes(x=week, y=ages, size = case)) + 
  geom_point() +
  theme_classic()

# SNS appealing figure
covid_19 %>%
  tidyr::drop_na(確定日, 年代) %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  dplyr::mutate(week = format(days, "%W")) %>%
  dplyr::mutate(ages = recode(年代, "0-10" = "0", "不明" = "Unknown", "100" = "99+", .missing = "NA")) %>%
  dplyr::group_by(ages,week) %>%
  dplyr::mutate(case = n()) %>%
  ggplot2::ggplot(aes(x=week, y=ages, size = case, color = case)) + 
  geom_point() +
  theme_classic() +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  guides(size = FALSE)

# SNS appealing figure by prefecture
covid_19 %>%
  dplyr::rename(pref = "Residential Pref") %>%
  tidyr::drop_na(確定日, 年代) %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  dplyr::mutate(week = format(days, "%W")) %>%
  dplyr::mutate(ages = recode(年代, "0-10" = "0", "不明" = "Unknown", "100" = "99+", .missing = "NA")) %>%
  dplyr::group_by(ages,week) %>%
  dplyr::mutate(case = n()) %>%
  ggplot2::ggplot(aes(x=week, y=ages, size = case, color = case)) + 
  geom_point() +
  theme_classic() +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  guides(size = FALSE) +
  facet_wrap(~ pref, nrow = 5)

# colnames(covid_19) <- str_replace(colnames(covid_19), " " , "_")

# table
covid_19 %>%
  dplyr::mutate(days = as.Date(確定日, "%m/%d/%y")) %>%
  tidyr::drop_na(days) %>%
  dplyr::mutate(month = format(days, "%m")) %>%
  dplyr::mutate(ages = as.numeric(年代)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarize(mean = mean(ages),
                   median = median(ages, na.rm = TRUE))

# table ranking
covid_19 %>%
  dplyr::rename(pref = "Residential Pref") %>%
  dplyr::group_by(pref) %>%
  dplyr::summarize(case = n()) %>%
  dplyr::mutate(proportion = case/sum(case)) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::select(rank, proportion, pref)

# table ranking object
ranking_table <- covid_19 %>% 
  dplyr::rename(pref = "Residential Pref") %>%
  dplyr::group_by(pref) %>%
  dplyr::summarize(case = n()) %>%
  dplyr::mutate(proportion = round(case/sum(case), 3)) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::select(rank, proportion, pref)

# View object
View(ranking_table)

# format
ranking_table$proportion <- round(ranking_table$proportion, 3)
View(ranking_table)
