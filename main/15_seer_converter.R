library(dplyr)
#seer <- readr::read_csv("./data/seer.csv")
SEER <- readr::read_tsv("./us.1969_2018.19ages.txt", col_names = FALSE)

# 19 Age group data
# 00 = 0 years 01 = 1-4 years 02 = 5-9 years 03 = 10-14 years 04 = 15-19 years
# 5 = 20-24, 
# 17 = 80-84 years 18 = 85+ years
SEER$year <- as.numeric(substr(SEER, 1, 4))
SEER$state <- substr(SEER, 5, 6)
SEER$race <- as.numeric(substr(SEER, 14, 14))
SEER$age <- as.numeric(substr(SEER, 17, 18))
SEER$population <- as.numeric(substr(SEER, 19, 26))

seer_use <- SEER %>% dplyr::filter(year>=1970&year<=2000)
seer_use <- seer_use %>% dplyr::mutate(yourth = ifelse(age ==4 | age == 5, 1,0))

seer_use_yourth <- seer_use %>% dplyr::group_by(yourth, year, state) %>%
  summarize(agg_pop = sum(population)) %>%
  group_by(year, state) %>%
  summarize(yourth = yourth,
            yourth_rate = agg_pop/sum(agg_pop)) %>%
  dplyr::filter(yourth == 1)

# export
readr::write_csv(seer_use_yourth, "./data/yourth_population_rate_by_state.csv")

seer$SEER[1:1]
