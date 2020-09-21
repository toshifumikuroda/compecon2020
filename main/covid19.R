# download covid19 data
download.file("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv",
              "./data/COVID-19.csv")
# read covid19 data
covid_19 <- read.csv("./data/COVID-19.csv")

# transform date string to daily data
infected <- as.Date(covid_19$確定日, "%m/%d/%y") 

# figure
hist(infected , breaks="days", freq=FALSE)

# by sex
covid_19_femail <- subset(covid_19, 性別=="女性")
infected_femails <- as.Date(covid_19_femail$確定日, "%m/%d/%y") 
hist(infected_femails , breaks="days", freq=TRUE)
