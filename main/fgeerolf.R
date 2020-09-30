pklist <- c("data.table", "tidyverse", "curl")
source("https://fgeerolf.com/code/load-packages.R")
setwd("./data/bea/")

## SAINC: Annual Personal Income by State -------

curl_download("https://apps.bea.gov/regional/zip/SAGDP.zip", 
              destfile = "SAGDP.zip",
              quiet = FALSE)

unzip("SAGDP.zip")

# SAGDP ---------

SAGDP <- tibble(filename = list.files(pattern = "_ALL_AREAS")) %>%
  mutate(data = map(filename, ~ fread(., fill = TRUE) %>%
                      gather(year, value, starts_with("19"), starts_with("20")) %>%
                      mutate(value = value %>% as.numeric,
                             date = paste0(year, "-01-01") %>% as.Date) %>%
                      select(-IndustryClassification, -year) %>%
                      filter(!is.na(value)))) %>%
  unnest

save(SAGDP, file = "SAGDP.RData")

# SAGDP_var ----------
names(SAGDP)
SAGDP_var <- SAGDP %>%
  mutate(year = date %>% year) %>%
  group_by(TableName, ComponentName, Description, Unit) %>%
  summarise(yearmin = min(year),
            yearmax = max(year))

save(SAGDP_var, file = "SAGDP_var.RData")

# files_ZIP --------

files_ZIP <- unzip("SAGDP.zip", list = TRUE) %>% 
  select(Name) %>% 
  unlist %>% 
  unname

unlink(files_ZIP)
unlink("SAGDP.zip")