# Replicate JuhÁsz (2018) to understand DD analysis for import protection

# setup
## Clear memory
rm(list=ls())

## load library
library(tidyverse)
library(estimatr)
library(huxtable)

# load data
shortrun <- haven::read_dta(file = "./data/replication_files_20151730_update1/department_shortrun_panel.dta")

# Table1
## drop unbalanced data
shortrun_balanced <- shortrun %>%
  tidyr::drop_na(thspindles) %>%
  dplyr::group_by(department) %>%
  dplyr::add_tally()%>%
  dplyr::filter(n==2) %>%
  dplyr::ungroup()

## generate standardized dataframe
shortrun_balanced_standardize <- shortrun_balanced %>% 
  mutate_at(scale, .vars = c("thspindles", "lnshortestLo"))

## Col1
### define xvars
model_base <- c("lnshortestLo", "factor(department)", "factor(year)")

### paste to model 2 formula
xvars_col1 <- paste(c(model_base), collapse = " + ")
model_col1 <- paste("thspindles", xvars_col1, sep = " ~ ")
model_col1 <- as.formula(model_col1)

### regress
table1_1 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col1, data =.,  clusters = department, se_type = "stata")

#### standardize coef
table1_1_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col1, data =.,  clusters = department, se_type = "stata")

table1_1_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_1_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col1, data =.,  clusters = clustervar_id, se_type = "stata")
table1_1_2[["std.error"]][["lnshortestLo"]]

huxtable::huxreg(table1_1, table1_1_1, table1_1_2,
       omit_coefs = c("(Intercept)", fe_department, fe_year)
)

## Col2
### define xvars
model_col2 <- c("lnstreamflowX1812")

### paste to model 2 formula
xvars_col2 <- paste(c(model_base, model_col2), collapse = " + ")
model_col2 <- paste("thspindles", xvars_col2, sep = " ~ ")
model_col2 <- as.formula(model_col2)

### regress
table1_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col2, data =.,  clusters = department, se_type = "stata")

#### standardize coef
table1_2_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col2, data =.,  clusters = department, se_type = "stata")

table1_2_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_2_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col2, data =.,  clusters = clustervar_id, se_type = "stata")
table1_2_2[["std.error"]][["lnshortestLo"]]

## Col3
### define xvars
model_col3 <- c("lncoalX1812")

### paste to model 3 formula
xvars_col3 <- paste(c(model_base, model_col3), collapse = " + ")
model_col3 <- paste("thspindles", xvars_col3, sep = " ~ ")
model_col3 <- as.formula(model_col3)

### regress
table1_3 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col3, data =.,  clusters = department, se_type = "stata")

table1_3_temp <- shortrun_balanced %>%
  dplyr::filter(department != "Yvelines") %>%
  estimatr::lm_robust(model_col3, data =.,  clusters = department, se_type = "stata")


table1_3_temp2 <- shortrun_balanced %>%
  dplyr::filter(department != "Vienne") %>%
  estimatr::lm_robust(model_col3, data =.,  clusters = department, se_type = "stata")

huxtable::huxreg(table1_3,table1_3_temp, table1_3_temp2,
                 omit_coefs = c("(Intercept)", fe_department, fe_year),
                 statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
                 stars = NULL,
                 number_format = 2)




#### standardize coef
table1_3_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col3, data =.,  clusters = department, se_type = "stata")

table1_3_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_3_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col4, data =.,  clusters = clustervar_id, se_type = "stata")
table1_3_2[["std.error"]][["lnshortestLo"]]

## Col4
### define xvars
model_col4 <- c("lnma_urbanX1812")

### paste to model 4 formula
xvars_col4 <- paste(c(model_base, model_col4), collapse = " + ")
model_col4 <- paste("thspindles", xvars_col4, sep = " ~ ")
model_col4 <- as.formula(model_col4)

### regress
table1_4 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col4, data =.,  clusters = department, se_type = "stata")

#### standardize coef
table1_4_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col4, data =.,  clusters = department, se_type = "stata")

table1_4_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_4_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col4, data =.,  clusters = clustervar_id, se_type = "stata")
table1_4_2[["std.error"]][["lnshortestLo"]]

## Col5
### define xvars
model_col5 <- c("lnma_universityX1812")

### paste to model 5 formula
xvars_col5 <- paste(c(model_base, model_col5), collapse = " + ")
model_col5 <- paste("thspindles", xvars_col5, sep = " ~ ")
model_col5 <- as.formula(model_col5)

### regress
table1_5 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col5, data =.,  clusters = department, se_type = "stata")

#### standardize coef
table1_5_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col5, data =.,  clusters = department, se_type = "stata")

table1_5_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_5_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col5, data =.,  clusters = clustervar_id, se_type = "stata")
table1_5_2[["std.error"]][["lnshortestLo"]]

## Col6
### define xvars
model_col6 <- c("literacyX1812")

### paste to model 6 formula
xvars_col6 <- paste(c(model_base, model_col6), collapse = " + ")
model_col6 <- paste("thspindles", xvars_col6, sep = " ~ ")
model_col6 <- as.formula(model_col6)

### regress
table1_6 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col6, data =.,  clusters = department, se_type = "stata")

#### standardize coef
table1_6_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col6, data =.,  clusters = department, se_type = "stata")

table1_6_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_6_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col6, data =.,  clusters = clustervar_id, se_type = "stata")
table1_6_2[["std.error"]][["lnshortestLo"]]


## Col7
### define xvars
model_col7 <- c("lnstreamflowX1812", "lncoalX1812", "lnma_urbanX1812", 
                "lnma_universityX1812", "literacyX1812")

### paste to model 7 formula
xvars_col7 <- paste(c(model_base, model_col7), collapse = " + ")
model_col7 <- paste("thspindles", xvars_col7, sep = " ~ ")
model_col7 <- as.formula(model_col7)

### regress
table1_7 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col7, data =.,  clusters = department, se_type = "stata")

#### standardize coef
table1_7_1 <- shortrun_balanced_standardize %>%
  estimatr::lm_robust(model_col7, data =.,  clusters = department, se_type = "stata")

table1_7_1[["coefficients"]][["lnshortestLo"]]

#### alternative cluster
table1_7_2 <- shortrun_balanced %>%
  estimatr::lm_robust(model_col7, data =.,  clusters = clustervar_id, se_type = "stata")
table1_7_2[["std.error"]][["lnshortestLo"]]

### department fixed effect
fe_department <- generics::tidy(table1_1) %>% 
  dplyr::filter(str_detect(term, "department")) %>% 
  pull(term)
### time fixed effect
fe_year <- generics::tidy(table1_1) %>% 
  dplyr::filter(str_detect(term, "year")) %>% 
  pull(term)

## Table1
## coef names
coef_names <- c("Effective distance" = "lnshortestLo", 
                "Streams × 1812" = "lnstreamflowX1812",
                "Coal × 1812" = "lncoalX1812",
                "Market potential  × 1812" = "lnma_urbanX1812",
                "Knowledge access × 1812" = "lnma_universityX1812",
                "Literacy × 1812" = "literacyX1812")

## row of normalized vars
std_rows <- cbind(" ",
              format(table1_1_1[["coefficients"]][["lnshortestLo"]], digit = 2),
              format(table1_2_1[["coefficients"]][["lnshortestLo"]], digit = 2),
              format(table1_3_1[["coefficients"]][["lnshortestLo"]], digit = 2),
              format(table1_4_1[["coefficients"]][["lnshortestLo"]], digit = 2),
              format(table1_5_1[["coefficients"]][["lnshortestLo"]], digit = 2),
              format(table1_6_1[["coefficients"]][["lnshortestLo"]], digit = 2),
              format(table1_7_1[["coefficients"]][["lnshortestLo"]], digit = 2))

## plot table 1
huxtable::huxreg(table1_1, table1_2, table1_3, table1_4, table1_5, table1_6,  table1_7,
       coefs = coef_names,
       omit_coefs = c("(Intercept)", fe_department, fe_year),
       statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
       stars = NULL,
       number_format = 2) %>%
  add_rows(huxtable::hux(std_rows), 
           after = 2) %>%
  add_rows(huxtable::hux("Time fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  add_rows(huxtable::hux("Department fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  set_caption("Spindles per thousand inhabitants")

## with star★
huxtable::huxreg(table1_1, table1_2, table1_3, table1_4, table1_5, table1_6,  table1_7,
                 coefs = coef_names,
                 omit_coefs = c("(Intercept)", fe_department, fe_year),
                 statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
                 number_format = 2) %>%
  add_rows(huxtable::hux(std_rows), 
           after = 2) %>%
  add_rows(huxtable::hux("Time fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  add_rows(huxtable::hux("Department fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  set_caption("Spindles per thousand inhabitants")

# table3
pretreatment <- haven::read_dta(file = "./data/replication_files_20151730_update1/department_pretreatment_panel.dta")

## regress
table3_1 <- pretreatment %>%
  estimatr::lm_robust(model_col1, data =.,  clusters = department, se_type = "stata")
table3_2 <- pretreatment %>%
  estimatr::lm_robust(model_col4, data =.,  clusters = department, se_type = "stata")
table3_3 <- pretreatment %>%
  estimatr::lm_robust(model_col7, data =.,  clusters = department, se_type = "stata")

## column 4
model_table2_col4 <- paste("lnkl", xvars_col1, sep = " ~ ")
model_table2_col4 <- as.formula(model_table2_col4)
table3_4 <- shortrun_balanced %>%
  estimatr::lm_robust(model_table2_col4, data =.,  clusters = department, se_type = "stata")

## column 5
model_table2_col5 <- paste("proportion_mj", xvars_col1, sep = " ~ ")
model_table2_col5 <- as.formula(model_table2_col5)
table3_5 <- shortrun_balanced %>%
  estimatr::lm_robust(model_table2_col5, data =.,  clusters = department, se_type = "stata")

## column 6
model_table2_col6 <- paste("thwool", xvars_col1, sep = " ~ ")
model_table2_col6 <- as.formula(model_table2_col6)
table3_6 <- shortrun_balanced %>%
  estimatr::lm_robust(model_table2_col6, data =.,  clusters = department, se_type = "stata")

## column 7
model_table2_col7 <- paste("thtanning", xvars_col1, sep = " ~ ")
model_table2_col7 <- as.formula(model_table2_col7)
table3_7 <- shortrun_balanced %>%
  estimatr::lm_robust(model_table2_col7, data =.,  clusters = department, se_type = "stata")

coef_names_table3 <- c("Effective distance" = "lnshortestLo", 
                "Market potential  × 1812" = "lnma_urbanX1812",
                "Streams × 1812" = "lnstreamflowX1812",
                "Coal × 1812" = "lncoalX1812",
                "Knowledge access × 1812" = "lnma_universityX1812",
                "Literacy × 1812" = "literacyX1812")

## table3
huxtable::huxreg("Spind.(1)" = table3_1, "Spind.(2)" = table3_2, "Spind.(3)" = table3_3,
                 "K/L" = table3_4, "Mach." = table3_5, "Wool" = table3_6, "Leather" = table3_7,
                 coefs = coef_names_table3,
                 omit_coefs = c("(Intercept)", fe_department, fe_year),
                 statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
                 stars = NULL,
                 number_format = 2) %>%
  add_rows(huxtable::hux("Time fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  add_rows(huxtable::hux("Department fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  set_caption("Falsification Tests")

## table3 with star
huxtable::huxreg("Spind.(1)" = table3_1, "Spind.(2)" = table3_2, "Spind.(3)" = table3_3,
                 "K/L" = table3_4, "Mach." = table3_5, "Wool" = table3_6, "Leather" = table3_7,
                 coefs = coef_names_table3,
                 omit_coefs = c("(Intercept)", fe_department, fe_year),
                 statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
                 number_format = 2) %>%
  add_rows(huxtable::hux("Time fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  add_rows(huxtable::hux("Department fixed effects", "Yes", "Yes",  "Yes","Yes", "Yes", "Yes", "Yes"), 
           after = nrow(.) - 2) %>%
  set_caption("Falsification Tests")

# table4
longrun <- haven::read_dta(file = "./data/replication_files_20151730_update1/department_longrun.dta")

## columns 1
### define xvars
model_table4_base <- c("thspindles1812", "thspindles1803")

### paste
xvars_table4_col1 <- paste(c(model_table4_base), collapse = " + ")
model_table4_col1 <- paste("thspindles1840", xvars_table4_col1, sep = " ~ ")
model_table4_col1 <- as.formula(model_table4_col1)

### regress
table4_1 <- longrun %>%
  lm(model_table4_col1, data = .)

## columns 2
xvars_table4_control <- c("literacy", "lnma_urban", "lnma_university", "lncoal" ,"lnstreamflow")

### paste
xvars_table4_col2 <- paste(c(model_table4_base, xvars_table4_control), collapse = " + ")
model_table4_col2 <- paste("thspindles1840", xvars_table4_col2, sep = " ~ ")
model_table4_col2 <- as.formula(model_table4_col2)

### regress
table4_2 <- longrun %>%
  estimatr::lm_robust(model_table4_col2, data =.,  clusters = clustervar_id, se_type = "stata")

## columns 3
model_table4_col3 <- paste("thspindles1887", xvars_table4_col1, sep = " ~ ")
model_table4_col3 <- as.formula(model_table4_col3)

### regress
table4_3 <- longrun %>%
  lm(model_table4_col3, data = .)

## column4
model_table4_col4 <- paste("thspindles1887", xvars_table4_col2, sep = " ~ ")
model_table4_col4 <- as.formula(model_table4_col4)

### regress
table4_4 <- longrun %>%
  estimatr::lm_robust(model_table4_col4, data =.,  clusters = clustervar_id, se_type = "stata")

summary(table4_2)

coef_names_table4 <- c("Spindles, 1812" = "thspindles1812", "Spindles, 1803" = "thspindles1803",
                       "Lliteracy" = "literacy", "Market potential" = "lnma_urban", 
                       "Knowledge access" = "lnma_university", "Coal" = "lncoal", "Streams" = "lnstreamflow")

## table4
huxtable::huxreg("1840(1)" = table4_1, "1840(2)" = table4_2, "1887(3)" = table4_3, "1887(4)" = table4_4,
                 coefs = coef_names_table4,
                 omit_coefs = c("(Intercept)", fe_department, fe_year),
                 statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
                 stars = NULL,
                 number_format = 2) %>%
  set_caption("Spindles per thousand inhabitants")

# table6
## formula of columns 1 -4
lhs_table6 <- c('1860', '1896', '1930', '2000')
formula_df <- c()
for (year in lhs_table6) {
  lhs <- paste('lnindustrie',year,'pc', sep = '')
  model_table6 <- paste(lhs, xvars_table4_col1, sep = " ~ ")
  formula <- as.formula(model_table6)
  formula_df <- c(formula_df, formula)
}

## regress
table6_1 <- longrun %>%
  estimatr::lm_robust(formula_df[[1]], data =.,  clusters = clustervar_id, se_type = "stata")
table6_2 <- longrun %>%
  estimatr::lm_robust(formula_df[[2]], data =.,  clusters = clustervar_id, se_type = "stata")
table6_3 <- longrun %>%
  estimatr::lm_robust(formula_df[[3]], data =.,  clusters = clustervar_id, se_type = "stata")
table6_4 <- longrun %>%
  estimatr::lm_robust(formula_df[[4]], data =.,  clusters = clustervar_id, se_type = "stata")

## coef names
coef_names_table6 <- c("Spindles, 1812" = "thspindles1812", "Spindles, 1803" = "thspindles1803")
                       
## table 6
huxtable::huxreg("1860(1)" = table6_1, "1896(2)" = table6_2, "1930(3)" = table6_3, "2000(4)" = table6_4,
                 coefs = coef_names_table6,
                 omit_coefs = c("(Intercept)"),
                 statistics = c('Observations' = 'nobs', 'Adjusted R2'= 'adj.r.squared'),
                 stars = NULL,
                 number_format = 4) %>%
  set_caption("Natural logarithm of industrial value added per capita")
