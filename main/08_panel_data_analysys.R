# Section Panel Data

## set up
library(AER)
data(Fatalities)

## data exprole
str(Fatalities)
head(Fatalities)
view(Fatalities)

## data management
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

## regress
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

## plot 1982
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")
abline(fatal1982_mod, lwd = 1.5)

## plot 1988
plot(x = Fatalities1988$beertax, 
     y = Fatalities1988$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")
abline(fatal1988_mod, lwd = 1.5)

## difference
### regression
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)
coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

### plot
plot(x = diff_beertax, 
     y = diff_fatal_rate, 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")
abline(fatal_diff_mod, lwd = 1.5)

# Section Fixed Effect
## heteroscedacity
# library(AER)
data("CPSSWEducation")
attach(CPSSWEducation)

## plot
labor_model <- lm(earnings ~ education)
plot(education,  earnings,  ylim = c(0, 150))
abline(labor_model, col = "steelblue", lwd = 2)
summary(labor_model)

## boxplot
boxplot(formula = earnings ~ education, 
        at = c(intersect(CPSSWEducation$education,CPSSWEducation$education)),
        add = TRUE, 
        border = "black")

## Homoskedasdicity and Heteroskedasticity 
coeftest(labor_model)

vcov_HC0 <- vcovHC(labor_model, type = "HC0") 
coeftest(labor_model, vcov. = vcov_HC0) 

vcov_HC1 <- vcovHC(labor_model, type = "HC1") 
coeftest(labor_model, vcov. = vcov_HC1) 

## Panel Data 
library(estimatr)
library(huxtable)

### generate additional variables
Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

### base model
fatalities_mod <- lm(fatal_rate ~ beertax, data = Fatalities)

### fixed effect
fatalities_mod_i <- lm(fatal_rate ~ beertax + state, data = Fatalities)

fatalities_mod_it <- lm(fatal_rate ~ beertax + state + year, data = Fatalities)

### fixed effect with Heteroskedasticity robust
fatalities_mod_hc <- lm_robust(fatal_rate ~ beertax + state + year, data = Fatalities)

### fixed effect with Heteroskedasticity and Autocorrelation robust
fatalities_mod_hac <- lm_robust(fatal_rate ~ beertax + state + year, data = Fatalities, clusters = state)

### additional variables with Heteroskedasticity and Autocorrelation robust
fatalities_mod_hc2 <- lm_robust(fatal_rate ~ beertax + state + year + drinkagec + punish + miles, 
                                 data = Fatalities)
fatalities_mod_hac2 <- lm_robust(fatal_rate ~ beertax + state + year + drinkagec + punish + miles, 
                                 data = Fatalities, clusters = state)

### show
huxreg(fatalities_mod, fatalities_mod_i, fatalities_mod_it, fatalities_mod_hc, fatalities_mod_hac, fatalities_mod_hc2, fatalities_mod_hac2, 
       statistics = c(N = "nobs", R2 = "r.squared")) %>%
        add_rows(hux("Robust Standard Error", "No", "No", "NO", "HC", "Cluster", "HC", "Cluster"),
                 after = nrow(.) - 3)

### state coefficients
state_coefs <- tidy(fatalities_mod_i) %>% 
        filter(str_detect(term, "state")) %>% 
        pull(term)

### year coefficients
year_coefs <- tidy(fatalities_mod_it) %>% 
        filter(str_detect(term, "year")) %>% 
        pull(term)

### elliinate state and year dummy
huxreg(fatalities_mod, fatalities_mod_i, fatalities_mod_it, fatalities_mod_hc, fatalities_mod_hac, fatalities_mod_hc2, fatalities_mod_hac2, 
       statistics = c(N = "nobs", R2 = "r.squared"),
       omit_coefs = c(state_coefs, year_coefs)) %>%
        add_rows(hux("State controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                 after = nrow(.) - 3) %>%
        add_rows(hux("Year controls", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
                 after = nrow(.) - 3) %>%
        add_rows(hux("Robust Standard Error", "No", "No", "NO", "HC", "Cluster", "HC", "Cluster"),
                 after = nrow(.) - 3)
