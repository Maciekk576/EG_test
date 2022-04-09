library(tidyverse)
library(tidyverse)
##### pakiety #####

library(car)
library(foreign)
library(gplots)
library(plm)

##### import data ####

# set wd
load("C:/Users/macie/OneDrive/Pulpit/EG_test/Spotify_long.RData")
data_long$country <- as.factor(data_long$country)
# View(data)

##### Basic EDA ####

Panel <- data_long
coplot(valence ~ time|country, type="l", data=Panel) # Lines
summary(data_long)


data_aux <- data_long %>% filter(country == "us")
plot(data_aux$time, data_aux$valence, type = "l")
plot()

d <- ggplot(data_long, aes(x=time, y=valence, group=country, colour=country ) ) + 
  geom_line(size=2)
d

Panel <- data_long
plotmeans(valence ~ country, main="Heterogeineity across countries", data=Panel)

##### fixed 1-24 ####

fixed_only_country <- lm(valence ~ temperature + sky + factor(country) - 1, data=Panel)
fixed_only_time <- lm(valence ~ temperature + sky + factor(time) - 1, data=Panel)
fixed_twoway <-lm(valence ~ temperature + sky +  factor(country) + factor(time) - 1, data=Panel)
stargazer::stargazer(fixed_only_time)
stargazer::stargazer(fixed_only_country)
stargazer_twoway(fixed_twoway)

summary(fixed_only_country)
summary(fixed_only_time)
summary(fixed_twoway)

# fixed time effects w modelu one-way
time_coeffs <- as.numeric(fixed_only_time$coefficients[3:26])
time <- seq(1,24)
plot(time, time_coeffs, type = "l")

# fixed country effects w modelu one-way country only
country_coeffs <- as.numeric(fixed_only_country$coefficients[3:28])
time <- seq(1,26)
plot(time, country_coeffs, type = "l")

# fixed time effects w modelu two-way
time_coeffs <- as.numeric(fixed_twoway$coefficients[29:51])
time <- seq(1,23)
plot(time, time_coeffs, type = "l")



##### Modele z kwandartem "temperature" i "sky" ####

fixed_only_country_squares <- lm(valence ~ temperature + temperature^2 + sky + sky^2 + factor(country) - 1, data=Panel)
fixed_only_time_squares <- lm(valence ~ temperature + temperature^2 + sky + sky^2 + factor(time) - 1, data=Panel)
fixed_twoway_squares <-lm(valence ~ temperature + temperature^2 + sky + sky^2 +  factor(country) + factor(time) - 1, data=Panel)
stargazer::stargazer(fixed_only_time)
stargazer::stargazer(fixed_only_country)
stargazer::stargazer(fixed_twoway)

summary(fixed_only_country_squares)
summary(fixed_only_time_squares)
summary(fixed_twoway_squares)

# fixed time effects w modelu one-way
time_coeffs <- as.numeric(fixed_only_time$coefficients[3:26])
time <- seq(1,24)
plot(time, time_coeffs, type = "l")

# fixed country effects w modelu one-way country only
country_coeffs <- as.numeric(fixed_only_country$coefficients[3:28])
time <- seq(1,26)
plot(time, country_coeffs, type = "l")

# fixed time effects w modelu two-way
time_coeffs <- as.numeric(fixed_twoway$coefficients[29:51])
time <- seq(1,23)
plot(time, time_coeffs, type = "l")

# korelacje 
# OLS

###### merge danych ######

load("C:/Users/macie/OneDrive/Pulpit/EG_test/macro.rdata")
View(macro)

data2 <- merge(data_long, macro)#, by.x = "country", by.y = "country")
data2$month = data2$time + (data2$time> 12)*-12
data2$ind_prod <- as.numeric(data2$ind_prod)
data2$unemp <- as.numeric(data2$unemp)
data2$ret_sales <- as.numeric(data2$ret_sales)
# View(data2)
write.csv(data2, file = "data2.csv")

######### 1-12 ################

Panel = data2
fixed_only_time_squares <- lm(valence ~ temperature + temperature^2 + sky + sky^2 + factor(month) - 1, data=Panel)
fixed_twoway_squares <-lm(valence ~ temperature + temperature^2 + sky + sky^2 +  factor(country) + factor(month) - 1, data=Panel)

summary(fixed_only_time_squares)
summary(fixed_twoway_squares)


fixed_only_time_squares$coefficients[3:14]

# fixed time effects w modelu one-way
time_coeffs <- as.numeric(fixed_only_time_squares$coefficients[3:14])
time <- seq(1,12)
plot(time, time_coeffs, type = "l")

# fixed time effects w modelu two-way
time_coeffs <- as.numeric(fixed_twoway_squares$coefficients[28:38])
time <- seq(1,11)
plot(time, time_coeffs, type = "l")


##### OLS #####

ols <- lm(valence ~ sky + temperature, data = data2)
summary(ols)

#### final #####

# OLS

formula_basic <- valence ~ sky + temperature
formula_rich <- valence ~ sky + temperature + unemp + ret_sales + ind_prod

ols_basic <- lm(formula_basic, data2)
ols_rich <- lm(formula_rich, data2)
summary(ols_basic)
summary(ols_rich)

stargazer::stargazer(ols_basic, ols_rich)

fixed_only_country <- lm(valence ~ temperature + sky + factor(country) - 1, data=Panel)
fixed_only_time <- lm(valence ~ temperature + sky + factor(time) - 1, data=Panel)
fixed_twoway <-lm(valence ~ temperature + sky +  factor(country) + factor(time) - 1, data=Panel)
stargazer::stargazer(fixed_only_time, fixed_only_country, fixed_twoway)
stargazer::stargazer(fixed_only_country)
stargazer_twoway(fixed_twoway)

summary(fixed_only_country)
summary(fixed_only_time)
summary(fixed_twoway)


