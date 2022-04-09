library(tidyverse)
library(tidyverse)
library(car)
library(foreign)
library(gplots)
library(plm)

# set wd
data_long <- load("C:/Users/macie/OneDrive/Pulpit/EG_test/Spotify_long.RData")
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

fixed.dum <-lm(valence ~ temperature + sky +  factor(country) + factor(time) - 1, data=Panel)
summary(fixed.dum)

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

# 1-12
# korelacje 
# OLS

library(gplots)
plotmeans(valence ~ country, main="Heterogeineity across countries", data=Panel)








