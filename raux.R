library(tidyverse)

# set wd
data_long <- load("C:/Users/macie/OneDrive/Pulpit/EG_test/Spotify_long.RData")
# View(data)

data_long$country <- as.factor(data_long$country)

class(data_long)

library(foreign)
Panel <- data
coplot(valence ~ time|country, type="l", data=Panel) # Lines
coplot(valence ~ time|country, type="b", data=Panel) # Points and lines

summary(data_long)

library(car)
scatterplot(valence ~ time|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data_long)


data_aux <- data_long %>% filter(country == "us")
scatterplot(valence ~ time|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data_aux)
scatterplot(valence ~ time, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data_aux)
plot(data_aux$time,data_aux$valence, type = "l")
plot()


d <- ggplot(data_long, aes(x=time, y=valence, group=country, colour=country ) ) + 
  geom_line(size=2)
d


library(gplots)
Panel <- data_long
plotmeans(valence ~ country, main="Heterogeineity across countries", data=Panel)


library(plm)
fixed.dum <-lm(valence ~ temperature + sky +  factor(country) - 1, data=Panel)
summary(fixed.dum)

fit_plm <- plm(valence ~ temperature + sky , 
               data = Panel, 
               index = c("country", "time"), 
               model = "within", 
               effect = "twoways")
summary(fit_plm)


