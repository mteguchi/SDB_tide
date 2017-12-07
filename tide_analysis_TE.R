#turtle capture logistic regression
rm(list=ls())
library('tidyverse')
library('stats')

dat1 <- read_csv('T:/Katie Turtle Data/data/tide_data_all.csv')

inv.logit <- function(x) exp(x)/(1 + exp(x))

my.logit <- function(p) log(p/(1-p))

#make east and west wind direction, west= 1, east= 0, if the wd.time.caught = 0, it is in the east
dat1$wd_01 <- ifelse(dat1$wd.time.caught >  1.570796 & dat1$wd.time.caught < 4.71239, 1,0)

#powerplant variable, if <= 2010 == 0, if >= 2011 == 1, powerplant was there == 0
dat1$date.caught <- as.Date(dat1$date.caught, format = '%m/%d/%Y')
dat1$powpt_01 <- ifelse(dat1$date.caught < "2010-12-31", 0, 1)

# This doesnt work because it eliminates any rows where the watertemp isnt there when merged with dat
#for all NAs, we averaged the day before and day after at the same time to get the best estimate because
#NOAA didn't have the data for that day
wtmp <- read_csv('T:/Katie Turtle Data/data/Necessary Data/watertmp_data.csv')
wtmp$date.caught <- as.Date(wtmp$date.caught, format = "%m/%d/%Y")

# add two new variables - tidedif and tidedif.time
#tidedif = the change in tide height around the capture time
#tidedif.time = change in tide height per minute
dat <- merge(dat1, wtmp, by = 'date.caught', all = T) %>%
  mutate(., tidedif = after.tide.height - before.tide.height) %>%
  mutate(., tidedif.time = tidedif/(after.timedif + before.timedif)) %>%
  na.omit(.)

head(dat)
summary(dat)

# This uses a fxn that goes straigt to noaa but I'm still deciphering it
# rm(list=ls())
# install.packages('rnoaa')
# library('rnoaa')
#
# x <- buoy(dataset = 'ocean', buoyid = 9410170, year = 2006, length= T)
# y <- ncdc_stations(stationid = "9410170")

# first glm is a logistic regression with all variables plus some interactions:
fit <- glm(turtles_01 ~ height.caught + ws.time.caught +
                 airtmp.time.caught + tidedif.time +
                 enviro.dt + effort.ratio + doy+
                 wd_01 + powpt_01 + tidedif + wat.tmpC +
                 wat.tmpC*powpt_01 + doy *powpt_01,
               data = dat, family = 'binomial')
summary(fit)

# look at the Poisson regression but this probably doesn't make much sense.
fit.poisson <- glm(n_turtles ~ height.caught + ws.time.caught +
                      airtmp.time.caught + tidedif.time +
                      enviro.dt + effort.ratio + doy+
                      wd_01 + powpt_01 + tidedif + wat.tmpC +
                      wat.tmpC*powpt_01 + doy *powpt_01,
                    data = dat, family = 'poisson')

#looking at summary of fit.1.0, you can see that height.caught, enviro.dt, and effort.ratio suck in Pr values
fit1 <- update(fit, .~. - height.caught - doy - enviro.dt
                    - effort.ratio - wd_01- airtmp.time.caught)

summary(fit1)

fit2 <- inv.logit(predict(fit1))
plot(fit2, dat$turtles_01)

#is this something^^^^^ (the after tides are kinda correlated so that might be why)

library(car)

#you want residuals to be scattered around the midline, consistantly above and below
residualPlots(fit1)

#this is the best plot to see regression lines
marginalModelPlots(fit1)
#Identify outliers though a plot
influenceIndexPlot(fit1)
#identify outliers explicitly (gives studentized Residual, Hat, CookD values)
influencePlot(fit1)

#library('logistf')

# data with only the 2017 season:
dat_2017 <- filter(dat, date.caught > '2017-01-01')

# Because there was only one day of 0 capture, logistic regression
# would not be a good choice. Use Poisson regression here:
fit.1.2017Poi <- glm(n_turtles ~ height.caught + ws.time.caught +
                         airtmp.time.caught +
                         doy +
                         wd_01 + tidedif + wat.tmpC,
                       data = dat_2017, family = 'poisson')

# update to remove one variable at a time:
fit.2.2017Poi <- update(fit.1.2017Poi, .~. - doy)
fit.3.2017Poi <- update(fit.2.2017Poi, .~. - height.caught)
fit.4.2017Poi <- update(fit.3.2017Poi, .~. - wd_01)
fit.5.2017Poi <- update(fit.4.2017Poi, .~. - tidedif)
fit.6.2017Poi <- update(fit.5.2017Poi, .~. - wat.tmpC)

# Compare AICs to see which one is the best
AIC(fit.1.2017Poi, fit.2.2017Poi, fit.3.2017Poi,
    fit.4.2017Poi, fit.5.2017Poi, fit.6.2017Poi)

# look at the predictions:
height.caught.vec <- seq(from = -0.5, to = 1.5, by = 0.1)
predict.height <- fit.2.2017Poi[["coefficients"]][["(Intercept)"]] +
  fit.2.2017Poi[["coefficients"]][["height.caught"]] * height.caught.vec +
  fit.2.2017Poi[["coefficients"]][["ws.time.caught"]] * mean(dat_2017$ws.time.caught)+
  fit.2.2017Poi[["coefficients"]][["airtmp.time.caught"]] * mean(dat_2017$airtmp.time.caught)+
  fit.2.2017Poi[["coefficients"]][["wd_01"]] * mean(dat_2017$wd_01)+
  fit.2.2017Poi[["coefficients"]][["tidedif"]] * mean(dat_2017$tidedif)+
  fit.2.2017Poi[["coefficients"]][["wat.tmpC"]] * mean(dat_2017$wat.tmpC)

df.height.caught <- data.frame(xvals = height.caught.vec,
                               yvals = exp(predict.height))

fit.height.caught <- loessLine(dat_2017$height.caught, dat_2017$n_turtles)

# plot results:
p1 <- ggplot() +
  geom_point(data = dat_2017,
             aes(x = height.caught,
                 y = n_turtles),
             size = 5) +
  geom_line(data = df.height.caught,
            aes(x = xvals,
                y = yvals),
            color = 'red',
            size = 2.5)


summary(dat_2017adj) #p values are pretty high but standard error is low

fit.1.2017 <- glm(turtles_01 ~ height.caught + ws.time.caught +
                    airtmp.time.caught + tidedif.time +
                    enviro.dt + effort.ratio + doy +
                    tidedif + wat.tmpC,
                  data = dat_2017, family = 'binomial')
summary(fit.1.2017)


