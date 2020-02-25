# ##############################
# Sorry ARIMA, but I’m Going Bayesian | Stitch Fix Technology – Multithreaded.PDF
# bsts
#
# ##############################

if(!sum(installed.packages()[,1]=="lubridate")){install.packages("lubridate")}
if(!sum(installed.packages()[,1]=="bsts")){install.packages("bsts")}
if(!sum(installed.packages()[,1]=="ggplot2")){install.packages("ggplot2")}
if(!sum(installed.packages()[,1]=="reshape2")){install.packages("rehsape2")}

library(lubridate)
library(bsts)
library(ggplot2)
library(reshape2)

### Set up the model
data("AirPassengers")
Y <- window(AirPassengers, start=c(1949, 1), end=c(1959,12))
Y
y <- log10(Y)
y
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)  # monthly data (12 months)
bsts.model <- bsts(y, state.specification = ss, niter = 500, ping=0, seed=2016)
bsts.model

summary(AirPassengers)
View(AirPassengers)

### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, bsts.model)

### Extract the components
components <- cbind.data.frame(colMeans(bsts.model$state.contributions[-(1:burn),"trend",]), 
                               colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.12.1",]), as.Date(time(Y)))
names(components) <- c("Trend", "Seasonality", "Date") 
components <- melt(components, id="Date") 
names(components) <- c("Date", "Component", "Value")

### Plot
ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + 
  guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

### Predict 
# horizon = 48 (next 4 years)
bsts.pred <- predict(bsts.model, horizon = 48, burn = 1)
par(mfrow = c(1,2))
plot(bsts.model)
plot(bsts.pred)

# ---------------------------
# spike and slab regression
# ---------------------------

library(lubridate)
library(bsts)
library(ggplot2)
library(reshape2)

### Fit the model with regressors
data(iclaims)
ss <- AddLocalLinearTrend(list(), initial.claims$iclaimsNSA)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52) # weekly data (52 weeks)
bsts.reg <- bsts(iclaimsNSA ~ ., state.specification = ss, data = initial.claims, niter = 500, ping=0, seed=2016) 

### Get the number of burn-ins to discard
burn <- SuggestBurn(0.1, bsts.reg)

### Helper function to get the positive mean of a vector
PositiveMean <- function(b){
  b <- b[abs(b) > 0]
  if (length(b) > 0)
    return(mean(b)) 
  return(0)
}

### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(melt(apply(bsts.reg$coefficients[-(1:burn),], 2, PositiveMean))) 
coeff$Variable <- as.character(row.names(coeff))

ggplot(data=coeff, aes(x=Variable, y=value)) +
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + xlab("") + ylab("") + 
  ggtitle("Average coefficients")

### Predict 
# horizon = 48 (next 4 years)
bsts.pred <- predict(bsts.reg, horizon = 52, burn = 100)
par(mfrow = c(1,2))
plot(bsts.reg)
plot(bsts.pred)

# -----------------------
# gbdetail.csv
# -----------------------

setwd("/users/akuppam/documents/Hprog/R")
gbdetail <- read.csv("gbdetail2.csv")
str(gbdetail)
summary(gbdetail)

library(lubridate)
library(bsts)
library(ggplot2)
library(reshape2)

# installed "timeSeries" package (install.packages("timeSeries"))
library(timeSeries)

# plot vector of values in 'initial' datafile....
# [,] means include all columns and rows of data
# [,5] means include all rows of data and 5th column of data
# type 'l' means 'lines'
# type 'p' means 'points'
# type 'b' means both - 'lines and points'
plot(gbdetail[,19], type="l")
plot(gbdetail[,19], type="b", col="blue", lwd=1, 
     xlab="Time", ylab="Open Values", 
     main="nbcr", 
     ylim=c(0,0.5)
     )

### Set up the model
data("AirPassengers")
Y <- window(AirPassengers, start=c(1949, 1), end=c(1959,12))
y <- log10(Y)
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)

# -----
ss <- AddLocalLinearTrend(list(), gbdetail$nbcr)
ss <- AddSeasonal(ss, gbdetail$nbcr, nseasons = 12)

bsts.model <- bsts(gbdetail$nbcr, state.specification = ss, niter = 500, ping=0, seed=2016)
bsts.model

### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, bsts.model)

### Extract the components
components <- cbind.data.frame(colMeans(bsts.model$state.contributions[-(1:burn),"trend",]), 
                               colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.12.1",]), 
                               as.Date(time(gbdetail$nbcr)))
names(components) <- c("Trend", "Seasonality", "Date") 
components <- melt(components, id="Date") 
names(components) <- c("Date", "Component", "Value")

### Plot
ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + 
  guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# -----------------------
# rover.csv
# -----------------------

library(lubridate)
library(bsts)
library(ggplot2)
library(reshape2)

setwd("/users/akuppam/documents/Hprog/R")
rover <- read.csv("rover.csv")
str(rover)
summary(rover)

library(dplyr)
tbl_df(rover)

rover_nb <- rover %>%
  group_by(date) %>%
  summarise(rover_nb = sum(nb))

plot(rover_nb, type="b", col="blue", lwd=1, 
     xlab="Time", ylab="net bookings", 
     main="nb" 
     #ylim=c(0,0.5)
)

### Set up the model
summary(rover_nb)
write.csv(rover_nb, 'rover_nb.csv')
ss <- AddLocalLinearTrend(list(), rover_nb$rover_nb)
ss <- AddSeasonal(ss, rover_nb$rover_nb, nseasons = 12)

bsts.model <- bsts(rover_nb$rover_nb, state.specification = ss, niter = 500, ping=0, seed=2016)
bsts.model

### Get a suggested number of burn-ins
# 0.1 indicates the proportion of the MCMC run to discard as burn in
burn <- SuggestBurn(0.1, bsts.model)

### Extract the components
components <- cbind.data.frame(colMeans(bsts.model$state.contributions[-(1:burn),"trend",]), 
                               colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.12.1",]), 
                               as.Date(time(rover_nb$date)))
# as.Date(time(Y))

names(components) <- c("Trend", "Seasonality", "Date") 
components <- melt(components, id="Date")
names(components) <- c("Date", "Component", "Value")

### Plot
ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + 
  guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

### Predict 
# horizon = 48 (next 4 years)
bsts.pred <- predict(bsts.model, horizon = 48, burn = 100)
par(mfrow = c(1,2))
plot(bsts.model)
plot(bsts.pred)

# -----------------------------
# -----------------------------
# ses vs arima vs naive
# Seasonal and Trend decomposition using Loess Forecasting (STLF) model
# https://itnext.io/understanding-the-forecasting-algorithm-stlf-model-29d74b3a0336
# -----------------------------
# -----------------------------

### rover_nb.csv
### use 365 or 366 for daily data
setwd("/users/akuppam/documents/Hprog/R")
rover_nb <- read.csv("rover_nb.csv")
View(rover_nb)
rnb <- ts(rover_nb[,3], start=2016,freq=365)
View(rnb)
plot(rnb)
summary(rnb)
str(rnb)

# Exponential Smoothing using state space approach
# specify 'h' for no of time points you want to forecast (h = 730: 2 years)
if(!sum(installed.packages()[,1]=="forecast")){install.packages("forecast")}
library(forecast)

## STLF Exponential smoothing
st_ets <- stlf(ts(rover_nb[,3],frequency=365), method="ets", h=120)$mean
st_ets
write.csv(st_ets, "st_ets.csv")

## STLF Arima
st_arima <- stlf(ts(rover_nb[,3],frequency=365), method="arima", h=120)$mean
st_arima
write.csv(st_arima, "st_arima.csv")

## STLF Naive Method - whatever I did last year same week is what I'm going to do the same week this year
st_naive <- stlf(ts(rover_nb[,3],frequency=365), method="naive", h=120)$mean
st_naive
write.csv(st_naive, "st_naive.csv")

plot(st_ets, col='red')
plot(st_arima, col='blue')
plot(st_naive, col='green')

ts.plot(st_ets, st_arima, st_naive)

# ---
# combining 3 vectors into a DF
df1 <- c(rnb, st_ets)
View(df1)
df2 <- c(rnb, st_arima)
df3 <- c(rnb, st_naive)

df <- data.frame(x,y)
names(df) <- c(x_name,y_name)
print(df)

df <- data.frame(df1,df2,df3)
names(df) <- c("ets","arima","naive")
View(df)
length(df)
nrow(df)

# adding date as index
#starter_df <- data.frame(dates=seq(from=(as.POSIXct(strftime("2006-01-01 00:00"))),
#                                   length.out = 2920, 
#                                   by="3 hours"),
#                         data = rnorm(2920))

starter_df1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                   length.out = nrow(df), 
                                   by="days"),
                         data = df)

#plot(starter_df1$data.ets, starter_df1$data.arima, starter_df1$data.naive)

#library(ggplot2)
#ggplot(data = starter_df1, aes(dates, data.ets)) + geom_line() 
#ggplot + geom_line(data = starter_df1, aes(dates, data.ets))
#ggplot(data = starter_df1, aes(dates, data.ets)) + geom_line() 
#ggplot(data = starter_df1, aes(dates, data.arima)) + geom_line() 
#ggplot(data = starter_df1, aes(dates, data.naive)) + geom_line() 

#ggplot(data = starter_df1, aes(dates, data.ets)) + 
#  ggplot(data = starter_df1, aes(dates, data.arima)) + 
#  ggplot(data = starter_df1, aes(dates, data.naive)) +
#  geom_line() 
#ggplot(data = starter_df1, aes(dates, data.arima)) + geom_line() 
#ggplot(data = starter_df1, aes(dates, data.naive)) + geom_line() 

#DateTime = as.POSIXct('1/27/2017 6:49', format='%m/%d/%Y %H:%M') + 1:10*60
#AMK = c(17,17,15,17,17,17,17,16,16,19)
#SK = c(3,2,1,1,2,1,1,4,3,3)
#JR = c(11,13,14,13,13,10,13,14,10,11)

#df = data.frame(DateTime, AMK, SK, JR)

###

library(ggplot2)
library(dplyr)
library(tidyr)

dfplot <- starter_df1 %>% gather(key, value, -dates)

ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line()


# ---

## Auto.Arima
rnb_arima <- auto.arima(rover_nb[,3])
rnb_arima
rnb_arima_pred <- predict(rnb_arima, n.ahead=120)
rnb_arima_pred_f <- forecast(rnb_arima, h=120)
write.csv(rnb_arima_pred, "rnb_arima_pred.csv")
write.csv(rnb_arima_pred_f, "rnb_arima_pred_f.csv")

summary(st_arima)
summary(rnb_arima_pred$pred)
summary(rnb_arima_pred_f$fitted)

## STLM - apply Auto.Arima model to data
fit <- stlm(ts(rover_nb[,3],frequency=365), modelfunction=Arima, order=c(2,1,2))
fc <- forecast(fit, h=120)
plot(fc)
write.csv(fc, "rnb_arima_pred_fc.csv")


# -----------------
# decompose to plot diff parts of the data (actual, trend, seasonal, residual)
rnbd <- decompose(rnb)
plot(rnbd)
str(rnbd)

# ------
# https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/discussion/8095
# ------
require("forecast")
require("plyr")

library(forecast)
library(plyr)
##Exponential Smoothing using state space approach
ets.f <- dlply(rover_nb, function(x) stlf(ts(x[,3],frequency=365),method="ets",h=120)$mean)

##ets.f[[1]] would give 39 weeks of forecast for store 1_1 and so on

##Arima
arima.f <- dlply(train, "id", function(x) stlf(ts(x[,2],frequency=52),method="arima",h=39,stepwise=FALSE,approx=FALSE)$mean)

##Naive Method - whatever I did last year same week is what I'm going to do ##same week this year
naive.f <- dlply(train, "id", function(x) stlf(ts(x[,2],frequency=52),method="naive",h=39)$mean)

# --------------------
library(tseries)
# use the actual data here (not decomposed)
adf.test(rnb) # p-value < 0.05 indicates the TS is stationary
adf.test(diff(log(rnb)), alternative="stationary", k=0)
adf.test((log(rnb)), alternative="stationary", k=0)

# 
# kpss.test(rnb)

# Seasonal Differencing
# nsdiffs(rnb)  # number for seasonal differencing needed
#> 1
rnb_seasdiff <- diff(rnb, lag=frequency(rnb), differences=1)  # seasonal differencing
plot(rnb_seasdiff, type="l", main="Seasonally Differenced")  # almost stationary

# Make it stationary
ndiffs(rnb_seasdiff)  # number of differences need to make it stationary
#> 1 
stationaryTS <- diff(rnb_seasdiff, differences= 1)
plot(stationaryTS, type="l", main="Differenced and Stationary")  # appears to be stationary (d=1)

acf(stationaryTS)  # seems like it exceeds at 1 (q=1)
pacf(stationaryTS) # seems like it crosses zero after 5 (p=5)

# arima (p,d,q) (5,1,1)
st_arima <- stlf(ts(rover_nb[,3],frequency=365), method="arima", h=730)$mean

nbmodel <- auto.arima(stationaryTS)
nbmodel
predict(nbmodel, n.ahead=20)

lognbmodel <- auto.arima(diff(log(rnb)))
lognbmodel
predict(lognbmodel, n.ahead=20)

# ############################################################################
# Holt Winters method for Simple Expoential Smoothing for short-term forecasts
# SES - perfect for additive model, constant level, no seasonality
# ############################################################################

rnbAlphaBetaGamma <- HoltWinters(rnb)
rnbAlphaBeta <- HoltWinters(rnb, gamma = FALSE)
rnbAlpha <- HoltWinters(rnb, beta = FALSE, gamma = FALSE)

plot(rnbAlphaBetaGamma)  # additive model, incr/decr trend, with seasonality
plot(rnbAlphaBeta)       # additive model, incr/decr trend, no seasonality
plot(rnbAlpha)           # additive model, constant level, no seasonality

rnbAlphaBetaGamma
rnbAlphaBeta
rnbAlpha


acf(rnbAlpha$SSE, lag.max = 20)

library(stats)
rnbForecasts <- stats::HoltWinters(rnb)
rnbForecasts

# =======================
# rnbl (rover data by date, nb, listings)
# ======================

# ---
# ses vs arima vs naive

### use 365 or 366 for daily data
setwd("/users/akuppam/documents/Hprog/R")
rnbl <- read.csv("rnbl.csv")
View(rnbl)

rnblts <- ts(rnbl[,7], start=2016,freq=365)
View(rnblts)
plot(rnblts)

# Exponential Smoothing using state space approach
# specify 'h' for no of time points you want to forecast (h = 730: 2 years)
if(!sum(installed.packages()[,1]=="forecast")){install.packages("forecast")}
library(forecast)

# Exponential smoothing
st_ets <- stlf(ts(rnbl[,7],frequency=365), method="ets", h=730)$mean
st_ets

##Arima
st_arima <- stlf(ts(rnbl[,7],frequency=365), method="arima", h=730)$mean
st_arima

## Naive Method - whatever I did last year same week is what I'm going to do the same week this year
st_naive <- stlf(ts(rnbl[,7],frequency=365), method="naive", h=730)$mean
st_naive

plot(st_ets)
plot(st_arima)
plot(st_naive)

ts.plot(st_ets, st_arima, st_naive)


# -----------
# testing 9/28/2018
# -----------

data(AirPassengers)
summary(AirPassengers)
str(AirPassengers)

y <- log(AirPassengers)
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)
ss
model <- bsts(y, state.specification = ss, niter = 500)
pred <- predict(model, horizon = 12, burn = 100)
plot(pred)

# --------
# 9/29/2018

data(AirPassengers)
y <- log(AirPassengers)
ss <- AddLocalLinearTrend(list(), y)
trend.only <- bsts(y, ss, niter = 500)

ss <- AddSeasonal(ss, y, nseasons = 12)
trend.and.seasonal <- bsts(y, ss, niter = 500)

CompareBstsModels(list(trend = trend.only,
                       "trend and seasonal" = trend.and.seasonal))

CompareBstsModels(list(trend = trend.only,
                       "trend and seasonal" = trend.and.seasonal),
                  cutpoint = 100)








