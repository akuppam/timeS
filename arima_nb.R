# -----------------------------
# -----------------------------
# ses vs arima vs naive
# Seasonal and Trend decomposition using Loess Forecasting (STLF) model
# https://itnext.io/understanding-the-forecasting-algorithm-stlf-model-29d74b3a0336
# -----------------------------
# -----------------------------

### rnbl2agg.csv
### use 365 or 366 for daily data
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl2agg.csv")

#setwd("/users/akuppam/documents/Data/RoverData/")
#DF <- read.csv("SoEu-nonpaid.csv")
str(DF)
summary(DF)
View(DF)
plot(DF[,9])
plot(DF$date, DF[,9])

# colnames(DF) <- c("ds","region","marketing","visits","br","inq","gb","cb","y","ss","ts","listings")
# format should be what's in the data (as.Date will convert it to %Y-%m-%d that is reqd for prophet)
# DF$ds <- as.Date(DF$ds, format = "%y-%m-%d")
#View(DF)

rnb <- ts(DF[,9], start=2016,freq=365)
View(rnb)
plot(rnb)
summary(rnb)
str(rnb)

# -------------------------------------------
# -------------------------------------------
# create train and test data
train <- DF[1:800,]
test <- DF[801:nrow(DF),]
# -------------------------------------------
# Exponential Smoothing using state space approach
# specify 'h' for no of time points you want to forecast (h = 730: 2 years)
if(!sum(installed.packages()[,1]=="forecast")){install.packages("forecast")}
library(forecast)

## STLF Exponential smoothing
st_ets <- stlf(rnb, method="ets", h=120)$mean
st_ets
write.csv(st_ets, "1_st_ets.csv")

# -------------------------------------------
# -------------------------------------------
## STLF Arima
st_arima <- stlf(rnb, method="arima", h=120)$mean
st_arima
write.csv(st_arima, "2_st_arima.csv")

# -------------------------------------------
# -------------------------------------------
## STLF Naive Method - whatever I did last year same week is what I'm going to do the same week this year
st_naive <- stlf(rnb, method="naive", h=120)$mean
st_naive
write.csv(st_naive, "3_st_naive.csv")

plot(st_ets, col='red')
plot(st_arima, col='blue')
plot(st_naive, col='green')

ts.plot(st_ets, st_arima, st_naive)
# -------------------------------------------
# -------------------------------------------
## Auto.Arima
rnb_arima <- auto.arima(DF[,9])  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)

rnb_arima_pred <- predict(rnb_arima, n.ahead=120)
write.csv(rnb_arima_pred, "4_rnb_arima_pred.csv")
# -------------------------------------------
# -------------------------------------------
rnb_arima_pred_f <- forecast(rnb_arima, h=120)
write.csv(rnb_arima_pred_f, "5_rnb_arima_pred_f.csv")
# -------------------------------------------
# -------------------------------------------
## STLM - apply Auto.Arima model to data
## ***** MAKE SURE THE ORDER IS UPDATED FROM THE STEP ABOVE  ***** ##
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit <- stlm(rnb, modelfunction=Arima)
fc <- forecast(fit, h=120)
plot(fc)
write.csv(fc, "6_rnb_arima_pred_fc.csv")

# -------------------------------------------
# -------------------------------------------
# Plot forecasts from different methods
# combining 10 vectors into a DF
# df1,df2,df3,df4 - have only forecasts (n=120)
# df5, df6 - use ARIMA model to backcast as well ('fitted' = backcasts)
df1 <- c(rnb, st_ets)
df2 <- c(rnb, st_arima)
df3 <- c(rnb, st_naive)
df4 <- c(rnb, rnb_arima_pred$pred) # Using 'predict()' function: pred = forecasts (n=120)
df5 <- c(rnb, rnb_arima_pred_f$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)
df6 <- c(rnb, fc$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)
df7 <- c(rnb, fc$lower[,1]) # lower 80% bound of forecasts (n=120)
df8 <- c(rnb, fc$lower[,2])  # lower 95% bound of forecasts (n=120)
df9 <- c(rnb, fc$upper[,1])  # upper 80% bound of forecasts (n=120)
df10 <- c(rnb, fc$upper[,2])  # upper 95% bound of forecasts (n=120)

# -------------------------------------------
#df_all <- data.frame(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)
df_all <- data.frame(df1,df2,df3,df4,df5,df6)
#names(df_all) <- c("ets","arima","naive","auto.arima.p","auto.arima.f","arima.stlm",
#                   "arima.L80%","arima.L95%","arima.U80%","arima.U95%")
#View(df_all)
#nrow(df_all)

# adding date as index
library(ggplot2)
library(dplyr)
library(tidyr)

df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                    length.out = nrow(df_all), 
                                    by="days"),
                          data = df_all)
dfplot <- df_all1 %>% gather(key, value, -dates)
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line()

# -------------------------------------------
# -------------------------------------------
# Compute MAPES for df5, df6
# create another df
# create 3 vars - obs nb, df5 fitted, df6 fitted (n=998)
# compute 2 mapes (using auto.arima, stlm methods)

df_mape <- data.frame(rnb, rnb_arima_pred_f$fitted, fc$fitted)
mape1 <- mean(abs((as.numeric(df_mape$rnb) - as.numeric(df_mape$rnb_arima_pred_f.fitted))/as.numeric(df_mape$rnb)))
mape2 <- mean(abs((df_mape$rnb - df_mape$fc.fitted)/df_mape$rnb))
summary(df_mape)
str(df_mape)

# adding date as index
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------
# Plot backcasted values against observed
df_mape1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                 length.out = nrow(df_mape), 
                                 by="days"),
                       data = df_mape)

dfplot1 <- df_mape1 %>% gather(key, value, -dates)
ggplot(dfplot1, mapping = aes(x = dates, y = value, color = key) ) + geom_line()

# -------------------------------------------
# -------------------------------------------
# Compute MAPE for df6

df_mape <- data.frame(rnb, fit_arima_fc$fitted, fit_hw_fc$fitted)
mape_arima <- mean(abs((df_mape$rnb - df_mape$fit_arima_fc.fitted)/df_mape$rnb))
df_mape_hw <- df_mape[366:nrow(df_mape),]
mape_hw <- mean(abs((df_mape_hw$rnb - df_mape_hw$fit_hw_fc.fitted)/df_mape_hw$rnb))

# Plot backcasted values against observed
df_mape1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                 length.out = nrow(df_mape), 
                                 by="days"),
                       data = df_mape)

dfplot1 <- df_mape1 %>% gather(key, value, -dates)
ggplot(dfplot1, mapping = aes(x = dates, y = value, color = key) ) + geom_line()

# Plot backcasted values against observed
df_mape2 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                 length.out = nrow(df_mape_hw), 
                                 by="days"),
                       data = df_mape_hw)

dfplot2 <- df_mape2 %>% gather(key, value, -dates)
ggplot(dfplot2, mapping = aes(x = dates, y = value, color = key) ) + geom_line()

# ---------------------------------------
# -------------------------------------------
# to do 10/15/2018 tonight
# DO "df6" for ARIMA - use all data for modeling, forecast and backcast for mape
# DO "df1" for ETS - do a 80/20 train/test split, forecast and compute mape using 'test' data
# go thro' below code - see any of these shud be done first
# if not, write code to filter out fbu x rlt from 'rnbl2agg' and run thru all models
# see what the mapes look like
# save above plots
# FIGURE OUT WHAT'S GOING ON WITH SARIMA !!!

# ---------------------------------------
# ---------------------------------------
# sarima
# “State-space ARIMA” or “Several Seasonalities ARIMA”
# https://cran.r-project.org/web/packages/smooth/vignettes/ssarima.html
## Auto.SSArima
library(smooth)
library(Mcomp)

rnb_ssarima <- auto.ssarima(DF[,9])  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
rnb_ssarima
rnb_ssarima_pred <- predict(rnb_ssarima, n.ahead=120)
write.csv(rnb_ssarima_pred, "7_rnb_ssarima_pred.csv")

orders(rnb_ssarima)
plot(forecast(rnb_ssarima))
plot(forecast(rnb_ssarima, h=120))
plot(forecast(rnb_ssarima))

#rnb_ssarima1 <- auto.ssarima(DF[,9], h=120)  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
#rnb_ssarima1
#rnb_ssarima2 <- auto.ssarima(DF[,9], h=120, initial="backcasting")  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
#rnb_ssarima2
# -------------------------------------------
# -------------------------------------------
rnb_ssarima_pred_f <- forecast(rnb_ssarima, h=120)
write.csv(rnb_ssarima_pred_f, "8_rnb_ssarima_pred_f.csv")

#df4s <- c(rnb, rnb_ssarima_pred$forecast) # Using 'predict()' function: pred = forecasts (n=120)
#df4s <- c(rnb, rnb_ssarima_pred$fitted[,1]) # Using 'predict()' function: pred = forecasts (n=120)
df5s <- c(rnb, rnb_ssarima_pred_f$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)

# -------------------------------------------
df_alls <- data.frame(df5s)
#names(df_alls) <- c("auto.ssarima.p","auto.ssarima.f")

# adding date as index
library(ggplot2)
library(dplyr)
library(tidyr)

df_all1s <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(df_alls), 
                                by="days"),
                      data = df_alls)

dfplots <- df_all1s %>% gather(key, value, -dates)
ggplot(dfplots, mapping = aes(x = dates, y = value, color = key) ) + geom_line()

# -------------------------------------------
df_alls1 <- data.frame(rnb, rnb_ssarima_pred$fitted[,1])

df_all1s1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                 length.out = nrow(df_alls1), 
                                 by="days"),
                       data = df_alls1)

dfplots1 <- df_all1s1 %>% gather(key, value, -dates)
ggplot(dfplots1, mapping = aes(x = dates, y = value, color = key) ) + geom_line()

# -------------------------------------------
# to do 10/15/2018 tonight
# DO "df6" for ARIMA - use all data for modeling, forecast and backcast for mape
# DO "df1" for ETS - do a 80/20 train/test split, forecast and compute mape using 'test' data
# go thro' below code - see any of these shud be done first
# if not, write code to filter out fbu x rlt from 'rnbl2agg' and run thru all models
# see what the mapes look like
# save above plots
# FIGURE OUT WHAT'S GOING ON WITH SARIMA !!!


# decompose to plot diff parts of the data (actual, trend, seasonal, residual)
rnbd <- decompose(rnb)
plot(rnbd)
str(rnbd)
# ————————

# OTHER BASIC TS MODELS

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
# https://robjhyndman.com/hyndsight/estimation2/
# Rob Hyndman says.....ets() is more reliable tahn Holt-Winters()
# alpha - HW parameter
# beta - if set to false, it will do a SES
# gamma - if set to false, it will turn off seasonal part

rnbAlphaBetaGamma <- HoltWinters(rnb)
rnbAlphaBeta <- HoltWinters(rnb, gamma = FALSE)
rnbAlpha <- HoltWinters(rnb, beta = FALSE, gamma = FALSE)

plot(rnbAlphaBetaGamma)  # additive model, incr/decr trend, with seasonality
plot(rnbAlphaBeta)       # additive model, incr/decr trend, no seasonality
plot(rnbAlpha)           # additive model, constant level, no seasonality

rnbAlphaBetaGamma
rnbAlphaBeta
rnbAlpha

library(stats)
rnbForecasts <- stats::HoltWinters(rnb)

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

