# ##########################
# test seasonality
# arima & sarima
# ##########################

# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/R/Prophet/")
DFagg <- read.csv("rnb1015_2.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v2m/AMR_paid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'NotPaid'))),]
summary(DF)

h = 442
# -------------------
# -------------------
# SES, ARIMA, HW
# -------------------
# -------------------
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
# -------------------------------------------
# Make the 'y' variables to a ts object
rnb <- ts(DF$nb, start=2016,freq=365)
str(rnb)
# -------------------------------------------
# Exponential Smoothing using state space approach
## STLF Exponential smoothing
library(forecast)
st_ets <- stlf(rnb, method="ets", h=h)$mean
write.csv(st_ets, "1_st_ets.csv")
# -------------------------------------------
# -------------------------------------------
# HoltWinters
rnbAlphaBetaGamma <- HoltWinters(rnb, seasonal = "mult")
fit_hw_fc <- forecast(rnbAlphaBetaGamma, h=442)
write.csv(fit_hw_fc, "11_fit_hw.csv")

df_hw <- c(fit_hw_fc$fitted, fit_hw_fc$mean)

# -------------------------------------------
# -------------------------------------------
## Auto.Arima
rnb_arima <- auto.arima(DF[,9])  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit_arima_fc <- forecast(fit, h=h)
write.csv(fit_arima_fc, "TEST_stlm.csv")
plot(fit_arima_fc)
# -------------------------------------------
## Auto.Arima - TEST NO STLM
rnb_arima <- auto.arima(DF[,9])  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
#arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
#fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit_arima_fc <- forecast(rnb_arima, h=h)
write.csv(fit_arima_fc, "TEST_no_stlm.csv")
plot(fit_arima_fc)
# -------------------------------------------
# Compute MAPES
df_mape <- data.frame(rnb, fit_arima_fc$fitted, fit_hw_fc$fitted)
mape_arima <- mean(abs((df_mape$rnb - df_mape$fit_arima_fc.fitted)/df_mape$rnb))
mape_arima
# -------------------------------------------
# -------------------------------------------
library(astsa)  # applied statistical time series analysis
nb <- ts(DF[,9])  # same as: rnb <- ts(DF$nb, start=2016,freq=365)
plot(nb, type="b")
plot(DF[,9])
str(nb)
str(rnb)

diff12 = diff(nb,12)
plot(diff12)
acf2(diff12, 48)  # 48 is the no of lags you are plotting on acf, pacf charts
diff1and12 = diff(diff12, 1)

a <- auto.arima(DF[,9], seasonal = FALSE)
sa <- auto.arima(DF[,9], seasonal = TRUE)
sar <- auto.arima(DF[,9], seasonal = TRUE, test = "kpss", seasonal.test = "ocsb")
sa1 <- auto.arima(diff12, seasonal = TRUE)
sa2 <- auto.arima(DF[,9], stepwise=FALSE, seasonal=TRUE,approximation=FALSE,trace=TRUE)
sa3 <- auto.arima(DF[,9], stepwise=TRUE, seasonal=TRUE,approximation=FALSE,trace=TRUE)
sa3$arma

sa4 <- Arima(DF[,9],order=c(2,0,0),seasonal=c(2,1,1))
sa4$arma
arimaorder(sa4)
arimaorder(sa)

sar1 <- auto.arima(DF[,9], seasonal = TRUE, test = "kpss", seasonal.test = "ocsb", max.P = 5, max.Q = 5, max.D = 1)
sar1
arimaorder(sar1)
sar1$arma

# -----------------
# fit <- StructTS(trees, type = "level")
# Make the 'y' variables to a ts object
# freq should be >=2; freq=7, 30, 52 run within a minute
# freq = 365 took a very long time (few hours)
# freq = 120 takes a long time too
rnb <- ts(DF$nb, start=2016,freq=365)
plot(rnb)
sts <- StructTS(rnb, type = "BSM")
#par(mfrow = c(4, 1)) # to give appropriate aspect ratio for next plot.
#plot(rnb)
#plot(cbind(fitted(sts), resids=resid(sts)), main = "rnb_freq_365")
rnb_fit_resid <- cbind(fitted(sts), resids=resid(sts))
write.csv(rnb_fit_resid,"rnb_fit_resid.csv")

# ---
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
# ---
sts_forecasts <- forecast(sts, h = 442)
plot(forecast(sts, h = 442))
write.csv(sts_forecasts,"sts_forecasts.csv")
# -----

setwd("/users/akuppam/documents/Hprog/R/Prophet/")
UKgas <- read.csv("UKgas.csv")
UKgas <- ts(UKgas$value, start=1960,freq=4)
fit <- StructTS(log10(UKgas), type = "BSM")
par(mfrow = c(4, 1)) # to give appropriate aspect ratio for next plot.
plot(log10(UKgas))
plot(cbind(fitted(fit), resids=resid(fit)), main = "UK gas consumption")

# -------
# 12/06/2018
# fxn for arima

# df1 should ALWAYS be the raw data that is NOT a ts object
library(forecast)
forecast_func <- function(df1) {
  a_fit <- auto.arima(df1)
  a_forecast <- forecast(a_fit, h = 8)
  a_accuracy <- accuracy(a_forecast)
}

# data frame that stores the forecasts
# df2 is the data frame that the user supplies to the function
fores_output = forecast_func("df2")

# --------
# --------
# --------
h = 442
start = 2016
freq = 365
library(forecast)
forecast_func <- function(df1) {
  model_order <- auto.arima(df1)
  df1_ts <- ts(df1, start=start, freq=freq)
  model_fit <- stlm(df1_ts, modelfunction=Arima, order=arimaorder(model_order))
  df1_forecast <- forecast(model_fit, h=h)
  #a_accuracy <- accuracy(a_forecast)
}

fores_output = forecast_func(DF[,9])
df_all <- c(DF[,9], fores_output$mean)
plot(df_all)
# --------
# --------
# sarima Fxn in R
# -----
h = 442
start = 2016
freq = 365
library(forecast)
forecast_func <- function(df1) {
  df1_ts <- ts(df1, start=start, freq=freq)
  model_sorder <- auto.arima(df1_ts, D=1) # takes in a ts(object) as input data and outputs a sarima model
  
  asorder <- model_sorder$arma[c(1, 6, 2, 3, 7, 4, 5)]
  names(asorder) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
  model_fit_s <- stlm(df1_ts, modelfunction=Arima,
                      order=c(asorder[1],asorder[2],asorder[3]),
                      seasonal=list(order=c(asorder[4],asorder[5],asorder[6])))
  
  df1_forecast_s <- forecast(model_fit_s, h=h)
}

fores_output = forecast_func(DF[,9])
df_all_s <- c(DF[,9], fores_output$mean)
plot(df_all_s)
# --------
# --------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# the following works fine - creates arima, sarima, takes the order, sorder, forecasts, creates data frames, and plots
# --------------------------------------------------------------------------------------------------------------------
rawdata <- DF[,9]
# --------
tsdata <- rnb <- ts(rawdata, start=start, freq=freq)
# --------
model_order <- auto.arima(rawdata)
# --------
model_order
# --------
arimaorder(model_order)
# --------
model_sorder <- auto.arima(tsdata, D=1)
# --------
model_sorder
# --------
arimaorder(model_sorder)
# --------

aorder <- model_order$arma[c(1, 6, 2, 3, 7, 4, 5)]
names(aorder) <- c("p", "d", "q", "P", "D", "Q", "Frequency")

model_fit2 <- stlm(tsdata, modelfunction=Arima, order=arimaorder(model_order))
df1_forecast2 <- forecast(model_fit2, h=h)
arima_accuracy <- accuracy(df1_forecast2)
arima_accuracy

asorder <- model_sorder$arma[c(1, 6, 2, 3, 7, 4, 5)]
names(asorder) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
o = asorder[1:3]
s = asorder[4:7]

model_fit_s <- stlm(tsdata, modelfunction=Arima, 
                    order=c(asorder[1],asorder[2],asorder[3]), 
                    seasonal=list(order=c(asorder[4],asorder[5],asorder[6])))

df1_forecast_s <- forecast(model_fit_s, h=h)
sarima_accuracy <- accuracy(df1_forecast_s)
sarima_accuracy
# -------- plots below ------------------
df1_forecast_s$mean - df1_forecast2$mean
plot(df1_forecast_s$mean, df1_forecast2$mean)
plot(df1_forecast_s$fitted, df1_forecast2$fitted)

df_all_arima <- c(df1_forecast2$fitted, df1_forecast2$mean)
df_all_sarima <- c(df1_forecast_s$fitted, df1_forecast_s$mean)
plot(df_all_arima)
plot(df_all_sarima)

library(ggfortify)
autoplot(ts(cbind(df_all_arima, df_all_sarima), start = c(2016,1), frequency = 365),
         facets = FALSE)

# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# to-do-12/07
# include sarima within 'Test9v2m' and call it "Test9v3"
# use Test9v3 and execute this on R5x9 (w/ new data 2016/01/01 till 2018/11/30)
# get sarima in fxn similar to r_ArimaFxn
# include in class and run the class
# ideally, you should have prophet, hw, arima, sarima in the class by Monday 12/10
# also, run addl. script to summarize results from the class
# -----------------------------------------------------------

head(fit_sarima_fc$upper[,1])
head(fit_sarima_fc$upper[,2])
head(as.numeric(fit_sarima_fc$upper[,1]))
t <- head(as.numeric(fit_sarima_fc$upper[,1]))
write.csv(t, 't.csv')

# --------------------
# time, seasonal, scatter, lag plots
# https://otexts.org/fpp2/seasonal-plots.html
# START W/ ALL AMR (PAID+NOTPAID)
# Make the 'y' variables to a ts object
rnb <- ts(DF$nb, start=2016,freq=365)
str(rnb)
library(forecast)
library(ggplot2)
# time plot
autoplot(rnb) +
  ggtitle("Net Bookings - AMR Paid - Time Plot") +
  ylab("net bookings") +
  xlab("date")
# seasonal plot
#setwd("/users/akuppam/documents/Hprog/R/Prophet/")
#dfmm <- read.csv("amr1.csv")
ggseasonplot(rnb, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("net bookings") +
  ggtitle("Net Bookings - AMR Paid - Seasonal Plot")
monthplot(rnb, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("net bookings") +
  ggtitle("Net Bookings - AMR Paid - Seasonal Plot")
# polar seasonal plot
ggseasonplot(rnb, polar = TRUE) +
  ylab("Net bookings") +
  ggtitle("Polar seasonal plot: Net bookings - AMR Paid")
monthplot(rnb, polar = TRUE) +
  ylab("Net bookings") +
  ggtitle("Polar seasonal plot: Net bookings - AMR Paid")
# seasonal subseries plot
ggsubseriesplot(ts(dfmm$Net.Bookings)) +
  ylab("Net bookings") +
  ggtitle("Seasonal subseries plot: Net bookings - AMR Paid")
monthplot(ts(dfmm$Net.Bookings)) +
  ylab("Net bookings") +
  ggtitle("Seasonal subseries plot: Net bookings - AMR Paid")
# multivariate scatter plots
library(ggfortify)
autoplot(ts(cbind(DF$nb, DF$visits, DF$listings, DF$ts+DF$ss), start = c(2016,1), frequency = 365),
         facets = TRUE)
# qplot - relationship between nb x visits
qplot(nb, visits, data=as.data.frame(DF)) + 
  ylab("Net Bookings") + xlab("Visits")
qplot(nb, listings, data=as.data.frame(DF)) + 
  ylab("Net Bookings") + xlab("Listings")
autoplot(rnb, facets=TRUE) +
  ylab("Net bookings")
# lag plots
nbwindow <- window(rnb, start=2016)
gglagplot(nbwindow)
nbwindow1 <- window(ts(dfmm$Net.Bookings))
gglagplot(nbwindow1)
nbwindow2 <- window(dfmm$Net.Bookings, frequency = 12)
gglagplot(nbwindow2)
# acf plots
ggAcf(nbwindow)
ggAcf(nbwindow1)
# -----------------
# simple moving average
library(TTR)
rnbSMA3 <- SMA(DF$nb, n=3)
plot.ts(rnbSMA3)
write.csv(rnbSMA3, "rnbSMA3.csv")
rnbSMA31 <- SMA(rnb, n=442)
plot.ts(rnbSMA31)

# single/simple exponential smoothing
ses_model <- HoltWinters(rnb, beta=FALSE, gamma=FALSE)
ses_fc <- forecast(ses_model, h=442)
write.csv(ses_fc, "ses_fc.csv")

# Exponential Smoothing using state space approach
## STLF Exponential smoothing
library(forecast)
st_ets <- stlf(rnb, method="ets", h=h)$mean
write.csv(st_ets, "1_st_ets.csv")

st_etsf <- stlf(rnb, method="ets", h=h)$fitted
write.csv(st_etsf, "1_st_etsf.csv")

# Double Exponential Smoothing / Holt's
ses_model <- HoltWinters(tsdata, beta=FALSE, gamma=FALSE)
# Triple/ Holt-Winters

# ARIMA
# --------
h = 442
start = 2016
freq = 365
library(forecast)
forecast_func <- function(df1) {
  model_order <- auto.arima(df1)
  df1_ts <- ts(df1, start=start, freq=freq)
  model_fit <- stlm(df1_ts, modelfunction=Arima, order=arimaorder(model_order))
  df1_forecast <- forecast(model_fit, h=h)
  #a_accuracy <- accuracy(a_forecast)
}

fores_output = forecast_func(DF[,9])
df_all <- c(DF[,9], fores_output$mean)
plot(df_all)
# --------
# --------
# sarima Fxn in R
# -----
h = 442
start = 2016
freq = 365
library(forecast)
forecast_func <- function(df1) {
  df1_ts <- ts(df1, start=start, freq=freq)
  model_sorder <- auto.arima(df1_ts, D=1) # takes in a ts(object) as input data and outputs a sarima model
  
  asorder <- model_sorder$arma[c(1, 6, 2, 3, 7, 4, 5)]
  names(asorder) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
  model_fit_s <- stlm(df1_ts, modelfunction=Arima,
                      order=c(asorder[1],asorder[2],asorder[3]),
                      seasonal=list(order=c(asorder[4],asorder[5],asorder[6])))
  
  df1_forecast_s <- forecast(model_fit_s, h=h)
}

fores_output = forecast_func(DF[,9])
df_all_s <- c(DF[,9], fores_output$mean)
plot(df_all_s)

# checking Arima_Sarima_PypeR.ipynb code

X = df['y']
print(X)
r = pr.R()
r.assign("rDF", X)
r.assign("h", fcstdays)

# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/Py/rPy/")
rDF <- read.csv("test_data_6m.csv")
summary(rDF)
head(rDF)

library(forecast)
model_order <- auto.arima(rDF$nb, seasonal = T)
ts_data <- ts(rDF$nb, start=2016,freq=365)
summary(ts_data)
fit <- stlm(ts_data, modelfunction=Arima, order=arimaorder(model_order))
fit_arima_fc <- forecast(fit, h=365)
df_arima <- c(fit_arima_fc$fitted, fit_arima_fc$mean)
df_arima

#r_arima_series = pd.DataFrame(r.get("df_arima"))
#r_arima_series = r_arima_series[0].values
#return r_arima_series


# Prophet


# BSTS

# -------------------
# 1/9/2019
# https://stats.stackexchange.com/questions/213201/seasonality-not-taken-account-of-in-auto-arima



# https://www.kaggle.com/kailex/arima-with-fourier-terms

library(data.table)
library(TSA)
library(forecast)

# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/R/Prophet/")
DFagg <- read.csv("rnb1015_2.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v2m/AMR_paid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'NotPaid'))),]
summary(DF)

setwd("/users/akuppam/documents/Hprog/R/Prophet/")

ndiffs(DF$nb)
p <- periodogram(DF$nb)
data.table(period = 1/p$freq, spec = p$spec)[order(-spec)][1:2]

setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v2m/AMR_paid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'SoEu') & (DFagg[,'marketing'] == 'Paid'))),]
summary(DF)

setwd("/users/akuppam/documents/Hprog/R/Prophet/")

# ----- seasonality # 1 (https://www.kaggle.com/kailex/arima-with-fourier-terms)
ndiffs(DF$nb)
p <- periodogram(DF$nb)
data.table(period = 1/p$freq, spec = p$spec)[order(-spec)][1:2]

# ----- seasonality # 2 (https://anomaly.io/detect-seasonality-using-fourier-transform-r/)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

# display the 2 highest "power" frequencies
top2

# convert frequency to time periods
time = 1/top2$f
time

# -------
# Train set
#y <- ts(DF$nb[1:800])
y <- DF$nb[1:800]
# Test set
y.te <- DF$nb[801:1019]

# Base model
fit0 <- auto.arima(y)
(bestfit <- list(aicc=fit0$aicc, i=0, j=0, fit=fit0))

fc0 <- forecast(fit0, h=60)
plot(fc0)
# -------
# Two reg model
z1 <- fourier(ts(y, frequency=1024), K=1)
z2 <- fourier(ts(y, frequency=512), K=1)

fit2 <- auto.arima(y, xreg=cbind(z1, z2), seasonal=F)
fit2

fc2 <- forecast(fit2$fit,
               xreg=cbind(
                 fourier(ts(y, frequency=1024), K=1, h=60),
                 fourier(ts(y, frequency=512), K=1, h=60)))
plot(fc)
# --------
# Choose the best model by AICc
for(i in 1:10) {
  for (j in 1:10){
    z1 <- fourier(ts(y, frequency=1024), K=i)
    z2 <- fourier(ts(y, frequency=512), K=j)
    fit <- auto.arima(y, xreg=cbind(z1, z2), seasonal=F)
    if(fit$aicc < bestfit$aicc) {
      bestfit <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit
#fit
#bestfit$fit

xreg=cbind(
  fourier(ts(y, frequency=1024), K=bestfit$i, h=60),
  fourier(ts(y, frequency=512), K=bestfit$j, h=60)
  )

fc <- forecast(bestfit$fit,
               xreg=cbind(
                 fourier(ts(y, frequency=1024), K=bestfit$i, h=60),
                 fourier(ts(y, frequency=512), K=bestfit$j, h=60)))
plot(fc)

fc <- forecast(bestfit$fit,
               xreg=cbind(
                 #fourier(ts(y, frequency=1024), K=1, h=60),
                 fourier(ts(y, frequency=512), K=1, h=60)
                 )
               )

fc <- forecast(bestfit$fit,
               xreg=cbind(z1, z2))

plot(fc)

harmonics <- fourier(ts(y, frequency=1024), K = 13)
fit <- auto.arima(ts(y, frequency=1024), xreg = harmonics, seasonal = FALSE)
newharmonics <- fourier(ts(y, frequency=1024), K = 13, h = 156)
fc <- forecast(fit, xreg = newharmonics)
autoplot(fc)

# ----------
y <- DF$nb
# Choose the best model by AICc
for (j in 1:10){
  z2 <- fourier(ts(y, frequency=512), K=j)
  fit <- auto.arima(y, xreg=z2, seasonal=F)
  if(fit$aicc < bestfit$aicc) {
    bestfit <- list(aicc=fit$aicc, j=j, fit=fit)
    }
  }
bestfit

fc <- forecast(bestfit$fit, h=60,
               xreg=fourier(ts(y), K = bestfit$j, h=60))
plot(fc)
# ----------
# TBATS Model

y <- DF$nb[1:800]
y <- ts(DF$nb[1:800])
fc.tbats <- forecast(tbats(y, seasonal.periods=c(1024, 512)), h=442)
plot(fc.tbats)

# ------
# https://stackoverflow.com/questions/48306880/fourier-transform-for-daily-weekly-data
# THIS WORKS FINE !!!

#Check for daily seasonality
ets(y)
fit <- tbats(y)
seasonal <- !is.null(fit$seasonal)
seasonal

#Check for weekly seasonality
timeSeriesObj = ts(y, start=c(2016,1,1),frequency=365)
fit <- tbats(timeSeriesObj)
seasonal <- !is.null(fit$seasonal)
seasonal

# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# THE FOLLOWING CODE IS GOOD FOR FOURIER SERIES AS XREG + AUTO.ARIMA
# THIS CAN BE XFERRED OVER TO PYTHON
# THIS LINK HAS GOOD EXPLANATION OF FOURIER AND COEFFS IN TERMS OF EQUATIONS
# https://content.pivotal.io/blog/forecasting-time-series-data-with-multiple-seasonal-periods

# **** INCL TWO XREG/FOURIER TO SEE IF THIS CODE WILL WORK *** #  TODAY PM

library(data.table)
library(TSA)
library(forecast)

# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/R/Prophet/")
DFagg <- read.csv("rnb1015_2_v2.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'NotPaid'))),]
summary(DF)

y <- DF$nb
plot(y)

timeSeriesObj = ts(y, start=c(2016,1,1),frequency=365)
y <- timeSeriesObj

bestfit <- list(aicc=Inf)
for(i in 50)
{
  fit <- auto.arima(y, xreg=fourier(y, K=i), seasonal=TRUE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
  print(i)
  besti <- i
}
besti # outputs the best no of Fourier Terms for this model based on lowest AICc (from above)
bestfit

fc <- forecast(bestfit, xreg=fourier(y, K=besti, h=442))
autoplot(fc)

df_arima_fourier <- c(fc$fitted, fc$mean)
write.csv(df_arima_fourier, 'fc_arima_fourier.csv')

# ------------
# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/R/Prophet/")
DFagg <- read.csv("rnb1015_2_v2.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

DF = DFagg[(which((DFagg[,'region'] == 'UK') & (DFagg[,'marketing'] == 'NotPaid'))),]
summary(DF)

# test xreg (3/12/2019)

setwd("/Users/akuppam/Documents/Hprog/Py/AllMetrics_Jan31/xreg/")
DF <- read.csv("allData.csv")
summary(DF)

tsnb <- ts(DF$y, start=2017,freq=365)
fitX <- auto.arima(DF$y, xreg=DF$xreg1, seasonal = TRUE)
arimaorder(fitX)

tsvisits <- ts(DF$xreg1, start=2017,freq=365)
mvisits <- auto.arima(DF$xreg1, seasonal = TRUE)
arimaorder(mvisits)

m1visits <- stlm(tsvisits, modelfunction=Arima, order=arimaorder(mvisits))
fvisits <- as.numeric(data.frame(forecast(m1visits, h=442))$Point.Forecast)
autoplot(forecast(m1visits, h=442))
f1visits <- forecast(m1visits, h=442)


#fitX1 <- stlm(tsnb, modelfunction=Arima, order=arimaorder(fitX))
fcX <- forecast(fitX, xreg = xreg$mean)
autoplot(fcX)

fitX = stl(tsnb, "periodic")
fcX1 <- forecast(fitX, h=442, method="arima", xreg=DF$xreg1, newxreg=newxreg)

# /////////////////////////////////////
# /////////////////////////////////////

library(data.table)
library(TSA)
library(forecast)

# load dataset
library(dplyr)
setwd("/Users/akuppam/Documents/Hprog/Py/AllMetrics_Jan31/xreg/")
DF <- read.csv("allData.csv")

# xreg
tsvisits <- ts(DF$xreg1, start=2017,freq=365)
mvisits <- auto.arima(DF$xreg1, seasonal = TRUE)
m1visits <- stlm(tsvisits, modelfunction=Arima, order=arimaorder(mvisits))
f1visits <- forecast(m1visits, h=442)
autoplot(f1visits)
newxreg <- f1visits$mean
xreg <- DF$xreg1
xreg_data <- c(f1visits$fitted, f1visits$mean)
write.csv(xreg_data, "0_xreg_data.csv")

# 1. model & forecast w/ xreg
tsnb <- ts(DF$y, start=2017,freq=365)
fcX1 <- stlf(tsnb, h=442, method="arima", xreg=xreg, newxreg=newxreg)
autoplot(fcX1)
write.csv(fcX1, "1_model_xreg.csv")
model1_xreg <- c(fcX1$fitted, fcX1$mean)
write.csv(fcX1, "1_model_xreg_1.csv")

# 2. model & forecast w/o xreg
fcX2 <- stlf(tsnb, h=442, method="arima")
autoplot(fcX2)
write.csv(fcX2, "2_model_no_xreg.csv")
model2_no_xreg <- c(fcX2$fitted, fcX2$mean)
write.csv(fcX1, "2_model_xreg_2.csv")

## 3. Auto.Arima
rnb <- ts(DF$y, start=2017,freq=365)
rnb_arima <- auto.arima(DF$y)  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit_arima_fc <- forecast(fit, h=442)
autoplot(fit_arima_fc)
write.csv(fit_arima_fc, "3_auto_arima.csv")
model3_arima <- c(fit_arima_fc$fitted, fit_arima_fc$mean)
write.csv(fcX1, "3_model_xreg_3.csv")

## 4. Auto.Arima  +  xreg
rnb <- ts(DF$y, start=2017,freq=365)
rnb_arima <- auto.arima(DF$y)  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima), xreg = xreg)
fit_arima_fc <- forecast(fit, h=442)
autoplot(fit_arima_fc)
write.csv(fit_arima_fc, "4_auto_arima_xreg.csv")
model4_arima <- c(fit_arima_fc$fitted, fit_arima_fc$mean)
write.csv(fcX1, "4_model_xreg_4.csv")
# -------------------------------------------
stlm(y, s.window = 13, robust = FALSE, method = c("ets", "arima"),
     modelfunction = NULL, model = NULL, etsmodel = "ZZN",
     lambda = NULL, biasadj = FALSE, xreg = NULL,
     allow.multiplicative.trend = FALSE, x = y, ...)

# S3 method for stlm
forecast(object, h = 2 * object$m, level = c(80, 95),
         fan = FALSE, lambda = object$lambda, biasadj = NULL,
         newxreg = NULL, allow.multiplicative.trend = FALSE, ...)

stlf(y, h = frequency(x) * 2, s.window = 13, t.window = NULL,
     robust = FALSE, lambda = NULL, biasadj = FALSE, x = y, ...)
# -------------------------------------------
# Compute MAPES
df_mape <- data.frame(rnb, fit_arima_fc$fitted, fcX1$fitted, fcX2$fitted)
mape_arima <- mean(abs((df_mape$rnb - df_mape$fit_arima_fc.fitted)/df_mape$rnb))
mape_arima
mape_xreg <- mean(abs((df_mape$rnb - df_mape$fcX1.fitted)/df_mape$rnb))
mape_xreg
mape_no_xreg <- mean(abs((df_mape$rnb - df_mape$fcX2.fitted)/df_mape$rnb))
mape_no_xreg
# -------------------------------------------
# Plot forecasts from different methods
dfreal <- c(DF$y, rep(NA, 442))
df_all <- data.frame(dfreal, model1_xreg, model2_no_xreg, model3_arima)
names(df_all) <- c("real","model1_xreg","model2_no_xreg_stlf","model3_arima")
write.csv(df_all, "df_all_agg.csv")

# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2017-01-01"))),
                                length.out = nrow(df_all), 
                                by="days"),
                      data = df_all)
library(dplyr)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs all models.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models")
return(print(p))
dev.off()
# ----- forecasts ONLY -----------
df_all_f <- df_all[(nrow(DF)+1):nrow(df_all),]
# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2019-01-14"))),
                                length.out = nrow(df_all_f), 
                                by="days"),
                      data = df_all_f)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs all models - forecasts ONLY.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models - forecasts ONLY")
return(print(p))
dev.off()

# /////////////////////////////////////
# /////////////////////////////////////

# INVESTIGATE SHAPE OF PLOT....SEEMS ODD....CAN IT BE FIXED ?? MODIFY fcX; SEARCH FOR STLM W/ XREG

model<-stl(tseries,"periodic")
forecast<-forecast(model,h=10,method="arima",xreg=xreg,newxreg=newxreg)

forecast<-stlf(tseries,h=10,method="arima",xreg=xreg,newxreg=newxreg)

# https://github.com/earowang/hts/issues/6
htseg2x <- matrix(rnorm(16*17),nrow=16,ncol=17)
htseg2nx <- matrix(rnorm(10*17),nrow=10,ncol=17)
forecast(htseg2 , h=10, fmethod="arima", xreg=rnorm(16), newxreg=rnorm(10))

htseg2x <- matrix(rnorm(16*17),nrow=16,ncol=17)
htseg2nx <- matrix(rnorm(10*17),nrow=10,ncol=17)
forecast(htseg2 , h=10, fmethod="arima", xreg=htseg2x, newxreg = htseg2nx)




# findings
# main y var should not go through STLM
# put only xreg through STLM, if not, xreg will lead to flat forecasts
# so do y = f(xreg) normal way, and forecast xreg using stlm, and forecast y normal way
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------

# -------
# TBATS - always shows 'straight line' forecasts
nb_tbats <- tbats(y, seasonal.periods=365)
fc2 <- forecast(nb_tbats, h=442)
autoplot(fc2, ylab="net bookings")
# ------

# -------------------------------------------
# Make the 'y' variables to a ts object
rnb <- ts(DF$nb, start=2016,freq=341.3333)
str(rnb)
# -------------------------------------------
## Auto.Arima
rnb_arima <- auto.arima(DF$nb)  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit_arima_fc <- forecast(fit, h=442)
write.csv(fit_arima_fc, "test1_6_rnb_arima_pred_fc.csv")
plot(fit_arima_fc)

# ------------------------------------------------------------------------------
# Endogenously Detecting Structural Breaks in a Time Series: Implementation in R
# https://pythonandr.com/2016/11/08/endogenously-detecting-structural-breaks-in-a-time-series-implementation-in-r/

library(xlsx)
library(forecast)
library(tseries)
library(strucchange)

# look under GitHub repo copied here:  /users/akuppam/documents/Hprog/Time-Series-Analysis-master/

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
#prod_df <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 8:65, colIndex = 2, header = FALSE)
prod_df <- read.csv(file = '/users/akuppam/documents/Hprog/Time-Series-Analysis-master/rice.csv', header = TRUE)

#colnames(prod_df) <- c('Rice')
## store rice data as time series objects
rice <- ts(prod_df$Rice, start=c(1951, 1), end=c(2008, 1), frequency=1) 

# store the breakpoints
bp.rice <- breakpoints(rice ~ 1)
summary(bp.rice)

bp.y <- breakpoints(y ~ 1)
summary(bp.y)

## the BIC chooses 5 breakpoints; plot the graph with breakdates and their confidence intervals
plot(bp.rice)
plot(rice)
lines(bp.rice)

plot(bp.y)
plot(y)
lines(bp.y)

breakdates(bp.y)  # these show the black dashed vertical lines on the chart

## confidence intervals
ci.rice <- confint(bp.rice)
ci.rice
lines(ci.rice)

ci.y <- confint(bp.y)
ci.y
lines(ci.y)

# ---------------------------
# CE_call volumes analysis

library(data.table)
library(TSA)
library(forecast)

# load dataset
library(dplyr)
setwd("/users/akuppam/documents/CE/")
na_ptnr <- read.csv("NA_PTNR_2016-10.2018 for Arun.csv")

setwd("/users/akuppam/documents/Stay/")
stay <- read.csv("feb_test1.csv")

# ----- seasonality # 1 (https://www.kaggle.com/kailex/arima-with-fourier-terms)
ndiffs(na_ptnr$NA_PTNR)
p <- periodogram(na_ptnr$NA_PTNR)
data.table(period = 1/p$freq, spec = p$spec)[order(-spec)][1:2]

ndiffs(stay$y)
p <- periodogram(stay$y)
data.table(period = 1/p$freq, spec = p$spec)[order(-spec)][1:2]

periodicity(ts(stay$y))  # this works fine

# ----- seasonality # 2 (https://anomaly.io/detect-seasonality-using-fourier-transform-r/)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

# display the 2 highest "power" frequencies
top2

# convert frequency to time periods
time = 1/top2$f
time

y <- na_ptnr$NA_PTNR
y <- stay$y

#Check for daily seasonality
ets(y)
fit <- tbats(y)
seasonal <- !is.null(fit$seasonal)
seasonal

#Check for weekly seasonality
timeSeriesObj = ts(y, start=c(2016,1,1),frequency=7)
fit <- tbats(timeSeriesObj)
seasonal <- !is.null(fit$seasonal)
seasonal

#Check for monthly seasonality
timeSeriesObj = ts(y, start=c(2016,1,1),frequency=30.42)
fit <- tbats(timeSeriesObj)
seasonal <- !is.null(fit$seasonal)
seasonal

#Check for quaterly seasonality
timeSeriesObj = ts(y, start=c(2016,1,1),frequency=91.25)
fit <- tbats(timeSeriesObj)
seasonal <- !is.null(fit$seasonal)
seasonal

#Check for yearly seasonality
timeSeriesObj = ts(y, start=c(2016,1,1),frequency=365)
fit <- tbats(timeSeriesObj)
seasonal <- !is.null(fit$seasonal)
seasonal

# ------------------
# HW testing (2/4/19)
# hw() can handle ONLY WHEN freq=24 or less
# hw() older version which is mostly suitable to exponential smoothing but not seasonal data
# use HoltWinters() when data is seasonal (freq=365)
# ------------------
library(fpp)
data(austourists)

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")

fit1_fc <- forecast(fit1, h=10000)


fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

# ----------------------------------------------------------
# generate hourly data
# https://stackoverflow.com/questions/17156143/how-to-create-a-r-timeseries-for-hourly-data

require(xts)
require(forecast)

time_index <- seq(from = as.POSIXct("2012-05-15 07:00"), 
                  to = as.POSIXct("2012-05-17 18:00"), by = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))

eventdata <- xts(value, order.by = time_index)
ets(eventdata)
autoplot(eventdata)
