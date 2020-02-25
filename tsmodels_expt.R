# ########################
# Prophet Model w/
# - regressors
# - holidays
# - seasonality
# ETS model
# HW model
# Arima model
# ########################

library(corrplot)
library(plotly)
library(prophet)
library(tidyverse)
library(bsts)
library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# load dataset
#setwd("/users/akuppam/documents/Data/RoverData/")
#DF <- read.csv("rnbl2agg.csv")
#DF <- mutate(DF, ds = as.Date(date))
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# adding holidays
major <- data_frame(
  holiday = 'majorH',
  ds = as.Date(c('2017-01-01', '2017-05-29', '2017-07-04',
                 '2017-09-04', '2017-11-23', '2017-12-25',
                 '2018-01-01', '2018-05-28', '2018-07-04',
                 '2018-09-03', '2018-11-22', '2018-12-25')),
  lower_window = 0,
  upper_window = 1
)
minor <- data_frame(
  holiday = 'minorH',
  ds = as.Date(c('2017-01-16', '2017-02-20', '2017-10-09',
                 '2018-01-15', '2018-02-19', '2018-10-08')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(major, minor)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# adding seasonality (ADD THIS TO EVERY MODEL BELOW)
yearly.seasonality = TRUE
weekly.seasonality = TRUE
daily.seasonality = FALSE
# *******************
holidays = NULL
# *******************
model = prophet(holidays = holidays, 
        yearly.seasonality = yearly.seasonality, 
        weekly.seasonality = weekly.seasonality,
        daily.seasonality = daily.seasonality,
        seasonality.prior.scale = 20)

model = add_seasonality(model, name = 'monthly', period = 30.5, fourier.order = 5)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# adding regressors
colnames(DF) <- c("date","region","marketing","visits","br","inq","gb","cb","y","ss","ts","listings","ds")
pdat <- data.frame(ds=DF$ds, y=DF$y, visits=DF$visits, br=DF$br, listings=DF$listings)
pfdat <- data.frame(ds=max(DF$ds) + 1:365)
pvisits <- DF %>% dplyr::select(ds,y=visits) %>% prophet(holidays = holidays, 
                                                         yearly.seasonality = yearly.seasonality, 
                                                         weekly.seasonality = weekly.seasonality,
                                                         daily.seasonality = daily.seasonality) %>% predict(pfdat)
pbr <- DF %>% dplyr::select(ds,y=br) %>% prophet(holidays = holidays, 
                                                 yearly.seasonality = yearly.seasonality, 
                                                 weekly.seasonality = weekly.seasonality,
                                                 daily.seasonality = daily.seasonality) %>% predict(pfdat)
plistings <- DF %>% dplyr::select(ds,y=listings) %>% prophet(holidays = holidays, 
                                                             yearly.seasonality = yearly.seasonality, 
                                                             weekly.seasonality = weekly.seasonality,
                                                             daily.seasonality = daily.seasonality) %>% predict(pfdat)
fdat <-  data.frame(ds=pfdat$ds, visits=pvisits$yhat, br=pbr$yhat, listings=plistings$yhat)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
fit <- model %>% fit.prophet(pdat)

forecast <- predict(fit, fdat)
fpred <- predict(fit)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

dfprophet <- c(fpred$yhat, forecast$yhat)
mape_prophet <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
fit <- model %>% 
  add_regressor('visits') %>% 
  fit.prophet(pdat)

forecast <- predict(fit, fdat)
fpred <- predict(fit)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

dfprophet_visits <- c(fpred$yhat, forecast$yhat)
mape_prophet_visits <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------
# -------------------
fit <- model %>% 
  add_regressor('br') %>% 
  fit.prophet(pdat)

forecast <- predict(fit, fdat)
fpred <- predict(fit)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

dfprophet_br <- c(fpred$yhat, forecast$yhat)
mape_prophet_br <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------
# -------------------
fit <- model %>% 
  add_regressor('visits') %>% 
  add_regressor('br') %>% 
  fit.prophet(pdat)

forecast <- predict(fit, fdat)
fpred <- predict(fit)

dfprophet_vibr <- c(fpred$yhat, forecast$yhat)
mape_prophet_vibr <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------
# -------------------
fit <- model %>% 
  add_regressor('visits') %>% 
  add_regressor('br') %>% 
  add_regressor('listings') %>% 
  fit.prophet(pdat)

forecast <- predict(fit, fdat)
fpred <- predict(fit)

dfprophet_vibrli <- c(fpred$yhat, forecast$yhat)
mape_prophet_vibrli <- mean(abs((pdat$y - fpred$yhat)/pdat$y))

# -------------------
fit <- model %>% 
  add_regressor('br') %>% 
  add_regressor('listings') %>% 
  fit.prophet(pdat)

forecast <- predict(fit, fdat)
fpred <- predict(fit)

dfprophet_brli <- c(fpred$yhat, forecast$yhat)
mape_prophet_brli <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# ----------------------------------------------------
# ----------------------------------------------------
# SES, ARIMA, HW
# ----------------------------------------------------
# ----------------------------------------------------
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
# -------------------------------------------
# Make the 'y' variables to a ts object
rnb <- ts(DF$y, start=2016,freq=365)
str(rnb)
# -------------------------------------------
# Exponential Smoothing using state space approach
## STLF Exponential smoothing
st_ets <- stlf(rnb, method="ets", h=365)$mean
write.csv(st_ets, "1_st_ets.csv")
# -------------------------------------------
# -------------------------------------------
# HoltWinters
rnbAlphaBetaGamma <- HoltWinters(rnb)
fit_hw_fc <- forecast(rnbAlphaBetaGamma, h=365)
write.csv(fit_hw_fc, "11_fit_hw.csv")
# -------------------------------------------
# -------------------------------------------
## Auto.Arima
rnb_arima <- auto.arima(DF[,9])  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit_arima_fc <- forecast(fit, h=365)
write.csv(fit_arima_fc, "6_rnb_arima_pred_fc.csv")
# -------------------------------------------
# -------------------------------------------
# Compute MAPES
df_mape <- data.frame(rnb, fit_arima_fc$fitted, fit_hw_fc$fitted)
mape_arima <- mean(abs((df_mape$rnb - df_mape$fit_arima_fc.fitted)/df_mape$rnb))
df_mape_hw <- df_mape[366:nrow(df_mape),]
mape_hw <- mean(abs((df_mape_hw$rnb - df_mape_hw$fit_hw_fc.fitted)/df_mape_hw$rnb))
# Output MAPES by Model
library(dplyr)
mapes_by_model_Hols <- data_frame(
  ts_model = c('prophet','prophet_br','prophet_brli','prophet_vibr',
               'prophet_vibrli','prophet_visits',
               'hw','arima'),
  mape = c(mape_prophet,mape_prophet_br,mape_prophet_brli,mape_prophet_vibr,
           mape_prophet_vibrli,mape_prophet_visits,
           mape_hw,mape_arima) 
)
write.csv(mapes_by_model_Hols, "MAPE_by_model.csv")
# -------------------------------------------
# -------------------------------------------
# experimental stuff below
# -------------------
prophet_plot_components(fit, forecast)
plot_forecast_component(fit, forecast, 'majorH')
plot_forecast_component(fit, forecast, 'minorH')

# just checking the contribution of majorH, minorH on bookings
forecast %>% 
  dplyr::select(ds, majorH, minorH) %>% 
  filter(abs(majorH + minorH) > 0) %>%
  tail(10)

write.csv(forecast, "forecast.csv")
write.csv(fpred, "fpred.csv")

