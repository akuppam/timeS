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

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# horizon
h = 730
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# adding holidays
# US Holidays
easter <- data_frame(
  holiday = 'easterSunday',
  ds = as.Date(c('2010-04-04','2011-04-24','2012-04-08','2013-03-31','2014-04-20',
                 '2015-04-05','2016-03-27','2017-04-16','2018-04-01','2019-04-21',
                 '2020-04-12','2021-04-04','2022-04-17','2023-04-09','2024-03-31')),
  lower_window = -2,
  upper_window = 0
)
memorial <- data_frame(
  holiday = 'memorialMonday',
  ds = as.Date(c('2010-05-31','2011-05-30','2012-05-28','2013-05-27','2014-05-26',
                 '2015-05-25','2016-05-30','2017-05-29','2018-05-28','2019-05-27',
                 '2020-05-25','2021-05-31','2022-05-30','2023-05-29','2024-05-27')),
  lower_window = -2,
  upper_window = 0
)
laborday <- data_frame(
  holiday = 'laborMonday',
  ds = as.Date(c('2010-09-06','2011-09-05','2012-09-03','2013-09-02','2014-09-01',
                 '2015-09-07','2016-09-05','2017-09-04','2018-09-03','2019-09-02',
                 '2020-09-07','2021-09-06','2022-09-05','2023-09-04','2024-09-02')),
  lower_window = -2,
  upper_window = 0
)
thxgiving <- data_frame(
  holiday = 'thanksgiving',
  ds = as.Date(c('2010-11-25','2011-11-24','2012-11-22','2013-11-28','2014-11-27',
                 '2015-11-26','2016-11-24','2017-11-23','2018-11-22','2019-11-28',
                 '2020-11-26','2021-11-25','2022-11-24','2023-11-23','2024-11-28')),
  lower_window = 0,
  upper_window = 1
)
USholidays <- bind_rows(easter,memorial,laborday,thxgiving)

# France Holidays
easter <- data_frame(
  holiday = 'easterSunday',
  ds = as.Date(c('2010-04-04','2011-04-24','2012-04-08','2013-03-31','2014-04-20',
                 '2015-04-05','2016-03-27','2017-04-16','2018-04-01','2019-04-21',
                 '2020-04-12','2021-04-04','2022-04-17','2023-04-09','2024-03-31')),
  lower_window = -2,
  upper_window = 0
)
ascensionDay <- data_frame(
  holiday = 'ascensionDay',
  ds = as.Date(c('2014-05-29','2015-05-14','2016-05-05','2017-05-25','2018-05-10',
                 '2019-05-30','2020-05-21','2021-05-13','2022-05-26','2023-05-18',
                 '2024-05-09')),
  lower_window = 0,
  upper_window = 2
)
whit <- data_frame(
  holiday = 'whitMonday',
  ds = as.Date(c('2014-06-09','2015-05-25','2016-05-16','2017-06-05','2018-05-21',
                 '2019-06-10','2020-06-01','2021-05-24','2022-06-06','2023-05-29',
                 '2024-05-20')),
  lower_window = -1,
  upper_window = 0
)
FRholidays <- bind_rows(easter,ascensionDay,whit)

# UK Holidays
easter <- data_frame(
  holiday = 'easterSunday',
  ds = as.Date(c('2010-04-04','2011-04-24','2012-04-08','2013-03-31','2014-04-20',
                 '2015-04-05','2016-03-27','2017-04-16','2018-04-01','2019-04-21',
                 '2020-04-12','2021-04-04','2022-04-17','2023-04-09','2024-03-31')),
  lower_window = -2,
  upper_window = 1
)
mayDay <- data_frame(
  holiday = 'mayday',
  ds = as.Date(c('2015-05-04','2016-05-02','2017-05-01','2018-05-07','2019-05-06',
                 '2020-05-04','2021-05-03','2022-05-02','2023-05-01','2024-05-06',
                 '2025-05-05')),
  lower_window = -1,
  upper_window = 0
)
springBank <- data_frame(
  holiday = 'springBank',
  ds = as.Date(c('2015-05-25','2016-05-30','2017-05-29','2018-05-28','2019-05-27',
                 '2020-05-25','2021-05-31','2022-05-30','2023-05-29','2024-05-27',
                 '2025-05-26')),
  lower_window = -1,
  upper_window = 0
)
summerBank <- data_frame(
  holiday = 'summerBank',
  ds = as.Date(c('2015-08-31','2016-08-29','2017-08-28','2018-08-27','2019-08-26','2020-08-31',
                 '2021-08-30','2022-08-29','2023-08-28','2024-08-26','2025-08-25')),
  lower_window = -1,
  upper_window = 0
)
UKholidays <- bind_rows(easter,mayDay,springBank,summerBank)

# CE Holidays
easter <- data_frame(
  holiday = 'easterSunday',
  ds = as.Date(c('2010-04-04','2011-04-24','2012-04-08','2013-03-31','2014-04-20',
                 '2015-04-05','2016-03-27','2017-04-16','2018-04-01','2019-04-21',
                 '2020-04-12','2021-04-04','2022-04-17','2023-04-09','2024-03-31')),
  lower_window = -2,
  upper_window = 1
)
ascensionDay <- data_frame(
  holiday = 'ascensionDay',
  ds = as.Date(c('2014-05-29','2015-05-14','2016-05-05','2017-05-25','2018-05-10',
                 '2019-05-30','2020-05-21','2021-05-13','2022-05-26','2023-05-18',
                 '2024-05-09')),
  lower_window = 0,
  upper_window = 2
)
whit <- data_frame(
  holiday = 'whitMonday',
  ds = as.Date(c('2014-06-09','2015-05-25','2016-05-16','2017-06-05','2018-05-21',
                 '2019-06-10','2020-06-01','2021-05-24','2022-06-06','2023-05-29',
                 '2024-05-20')),
  lower_window = -1,
  upper_window = 0
)
CEholidays <- bind_rows(easter,ascensionDay,whit)

# SoEu Holidays
easter <- data_frame(
  holiday = 'easterSunday',
  ds = as.Date(c('2010-04-04','2011-04-24','2012-04-08','2013-03-31','2014-04-20',
                 '2015-04-05','2016-03-27','2017-04-16','2018-04-01','2019-04-21',
                 '2020-04-12','2021-04-04','2022-04-17','2023-04-09','2024-03-31')),
  lower_window = -2,
  upper_window = 1
)
SoEuholidays <- bind_rows(easter)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# adding seasonality (ADD THIS TO EVERY MODEL BELOW)
yearly.seasonality = TRUE
weekly.seasonality = TRUE
daily.seasonality = FALSE
# -----------------------
#holidays = NULL
if (region == 'AMR') {
  holidays <- USholidays
} else if (region == 'UK') {
  holidays <- UKholidays
} else if (region == 'FR') {
  holidays <- FRholidays
} else if (region == 'CE') {
  holidays <- CEholidays
} else {
  holidays <- SoEuholidays
}

# -----------------------
model = prophet(holidays = holidays, 
                yearly.seasonality = yearly.seasonality, 
                weekly.seasonality = weekly.seasonality,
                daily.seasonality = daily.seasonality,
                seasonality.prior.scale = 20)

#model = add_seasonality(model, name = 'monthly', period = 30.5, fourier.order = 5)
#model = add_seasonality(model, name = 'weekly', period = 7, fourier.order = 3, prior.scale = 0.1)
model = add_seasonality(model, name = 'yearly', period = 365, fourier.order = 3, prior.scale = 0.1)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# adding regressors
colnames(DF) <- c("date","region","marketing","visits","br","inq","gb","cb","y","ss","ts","listings","ds")
pdat <- data.frame(ds=DF$ds, y=DF$y, visits=DF$visits, br=DF$br, listings=DF$listings, inq=DF$inq)
pfdat <- data.frame(ds=max(DF$ds) + 1:h)
pvisits <- DF %>% dplyr::select(ds,y=visits) %>% prophet(holidays = holidays, 
                                                         yearly.seasonality = yearly.seasonality, 
                                                         weekly.seasonality = weekly.seasonality,
                                                         daily.seasonality = daily.seasonality) %>% predict(pfdat)
write.csv(pvisits,"pvisits.csv")
pbr <- DF %>% dplyr::select(ds,y=br) %>% prophet(holidays = holidays, 
                                                 yearly.seasonality = yearly.seasonality, 
                                                 weekly.seasonality = weekly.seasonality,
                                                 daily.seasonality = daily.seasonality) %>% predict(pfdat)
write.csv(pbr,"pbr.csv")
plistings <- DF %>% dplyr::select(ds,y=listings) %>% prophet(holidays = holidays, 
                                                             yearly.seasonality = yearly.seasonality, 
                                                             weekly.seasonality = weekly.seasonality,
                                                             daily.seasonality = daily.seasonality) %>% predict(pfdat)
write.csv(plistings,"plistings.csv")
pinq <- DF %>% dplyr::select(ds,y=inq) %>% prophet(holidays = holidays, 
                                                   yearly.seasonality = yearly.seasonality, 
                                                   weekly.seasonality = weekly.seasonality,
                                                   daily.seasonality = daily.seasonality) %>% predict(pfdat)
write.csv(pinq,"pinq.csv")
fdat <-  data.frame(ds=pfdat$ds, visits=pvisits$yhat, br=pbr$yhat, listings=plistings$yhat, inq=pinq$yhat)
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

#R
forecast %>%
  dplyr::select(ds, easterSunday, memorialMonday, laborMonday, thxgiving) %>%
  filter(abs(easterSunday + memorialMonday + laborMonday + thxgiving) > 0) %>%
  tail(10)

prophet_plot_components(fit, forecast)

forecast %>%
  dplyr::select(ds, easterSunday) %>%
  filter(abs(easterSunday) > 0) %>%
  tail(10)

prophet_plot_components(fit, forecast)
prophet:::plot_yearly(fit)

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
st_ets <- stlf(rnb, method="ets", h=h)$mean
write.csv(st_ets, "1_st_ets.csv")
# -------------------------------------------
# -------------------------------------------
# HoltWinters
rnbAlphaBetaGamma <- HoltWinters(rnb)
fit_hw_fc <- forecast(rnbAlphaBetaGamma, h=h)
write.csv(fit_hw_fc, "11_fit_hw.csv")
# -------------------------------------------
# -------------------------------------------
## Auto.Arima
rnb_arima <- auto.arima(DF[,9])  # ALWAYS INPUT RAW DATA THAT IS 'NOT' A ts() object
arimaorder(rnb_arima)
## STLM - apply Auto.Arima model to data
fit <- stlm(rnb, modelfunction=Arima, order=arimaorder(rnb_arima))
fit_arima_fc <- forecast(fit, h=h)
write.csv(fit_arima_fc, "6_rnb_arima_pred_fc.csv")
# -------------------------------------------
# -------------------------------------------
# Compute MAPES
df_mape <- data.frame(rnb, fit_arima_fc$fitted, fit_hw_fc$fitted)
mape_arima <- mean(abs((df_mape$rnb - df_mape$fit_arima_fc.fitted)/df_mape$rnb))
mape_arima
df_mape_hw <- df_mape[366:nrow(df_mape),]
mape_hw <- mean(abs((df_mape_hw$rnb - df_mape_hw$fit_hw_fc.fitted)/df_mape_hw$rnb))
mape_hw

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
