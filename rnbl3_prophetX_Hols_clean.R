# ----------------------
# Prophet Model w/ regressors
# propherX
# ----------------------

library(corrplot)
library(plotly)
library(prophet)
library(tidyverse)
library(bsts)  
library(prophet)
library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl2agg.csv")
#DF <- read.csv("UK-paid.csv")
DF <- mutate(DF, ds = as.Date(date))

# adding regressors
library(dplyr)
colnames(DF) <- c("date","region","marketing","visits","br","inq","gb","cb","y","ss","ts","listings","ds")
pdat <- data.frame(ds=DF$ds, y=DF$y, visits=DF$visits, br=DF$br, listings=DF$listings, inq=DF$inq)
pfdat <- data.frame(ds=max(DF$ds) + 1:365)
pvisits <- DF %>% dplyr::select(ds,y=visits) %>% prophet() %>% predict(pfdat)
pbr <- DF %>% dplyr::select(ds,y=br) %>% prophet() %>% predict(pfdat)
plistings <- DF %>% dplyr::select(ds,y=listings) %>% prophet() %>% predict(pfdat)
pinq <- DF %>% dplyr::select(ds,y=inq) %>% prophet() %>% predict(pfdat)
fdat <-  data.frame(ds=pfdat$ds, visits=pvisits$yhat, br=pbr$yhat, listings=plistings$yhat, inq=pinq$yhat)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
library(dplyr)
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
fit6 <- prophet(holidays = holidays) %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

dfprophet <- c(fpred$yhat, forecast$yhat)
mape_prophet <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
fit6 <- prophet(holidays = holidays) %>% 
  add_regressor('visits') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

dfprophet_visits <- c(fpred$yhat, forecast$yhat)
mape_prophet_visits <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------
# -------------------
fit6 <- prophet(holidays = holidays) %>% 
  add_regressor('br') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

dfprophet_br <- c(fpred$yhat, forecast$yhat)
mape_prophet_br <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------
# -------------------
fit6 <- prophet(holidays = holidays) %>% 
  add_regressor('visits') %>% 
  add_regressor('br') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)

dfprophet_vibr <- c(fpred$yhat, forecast$yhat)
mape_prophet_vibr <- mean(abs((pdat$y - fpred$yhat)/pdat$y))
# -------------------
# -------------------
fit6 <- prophet(holidays = holidays) %>% 
  add_regressor('visits') %>% 
  add_regressor('br') %>% 
  add_regressor('listings') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)

dfprophet_vibrli <- c(fpred$yhat, forecast$yhat)
mape_prophet_vibrli <- mean(abs((pdat$y - fpred$yhat)/pdat$y))

# -------------------
fit6 <- prophet(holidays = holidays) %>% 
  add_regressor('br') %>% 
  add_regressor('listings') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)

dfprophet_brli <- c(fpred$yhat, forecast$yhat)
mape_prophet_brli <- mean(abs((pdat$y - fpred$yhat)/pdat$y))

# -------------------
prophet_plot_components(fit6, forecast)
plot_forecast_component(fit6, forecast, 'majorH')
plot_forecast_component(fit6, forecast, 'minorH')

# just checking the contribution of majorH, minorH on bookings
forecast %>% 
  dplyr::select(ds, majorH, minorH) %>% 
  filter(abs(majorH + minorH) > 0) %>%
  tail(10)

# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ##############################
# -----------------------------
# SES, ARIMA, HW
# -----------------------------
# ##############################
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
fit_ets <- ets(rnb)$fitted
fit_ets_fc <- forecast(fit_ets, h=365)
write.csv(fit_ets_fc, "1_fit_ets.csv")
# Note - there are slight differences between stlf/ets and ets()
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
# -------------------------------------------
# -------------------------------------------
# Plot forecasts from different methods
df_rnb <- c(rnb, rep(NA,365))
df1 <- c(rnb, st_ets)
df6 <- c(fit_arima_fc$fitted, fit_arima_fc$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)
df11 <- c(fit_hw_fc$fitted, fit_hw_fc$mean)

dfreal <- c(DF$y, rep(NA,365))
df_all <- data.frame(dfreal,dfprophet,dfprophet_visits,dfprophet_br,dfprophet_vibr,dfprophet_vibrli,df1,df6,df11)
names(df_all) <- c("real","prophet","prophet_visits","prophet_br","prophet_vibr","prophet_vibrli","ets","arima","hw")
write.csv(df_all, "df_all_agg.csv")

# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(df_all), 
                                by="days"),
                      data = df_all)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs all models.jpg", height=4.25, width=5.5, res=200, units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models")
dev.off()
# ----- forecasts ONLY -----------
df_all_f <- df_all[999:nrow(df_all),]
# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2018-09-25"))),
                                length.out = nrow(df_all_f), 
                                by="days"),
                      data = df_all_f)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs all models - forecasts ONLY.jpg", height=4.25, width=5.5, res=200, units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models - forecasts ONLY")
dev.off()
