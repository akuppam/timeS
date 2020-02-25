# ##############################
# -----------------------------
# SES, ARIMA, HW
# -----------------------------
# ##############################

library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)


### rnbl2agg.csv
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl2agg.csv")
# -------------------------------------------
# https://www.r-bloggers.com/running-a-model-on-separate-groups/
# take sub-groups of data from the above data file (DF)
# run the following diff models
# save Plots
# compile MAPES
# output csv files
# -------------------------------------------
# Make the 'y' variables to a ts object
rnb <- ts(DF[,9], start=2016,freq=365)
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
# Compute MAPE for df6
df_mape <- data.frame(rnb, fit_arima_fc$fitted, fit_hw_fc$fitted)
mape_arima <- mean(abs((df_mape$rnb - df_mape$fit_arima_fc.fitted)/df_mape$rnb))
df_mape_hw <- df_mape[366:nrow(df_mape),]
mape_hw <- mean(abs((df_mape_hw$rnb - df_mape_hw$fit_hw_fc.fitted)/df_mape_hw$rnb))
# -------------------------------------------
# -------------------------------------------
# Plot forecasts from different methods
df_rnb <- c(rnb, rep(NA,365))
df1 <- c(rnb, st_ets)
df6 <- c(fit_arima_fc$fitted, fit_arima_fc$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)
df11 <- c(fit_hw_fc$fitted, fit_hw_fc$mean)
df_all <- data.frame(df_rnb,df1,df6,df11)
names(df_all) <- c("nb","ets","arima","hw")
# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                    length.out = nrow(df_all), 
                                    by="days"),
                          data = df_all)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("comp of ts models.jpg",height=4.25,width=5.5,res=200,units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() +
  ggtitle("real vs ets, hw, arima models")
# ----- forecasts ONLY -----------
df_all_f <- df_all[999:nrow(df_all),]
# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2018-09-25"))),
                                length.out = nrow(df_all_f), 
                                by="days"),
                      data = df_all_f)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs ets, hw, arima - forecasts ONLY.jpg", height=4.25, width=5.5, res=200, units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + ggtitle("real vs ets, hw, arima - forecasts ONLY")
dev.off()
# ----------------------------------------------------

