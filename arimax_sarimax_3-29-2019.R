# #############################################
# 
# ARIMAX & SARIMAX - W/ XREG (VISITS, LISTINGS)
#
# #############################################

# ///////////////////////////////////////////////////////////////////////////
# ARIMAX
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# 3/28/2019
# data from /LatestData/latest_data_amr.csv
# load dataset
library(dplyr)
library(forecast)

setwd("/users/akuppam/documents/Hprog/Py/AllMetrics_Jan31/xreg/LatestData/")
DFagg <- read.csv("latest_data_amr.csv")
DF = DFagg[(which((DFagg[,'rlt'] == 'SEO'))),]
summary(DF)

# 0. xreg using stlf method
tsvisits <- ts(DF$xreg1, start=2017,freq=365)
tslistings <- ts(DF$xreg2, start=2017, freq=365)
fcX0_1 <- stlf(tsvisits, h=365, method="arima")
fcX0_2 <- stlf(tslistings, h=365, method="arima")
model0_xreg1 <- c(fcX0_1$fitted, fcX0_1$mean)
model0_xreg2 <- c(fcX0_2$fitted, fcX0_2$mean)
model0_xreg <- cbind(model0_xreg1, model0_xreg2)
write.csv(model0_xreg, "0_model_xreg_3_amr_seo3.csv")

xreg <- cbind(DF$xreg1, DF$xreg2)
newxreg <- cbind(fcX0_1$mean, fcX0_2$mean)

# 1. model & forecast w/ xreg
tsnb <- ts(DF$y, start=2017, freq=365)
fcX1 <- stlf(tsnb, h=365, method="arima", xreg=xreg, newxreg=newxreg)
autoplot(fcX1)
model1_xreg <- c(fcX1$fitted, fcX1$mean)
write.csv(model1_xreg, "1_model_xreg_3_amr_seo3.csv")

# ///////////////////////////////////////////////////////////////////////////
# SARIMAX
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# 3/28/2019
library(dplyr)
library(forecast)
setwd("/users/akuppam/documents/Hprog/Py/AllMetrics_Jan31/xreg/LatestData/")
DFagg <- read.csv("latest_data_amr.csv")
DF = DFagg[(which((DFagg[,'rlt'] == 'Display'))),]
summary(DF)
# 1 ------
tsnb <- ts(DF$y, start=2017, freq=365)
model_sorder <- auto.arima(tsnb, D=1) # takes in a ts(object) as input data and outputs a sarima model
asorder <- model_sorder$arma[c(1, 6, 2, 3, 7, 4, 5)]
names(asorder) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
# 2 ------
tsvisits <- ts(DF$xreg1, start=2017, freq=365)
model_sorder_x <- auto.arima(tsvisits, D=1)
asorder_x <- model_sorder_x$arma[c(1, 6, 2, 3, 7, 4, 5)]
names(asorder_x) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
# 3 ------
tslistings <- ts(DF$xreg2, start=2017, freq=365)
model_sorder_x2 <- auto.arima(tslistings, D=1)
asorder_x2 <- model_sorder_x2$arma[c(1, 6, 2, 3, 7, 4, 5)]
names(asorder_x2) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
# --------
xreg1 <- tsvisits
xreg2 <- tslistings
xreg <- cbind(xreg1, xreg2)

m <- arima(tsnb, order=c(asorder[1],asorder[2],asorder[3]),
           seasonal=list(order=c(asorder[4],asorder[5],asorder[6]), period = 365), 
           xreg = data.frame(xreg))
asorder <- asorder_x
m_x <- arima(tsvisits, order=c(asorder[1],asorder[2],asorder[3]),
           seasonal=list(order=c(asorder[4],asorder[5],asorder[6]), period = 365))
asorder <- asorder_x2
m_x2 <- arima(tslistings, order=c(asorder[1],asorder[2],asorder[3]),
             seasonal=list(order=c(asorder[4],asorder[5],asorder[6]), period = 365))

f_x <- forecast(m_x, h = 365)
autoplot(f_x)
f_x2 <- forecast(m_x2, h = 365)
autoplot(f_x2)

newxreg <- cbind(f_x$mean, f_x2$mean)
f <- forecast(m, h = 365, xreg = unclass(newxreg))
autoplot(f)
f_sarima <- c(f$fitted, f$mean)
write.csv(f_sarima, "sarimaX_amr_display3.csv")
# /////////////////////////////////////////////////////////////////////////////////////
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
