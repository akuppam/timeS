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
# Call function with prophet to fit the model based on the historical data
setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl2agg.csv")
colnames(DF) <- c("ds","region","marketing","visits","br","inq","gb","cb","y","ss","ts","listings")

m <- prophet(DF)
summary(m$history)
summary(m$history.dates)

#Forecast future by default will also include historical dates
future <- make_future_dataframe(m, periods = 120, freq = 'day')
#Predict the future outcome into a new data frame
forecast <- predict(m, future)
write.csv(forecast, 'forecast_prophet_rnbl2agg.csv')
dfprophet <- forecast$yhat

#Mape of Prophet
Date <- DF$ds
nb_Real <- DF$y
nb_Prophet <- subset(forecast, ds < '2018-09-24')
nb_Prophet_m <- nb_Prophet$yhat 

p_data <- data.frame(Date,  nb_Real, nb_Prophet_m)
mape_prophet_agg <- mean(abs((p_data$nb_Real - p_data$nb_Prophet_m)/p_data$nb_Real))
write.csv(p_data, "p_data_rnbl2agg.csv")

#Plot Real vs Baseline Prophet
p_data <- data.frame(Date,  nb_Real, nb_Prophet_m)
p_data$Date <- factor(p_data$Date, levels = p_data[["Date"]], ordered = TRUE)
summary(p_data)

p <- plot_ly(p_data, x = ~Date, y = ~nb_Real, name = 'Real', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(0,250,154)', width = 3)) %>%
  add_trace(y = ~nb_Prophet_m, name = 'Forecast', line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
  layout(title = "Real vs Predicted - Prophet Model (rnbl2agg)",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Net Bookings"))

print(p)

# Add the regression line
ggplot(p_data, aes(x=nb_Real, y=nb_Prophet_m)) + 
  geom_point()+
  geom_smooth(method=lm)

jpeg("real vs prophet.jpg", height=4.25, width=5.5, res=200, units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + ggtitle("real vs prophet")
dev.off()

# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl2agg.csv")
DF <- mutate(DF, ds = as.Date(date))

# adding regressors
library(dplyr)
colnames(DF) <- c("date","region","marketing","visits","br","inq","gb","cb","y","ss","ts","listings","ds")
# -------------------
# -------------------
pdat <- data.frame(ds=DF$ds, y=DF$y, visits=DF$visits, br=DF$br, listings=DF$listings)
pfdat <- data.frame(ds=max(DF$ds) + 1:120)
pvisits <- DF %>% dplyr::select(ds,y=visits) %>% prophet() %>% predict(pfdat)
fdat <-  data.frame(ds=pfdat$ds, visits=pvisits$yhat)

fit6 <- prophet() %>% 
  add_regressor('visits') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

# plot components (trend, weekly, yearly, extra regressors for h=120 days)
plot(fit6, forecast)
prophet_plot_components(fit6, forecast)

dfprophet_visits <- c(fpred$yhat, forecast$yhat)
# -------------------
# -------------------
pdat <- data.frame(ds=DF$ds, y=DF$y, br=DF$br)
pfdat <- data.frame(ds=max(DF$ds) + 1:120)
pbr <- DF %>% dplyr::select(ds,y=br) %>% prophet() %>% predict(pfdat)
fdat <-  data.frame(ds=pfdat$ds, br=pbr$yhat)

fit6 <- prophet() %>% add_regressor('br') %>% fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

# plot components (trend, weekly, yearly, extra regressors for h=120 days)
plot(fit6, forecast)
prophet_plot_components(fit6, forecast)

dfprophet_br <- c(fpred$yhat, forecast$yhat)
# -------------------
# -------------------
pdat <- data.frame(ds=DF$ds, y=DF$y, visits = DF$visits, br=DF$br)
pfdat <- data.frame(ds=max(DF$ds) + 1:120)
pvisits <- DF %>% dplyr::select(ds,y=visits) %>% prophet() %>% predict(pfdat)
pbr <- DF %>% dplyr::select(ds,y=br) %>% prophet() %>% predict(pfdat)
fdat <-  data.frame(ds=pfdat$ds, visits=pvisits$yhat, br=pbr$yhat)

fit6 <- prophet() %>% 
  add_regressor('visits') %>% 
  add_regressor('br') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)

# residuals
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

# plot components (trend, weekly, yearly, extra regressors for h=120 days)
plot(fit6, forecast)
prophet_plot_components(fit6, forecast)

dfprophet_vibr <- c(fpred$yhat, forecast$yhat)
# -------------------
# -------------------
pdat <- data.frame(ds=DF$ds, y=DF$y, visits = DF$visits, br=DF$br, listings=DF$listings)
pfdat <- data.frame(ds=max(DF$ds) + 1:120)
pvisits <- DF %>% dplyr::select(ds,y=visits) %>% prophet() %>% predict(pfdat)
pbr <- DF %>% dplyr::select(ds,y=br) %>% prophet() %>% predict(pfdat)
plistings <- DF %>% dplyr::select(ds,y=listings) %>% prophet() %>% predict(pfdat)
fdat <-  data.frame(ds=pfdat$ds, visits=pvisits$yhat, br=pbr$yhat, listings=plistings$yhat)

fit6 <- prophet() %>% 
  add_regressor('visits') %>% 
  add_regressor('br') %>% 
  add_regressor('listings') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)

# residuals
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat

# plot components (trend, weekly, yearly, extra regressors for h=120 days)
plot(fit6, forecast)
prophet_plot_components(fit6, forecast)

dfprophet_vibrli <- c(fpred$yhat, forecast$yhat)
# ----------------------------------------------------
# ----------------------------------------------------
# plot real, forecasts
dfreal <- c(DF$y, rep(NA,120))
df_all <- data.frame(dfreal,dfprophet,dfprophet_visits,dfprophet_br,dfprophet_vibr,dfprophet_vibrli)
names(df_all) <- c("real","prophet","prophet_visits","prophet_br","prophet_vibr","prophet_vibrli")

# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(df_all), 
                                by="days"),
                      data = df_all)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs prophet models.jpg", height=4.25, width=5.5, res=200, units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + ggtitle("real vs prophet models")
dev.off()
# ----- forecasts ONLY -----------
df_all_f <- df_all[999:nrow(df_all),]
# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2018-09-25"))),
                                length.out = nrow(df_all_f), 
                                by="days"),
                      data = df_all_f)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs prophet models - forecasts ONLY.jpg", height=4.25, width=5.5, res=200, units = "in")
ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + ggtitle("real vs prophet models - forecasts ONLY")
dev.off()
# ----------------------------------------------------
