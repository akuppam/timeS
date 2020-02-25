# ######################################################################
# 5
# pick the best model (forecasts) for each region and marketing channel
# ######################################################################
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/AMR_nonpaid/")
df100 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[1] == 'prophet') {
  df100_f <- df100$prophet
} else if(df_min_mapes$Model[1] == 'prophet_visits') {
  df100_f <- df100$prophet_visits
} else if(df_min_mapes$Model[1] == 'arima') {
  df100_f <- df100$arima
} else if(df_min_mapes$Model[1] == 'sarima') {
  df100_f <- df100$sarima
} else {
  df100_f <- df100$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/AMR_paid/")
df200 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[2] == 'prophet') {
  df200_f <- df200$prophet
} else if(df_min_mapes$Model[2] == 'prophet_visits') {
  df200_f <- df200$prophet_visits
} else if(df_min_mapes$Model[2] == 'arima') {
  df200_f <- df200$arima
} else if(df_min_mapes$Model[2] == 'sarima') {
  df200_f <- df200$sarima
} else {
  df200_f <- df200$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/CE_nonpaid/")
df300 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[3] == 'prophet') {
  df300_f <- df300$prophet
} else if(df_min_mapes$Model[3] == 'prophet_visits') {
  df300_f <- df300$prophet_visits
} else if(df_min_mapes$Model[3] == 'arima') {
  df300_f <- df300$arima
} else if(df_min_mapes$Model[3] == 'sarima') {
  df300_f <- df300$sarima
} else {
  df300_f <- df300$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/CE_paid/")
df400 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[4] == 'prophet') {
  df400_f <- df400$prophet
} else if(df_min_mapes$Model[4] == 'prophet_visits') {
  df400_f <- df400$prophet_visits
} else if(df_min_mapes$Model[4] == 'arima') {
  df400_f <- df400$arima
} else if(df_min_mapes$Model[4] == 'sarima') {
  df400_f <- df400$sarima
} else {
  df400_f <- df400$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/FR_nonpaid/")
df500 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[5] == 'prophet') {
  df500_f <- df500$prophet
} else if(df_min_mapes$Model[5] == 'prophet_visits') {
  df500_f <- df500$prophet_visits
} else if(df_min_mapes$Model[5] == 'arima') {
  df500_f <- df500$arima
} else if(df_min_mapes$Model[5] == 'sarima') {
  df500_f <- df500$sarima
} else {
  df500_f <- df500$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/FR_paid/")
df600 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[6] == 'prophet') {
  df600_f <- df600$prophet
} else if(df_min_mapes$Model[6] == 'prophet_visits') {
  df600_f <- df600$prophet_visits
} else if(df_min_mapes$Model[6] == 'arima') {
  df600_f <- df600$arima
} else if(df_min_mapes$Model[6] == 'sarima') {
  df600_f <- df600$sarima
} else {
  df600_f <- df600$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/SoEu_nonpaid/")
df700 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[7] == 'prophet') {
  df700_f <- df700$prophet
} else if(df_min_mapes$Model[7] == 'prophet_visits') {
  df700_f <- df700$prophet_visits
} else if(df_min_mapes$Model[7] == 'arima') {
  df700_f <- df700$arima
} else if(df_min_mapes$Model[7] == 'sarima') {
  df700_f <- df700$sarima
} else {
  df700_f <- df700$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/SoEu_paid/")
df800 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[8] == 'prophet') {
  df800_f <- df800$prophet
} else if(df_min_mapes$Model[8] == 'prophet_visits') {
  df800_f <- df800$prophet_visits
} else if(df_min_mapes$Model[8] == 'arima') {
  df800_f <- df800$arima
} else if(df_min_mapes$Model[8] == 'sarima') {
  df800_f <- df800$sarima
} else {
  df800_f <- df800$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/UK_nonpaid/")
df900 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[9] == 'prophet') {
  df900_f <- df900$prophet
} else if(df_min_mapes$Model[9] == 'prophet_visits') {
  df900_f <- df900$prophet_visits
} else if(df_min_mapes$Model[9] == 'arima') {
  df900_f <- df900$arima
} else if(df_min_mapes$Model[9] == 'sarima') {
  df900_f <- df900$sarima
} else {
  df900_f <- df900$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/UK_paid/")
df1000 <- read.csv("df_all_agg.csv")
if(df_min_mapes$Model[10] == 'prophet') {
  df1000_f <- df1000$prophet
} else if(df_min_mapes$Model[10] == 'prophet_visits') {
  df1000_f <- df1000$prophet_visits
} else if(df_min_mapes$Model[10] == 'arima') {
  df1000_f <- df1000$arima
} else if(df_min_mapes$Model[10] == 'sarima') {
  df1000_f <- df1000$sarima
} else {
  df1000_f <- df1000$hw
}
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/")
df_best_forecasts <- data.frame(df100_f,df200_f,df300_f,df400_f,df500_f,
                                df600_f,df700_f,df800_f,df900_f,df1000_f)
names(df_best_forecasts) <- c("AMR_nonpaid","AMR_paid","CE_nonpaid","CE_paid","FR_nonpaid",
                              "FR_paid","SoEu_nonpaid","SoEu_paid","UK_nonpaid","UK_paid")
write.csv(df_best_forecasts, "df_best_forecasts.csv")
# -------------------------------------------------------------------------
summary(df_best_forecasts)
# -------------------------------------------------------------------------
#df_best_forecasts$nb_forecasts <- colSums(df_best_forecasts)
df_best_forecasts$nb_forecasts <- (df_best_forecasts$AMR_nonpaid+
                                        df_best_forecasts$AMR_paid+
                                        df_best_forecasts$CE_nonpaid+
                                        df_best_forecasts$CE_paid+
                                        df_best_forecasts$FR_nonpaid+
                                        df_best_forecasts$FR_paid+
                                        df_best_forecasts$SoEu_nonpaid+
                                        df_best_forecasts$SoEu_paid+
                                        df_best_forecasts$UK_nonpaid+
                                        df_best_forecasts$UK_paid)
plot(df_best_forecasts$nb_forecasts)
# ----- 'net bookings' forecasts ONLY -----------
# adding date as index
df_best_forecasts1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(df_best_forecasts), 
                                by="days"),
                      data = df_best_forecasts)
dfplot <- df_best_forecasts1 %>% gather(key, value, -dates)

jpeg("net bookings.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("net bookings")
return(print(p))
dev.off()
# ----- 'net bookings' forecasts AMR ONLY -----------
temp_AMR <- data.frame(df_best_forecasts$AMR_nonpaid, df_best_forecasts$AMR_paid)
temp_AMR1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                           length.out = nrow(temp_AMR), 
                                           by="days"),
                                 data = temp_AMR)
dfplot <- temp_AMR1 %>% gather(key, value, -dates)

jpeg("net bookings AMR only.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("net bookings forecasts - AMR only")
return(print(p))
dev.off()
# ----- 'net bookings' forecasts EU ONLY -----------
temp_EUR <- data.frame(df_best_forecasts$CE_nonpaid,
                         df_best_forecasts$CE_paid,
                         df_best_forecasts$FR_nonpaid,
                         df_best_forecasts$FR_paid,
                         df_best_forecasts$SoEu_nonpaid,
                         df_best_forecasts$SoEu_paid,
                         df_best_forecasts$UK_nonpaid,
                         df_best_forecasts$UK_paid)
temp_EUR1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                  length.out = nrow(temp_EUR), 
                                  by="days"),
                        data = temp_EUR)
dfplot <- temp_EUR1 %>% gather(key, value, -dates)

jpeg("net bookings EUR only.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("net bookings forecasts - EUR only")
return(print(p))
dev.off()
