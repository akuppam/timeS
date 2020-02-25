# #####################################
# 3
# Generate plots
# #####################################

# Plot forecasts from different methods
df_rnb <- c(rnb, rep(NA,h))
df1 <- c(rnb, st_ets)
df6 <- c(fit_arima_fc$fitted, fit_arima_fc$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)
df11 <- c(fit_hw_fc$fitted, fit_hw_fc$mean)

dfreal <- c(DF$y, rep(NA,h))
df_all <- data.frame(dfreal,dfprophet,dfprophet_visits,dfprophet_visits_hw,df6,df11)
names(df_all) <- c("real","prophet","prophet_visits","prophet_visits_hw","arima","hw")
#write.csv(df_all, "df_all_agg.csv")
write.csv(df_all, file = paste0(prefix, "_df_all_agg.csv"))

# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(df_all), 
                                by="days"),
                      data = df_all)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg((paste0(prefix, "_real vs all models.jpg")), height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models")
return(print(p))
dev.off()
# ----- forecasts ONLY -----------
df_all_f <- df_all[(nrow(DF)+1):nrow(df_all),]
# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2018-10-16"))),
                                length.out = nrow(df_all_f), 
                                by="days"),
                      data = df_all_f)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg((paste0(prefix, "_real vs all models - Forecasts ONLY.jpg")), height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models - forecasts ONLY")
return(print(p))
dev.off()
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
