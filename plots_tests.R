# #####################################
# 3
# Generate plots
# #####################################
# ########################
#This script uses parallel processing if p.flag=TRUE.  Set up a parallel 
#cluster as appropriate for your machine as appropriate. (the commented code 
#below will use 2 cores on a multicore computer)
library(doParallel)
cl <- makeCluster(8)  # Use 2 cores
registerDoParallel(cl) # register these 2 cores with the "foreach" package
library(plyr)
p.flag=TRUE  # Change to TRUE if using parallel processing
# ########################

# Plot forecasts from different methods
df_rnb <- c(rnb, rep(NA,h))
df1 <- c(rnb, st_ets)
df6 <- c(fit_arima_fc$fitted, fit_arima_fc$mean) # Using 'forecast()' function: mean = forecasts (n=120); fitted = backcasts (n=998)
df7 <- c(fit_sarima_fc$fitted, fit_sarima_fc$mean)
df11 <- c(fit_hw_fc$fitted, fit_hw_fc$mean)

dfreal <- c(DF$y, rep(NA,h))
df_all <- data.frame(dfreal,dfprophet,dfprophet_visits,dfprophet_visits_hw,df6,df11,df7)
names(df_all) <- c("real","prophet","prophet_visits","prophet_visits_hw","arima","hw","sarima")
write.csv(df_all, "df_all_agg.csv")

# adding date as index
df_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(df_all), 
                                by="days"),
                      data = df_all)
dfplot <- df_all1 %>% gather(key, value, -dates)

jpeg("real vs all models.jpg", height=4.25, width=5.5, res=200, units = "in")
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

jpeg("real vs all models - forecasts ONLY.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(dfplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("real vs all models - forecasts ONLY")
return(print(p))
dev.off()
# ------------------------------
# other plots
# ------------------------------
# visualize seasonality
# prophet:::plot_yearly(fit)
# prophet_plot_components(model, forecast)

# components plots 
comp_all <- data.frame(trend,hols,reg,wkly,yrly,yhat)
names(comp_all) <- c('trend','hols','reg','wkly','yrly','yhat')
# adding date as index
comp_all1 <- data.frame(dates=seq(from=(as.POSIXct(strftime("2016-01-01"))),
                                length.out = nrow(comp_all), 
                                by="days"),
                      data = comp_all)
compplot <- comp_all1 %>% gather(key, value, -dates)

jpeg("components.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(compplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("components analysis")
return(print(p))
dev.off()
# components w/ Hols only -------------
comp_all <- data.frame(dates,trend,hols,reg,wkly,yrly,yhat)
names(comp_all) <- c('dates','trend','hols','reg','wkly','yrly','yhat')
write.csv(comp_all, "comp_all.csv")

comp_all_hols <- comp_all[(which(comp_all[,'hols'] != 0)),]
compplot <- comp_all_hols %>% gather(key, value, -dates)

jpeg("components w Hols only.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(compplot, mapping = aes(x = dates, y = value, color = key) ) + geom_point() + 
  ggtitle("components analysis - Hols only")
return(print(p))
dev.off()

jpeg("components w Hols only line.jpg", height=4.25, width=5.5, res=200, units = "in")
p <- ggplot(compplot, mapping = aes(x = dates, y = value, color = key) ) + geom_line() + 
  ggtitle("components analysis - Hols only")
return(print(p))
dev.off()
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
