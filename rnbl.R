# ###########################################################
# R code for linear additive regression, prophet, bsts models
# Backup (bsts_rnbl.R in ark project)
# ###########################################################

#Forecast Y = NB USING x = MULTIPLE PREDICTORS AND LINEAR REGRESSION (visits, br, listings)
library(corrplot)
library(plotly)
library(prophet)
library(tidyverse)
library(bsts)  
library(prophet)

setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl_log.csv")
str(DF)
summary(DF)

#Visualize correlation between variables
newdatacor = cor(DF[2:10])
corrplot(newdatacor, method = "number", number.digits = 2)

# nb ~ visits, br, listings (highest correlations)

#Perform aditive multiple Xs linear regression
v_predictor <- c("visits","br", "listings")

v_response <- "nb"

f <- paste(v_predictor, collapse = "+")
#f <- paste(f, "-1")  # specify intercept
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
f

fit <- lm(f, data = DF)
summary(fit)

#Plot relation y_dependent & x_independent
plot(DF$nb, DF$visits)
abline(lm(DF$nb ~ DF$visits))

#Plot relation y_dependent & x_independent
plot(DF$nb, DF$br)
abline(lm(DF$nb ~ DF$br))

#Plot relation y_dependent & x_independent
plot(DF$nb, DF$listings)
abline(lm(DF$nb ~ DF$listings))

#Predict fitted values into dataframe
Y1 <- fit$fitted.values
Y_value <- as.data.frame(Y1)
write.csv(Y_value, file = "y_value_v1.csv")

#Export coefficients into data frame
write.csv(data.frame(summary(fit)$coefficients), file="coeff2.csv")

#manually check the Betas (and compare if fitted values match)
coeffs <- coefficients(fit)
coeffs
# logDF$visits * coeffs[1] + logDF$br *coeffs[2] +  logDF$listings * coeffs[3]

#Plot Real vs Baseline
Date <- DF$date
nb_Real <- DF$nb
nb_forecast <- fit$fitted.values

data <- data.frame(Date,  nb_Real, nb_forecast)
data$Date <- factor(data$Date, levels = data[["Date"]], ordered = TRUE)

# MAPE of the lineal model
mean(abs((data$nb_Real - data$nb_forecast)/data$nb_Real))

p <- plot_ly(data, x = ~Date, y = ~nb_Real, name = 'Real', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(105, 12, 24)', width = 3)) %>%
  add_trace(y = ~nb_forecast, name = 'Forecast', line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
  layout(title = "Real nb vs Predicted nb",
         xaxis = list(title = "Date"),
         yaxis = list (title = "nb Lm model"))

print(p)

# ----------------------
# Prophet Model
# ----------------------

library(corrplot)
library(plotly)
library(prophet)
library(tidyverse)
library(bsts)  
library(prophet)

# Call function with prophet to fit the model based on the historical data
setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl_log.csv")
str(DF)
summary(DF)

colnames(DF) <- c("ds","visits","br","inq","gb","cb","y","ss","ts","listings")
# format should be what's in the data (as.Date will convert it to %Y-%m-%d that is reqd for prophet)
DF$ds <- as.Date(DF$ds, format = "%m/%d/%y")
View(DF)

m <- prophet(DF)

#Forecast future by default the model will also include historical dates
future <- make_future_dataframe(m, periods = 30)
tail(future)

#Predict the future outcome into a new data frame
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Mape of Prophet
Date <- DF$ds
nb_Real <- DF$y
nb_Prophet <- subset(forecast, ds <'2017-12-31')
nb_Prophet_m <- nb_Prophet$yhat 

str(nb_Prophet)
str(nb_Prophet$yhat)

p_data <- data.frame(Date,  nb_Real, nb_Prophet_m)
mean(abs((p_data$nb_Real - p_data$nb_Prophet_m)/p_data$nb_Real))

#Plot Real vs Baseline Prophet
p_data <- data.frame(Date,  nb_Real, nb_Prophet_m)
data$Date <- factor(data$Date, levels = data[["Date"]])

p <- plot_ly(p_data, x = ~Date, y = ~nb_Real, name = 'Real', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(0,250,154)', width = 3)) %>%
  add_trace(y = ~nb_Prophet_m, name = 'Forecast', line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
  layout(title = "Real nb vs Predicted nb",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Prophet model"))

print(p)

# ---------------------------
# bsts model 
# ---------------------------

library(corrplot)
library(plotly)
library(prophet)
library(tidyverse)
library(bsts)  
library(prophet)

setwd("/users/akuppam/documents/Data/RoverData/")
DF <- read.csv("rnbl_log.csv")
str(DF)
summary(DF)

#Rename columns for BSTS
#colnames(DF) <- c("date","visits","br","inq","gb","cb","nb","ss","ts","listings")

#Create a new variable for BSTS Model
bsts_response <- DF$nb
(model_components <- list())
AddLocalLinearTrend #prefered trend selector assuming there is a linear regressions between time series
plot(DF$nb, ylab = "")

#Trend component
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = DF$nb))
#seasonal component
summary(model_components <- AddSeasonal(model_components, y = DF$nb, 
                                        nseasons  = 12, season.duration = 30))


#Construct bsts model that only takes into account seasonality and trend
st_bsts_fit <- bsts(DF$nb, model_components, niter = 1000)

#predict with 95% interval of confidence only using seasonality and trend
pred <- predict(st_bsts_fit, horizon = 30, quantiles = c(.05, .95))
plot(pred)

plot(st_bsts_fit)

#Construct BSTS model using seasonality & trend and Xi regressors
bsts_fit <- bsts(nb ~ visits + br + listings + inq + gb + cb + ss + ts, state.specification = model_components, 
                 data = DF, niter = 1000)

plot(bsts_fit)
plot(bsts_fit, "predictors")
plot(bsts_fit, "coef")

colMeans(bsts_fit$coefficients)

plot(bsts_fit, "comp")

#Construct BSTS model using seasonality & trend and Xi regressors forcing inclusion
bsts_fit1 <- bsts(nb ~ visits + br + listings + inq + gb + cb + ss + ts, state.specification = model_components, 
                  data = DF, niter = 1000, expected.model.size = 10) #passed to spike and slab

plot(bsts_fit1)
plot(bsts_fit1, "predictors")
plot(bsts_fit1, "coef")

colMeans(bsts_fit1$coefficients)

plot(bsts_fit1, "comp")


#Construct BSTS model using seasonality & trend and Xi regressors forcing specification
bsts_fit2 <- bsts(nb ~ visits + br + listings, state.specification = model_components, 
                  data = DF, niter = 1000, expected.model.size = 10) #passed to spike and slab

plot(bsts_fit2)
plot(bsts_fit2, "predictors")
plot(bsts_fit2, "coef")

colMeans(bsts_fit2$coefficients)

plot(bsts_fit2, "comp")

#view coeffs
summary(st_bsts_fit)
summary(bsts_fit)
summary(bsts_fit1)
summary(bsts_fit2)

#compare the BSTS models
CompareBstsModels(list("ST" = st_bsts_fit,
                       "ST + reg" = bsts_fit,
                       "ST + forced reg" = bsts_fit1,
                       "ST + specified reg" = bsts_fit2),
                  colors = c("black", "red", "blue", "green"))

# 1st bsts model - validation metrics

#predict based on times series seasonality, trends, cycles, Xi predictors and best model.
pred_bsts <- predict(bsts_fit, newdata = DF, horizon = 120, quantiles = c(.05, .95))
plot(pred_bsts)

# MAPE of Bsts
Date <- DF$date
nb_Real <- DF$nb
nb_Bsts <- pred_bsts$mean

bs_data <- data.frame(Date,  nb_Real , nb_Bsts)
summary(bs_data)
mean(abs((bs_data$nb_Real - bs_data$nb_Bsts)/bs_data$nb_Real))

#Plot Real vs Baseline Of Bsts model
Date <- DF$date
Bsts_nb_Real <- DF$nb
Bsts_nb_forecast <- nb_Bsts

data <- data.frame(Date,  Bsts_nb_Real, Bsts_nb_forecast)
data$Date <- factor(data$Date, levels = data[["Date"]])

p <- plot_ly(data, x = ~Date, y = ~Bsts_nb_Real, name = 'Real', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(207,181,59)', width = 3)) %>%
  add_trace(y = ~Bsts_nb_forecast, name = 'Forecast', line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
  layout(title = "Bsts Model Real nb vs Predicted nb",
         xaxis = list(title = "Date"),
         yaxis = list (title = "nb Lm model"))

print(p)

# 2nd bsts model - validation metrics

#predict based on times series seasonality, trends, cycles, Xi predictors and best model.
pred_bsts1 <- predict(bsts_fit1, newdata = DF, horizon = 120, quantiles = c(.05, .95))
plot(pred_bsts1)

# MAPE of Bsts
Date <- DF$date
nb_Real <- DF$nb
nb_Bsts1 <- pred_bsts1$mean

bs_data <- data.frame(Date,  nb_Real , nb_Bsts1)
summary(bs_data)
bs_data$Date <- factor(bs_data$Date, levels = bs_data[["Date"]])

mean(abs((bs_data$nb_Real - bs_data$nb_Bsts1)/bs_data$nb_Real))

p <- plot_ly(bs_data, x = ~Date, y = ~nb_Real, name = 'Real', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(207,181,59)', width = 3)) %>%
  add_trace(y = ~nb_Bsts1, name = 'Forecast', line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
  layout(title = "Bsts Model Real nb vs Predicted nb",
         xaxis = list(title = "Date"),
         yaxis = list (title = "nb Lm model"))

print(p)


# 3rd bsts model - validation metrics

#predict based on times series seasonality, trends, cycles, Xi predictors and best model.
pred_bsts2 <- predict(bsts_fit2, newdata = DF, horizon = 120, quantiles = c(.05, .95))
plot(pred_bsts2)

# MAPE of Bsts
Date <- DF$date
nb_Real <- DF$nb
nb_Bsts2 <- pred_bsts2$mean

bs2_data <- data.frame(Date,  nb_Real , nb_Bsts2)
summary(bs2_data)
mean(abs((bs2_data$nb_Real - bs2_data$nb_Bsts2)/bs2_data$nb_Real))

#Plot Real vs Baseline of Bsts model
Date <- DF$date
Bsts2_nb_Real <- DF$nb
Bsts2_nb_forecast <- nb_Bsts2

data <- data.frame(Date,  Bsts2_nb_Real, Bsts2_nb_forecast)
data$Date <- factor(data$Date, levels = data[["Date"]])

p <- plot_ly(data, x = ~Date, y = ~Bsts2_nb_Real, name = 'Real', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(207,181,59)', width = 3)) %>%
  add_trace(y = ~Bsts2_nb_forecast, name = 'Forecast', line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
  layout(title = "Bsts Model Real nb vs Predicted nb",
         xaxis = list(title = "Date"),
         yaxis = list (title = "nb Lm model"))

print(p)

