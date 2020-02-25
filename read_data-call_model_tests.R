# ########################
# 1
# Load dataset
# Subset based on FBU x RLT
# Call R code to run models
# (ETS, HW, Arima, Prophet)
# Plot forecasts
# ########################

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
library(corrplot)
library(plotly)
library(prophet)
library(tidyverse)
library(bsts)
library(dplyr)
library(ggplot2)

path = "/users/akuppam/documents/Hprog/R/R5x9/data/CE"
setwd(path)
my_files <- list.files(pattern = "\\.csv$")
my_data <- list()
DFagg <- list()
for (i in seq_along(my_files)) {
  my_data[[i]] <- read.csv(file = my_files[i])
  DFagg[[i]] <- mutate(my_data[[i]], ds = as.Date(date))
}

for(i in 1:length(DFagg)) {
  region = 'CE'
  DF = DFagg[[i]]
  source("/users/akuppam/documents/Hprog/R/R5x9/tsmodels_tests.R")
  source("/users/akuppam/documents/Hprog/R/R5x9/plots_tests1.R")
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
