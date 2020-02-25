# ########################
# Load dataset
# Subset based on FBU x RLT
# Call R code to run models
# (ETS, HW, Arima, Prophet)
# Plot forecasts
# ########################

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/R/Prophet/")
DFagg <- read.csv("rnb1015.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

setwd("/users/akuppam/documents/Hprog/R/Prophet/AMR_paid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/AMR_nonpaid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/UK_paid/")
region = 'UK'
DF = DFagg[(which((DFagg[,'region'] == 'UK') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/UK_nonpaid/")
region = 'UK'
DF = DFagg[(which((DFagg[,'region'] == 'UK') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/FR_paid/")
region = 'FR'
DF = DFagg[(which((DFagg[,'region'] == 'FR') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/FR_nonpaid/")
region = 'FR'
DF = DFagg[(which((DFagg[,'region'] == 'FR') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/CE_paid/")
region = 'CE'
DF = DFagg[(which((DFagg[,'region'] == 'CE') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/CE_nonpaid/")
region = 'CE'
DF = DFagg[(which((DFagg[,'region'] == 'CE') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/SoEu_paid/")
region = 'SoEu'
DF = DFagg[(which((DFagg[,'region'] == 'SoEu') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/SoEu_nonpaid/")
region = 'SoEu'
DF = DFagg[(which((DFagg[,'region'] == 'SoEu') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/plots_tests.R")
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

