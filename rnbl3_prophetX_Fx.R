# ########################
# Load dataset
# Subset based on FBU x RLT
# Call R code to run models
# ########################

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# load dataset
setwd("/users/akuppam/documents/Hprog/R/BSTS/")
DFagg <- read.csv("rnb0924.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

setwd("/users/akuppam/documents/Hprog/R/BSTS/AMR_paid/")
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/BSTS/tsmodels.R")
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
