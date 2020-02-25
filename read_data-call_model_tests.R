# ########################
# 1
# Load dataset
# Subset based on FBU x RLT
# Call R code to run models
# (ETS, HW, Arima, Prophet)
# Plot forecasts
# ########################
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
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# load dataset
library(dplyr)
setwd("/users/akuppam/documents/Hprog/R/Prophet/")
DFagg <- read.csv("rnb1015_2.csv")
DFagg <- mutate(DFagg, ds = as.Date(date))

setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/AMR_paid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/AMR_nonpaid/")
region = 'AMR'
DF = DFagg[(which((DFagg[,'region'] == 'AMR') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/UK_paid/")
region = 'UK'
DF = DFagg[(which((DFagg[,'region'] == 'UK') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/UK_nonpaid/")
region = 'UK'
DF = DFagg[(which((DFagg[,'region'] == 'UK') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/FR_paid/")
region = 'FR'
DF = DFagg[(which((DFagg[,'region'] == 'FR') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/FR_nonpaid/")
region = 'FR'
DF = DFagg[(which((DFagg[,'region'] == 'FR') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/CE_paid/")
region = 'CE'
DF = DFagg[(which((DFagg[,'region'] == 'CE') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/CE_nonpaid/")
region = 'CE'
DF = DFagg[(which((DFagg[,'region'] == 'CE') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/SoEu_paid/")
region = 'SoEu'
DF = DFagg[(which((DFagg[,'region'] == 'SoEu') & (DFagg[,'marketing'] == 'Paid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
setwd("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/SoEu_nonpaid/")
region = 'SoEu'
DF = DFagg[(which((DFagg[,'region'] == 'SoEu') & (DFagg[,'marketing'] == 'NotPaid'))),]
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/tsmodels_tests.R")
source("/users/akuppam/documents/Hprog/R/Prophet/Test9v3speed/plots_tests.R")
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# TESTS BELOW.....2/5/19

# dummy data
df <- read.table(text="Name  Math Science PE
                 David  90    70    25
                 Tom    100   60    40
                 John   30    40    100", header = TRUE)

# load dataset
library(dplyr)
#setwd("/users/akuppam/documents/Hprog/R/Prophet/")
df <- read.csv("AllData.csv")
df <- mutate(df, ds = as.Date(date))
summary(df)

# reading each column and writing out files w/ col headers as filenames
# loop and write csv for top 2 scores
lapply(colnames(df)[2:ncol(df)], function(i){
  DF <- df[, c("", i)]
  #res <- res[order(res[, i], decreasing = TRUE),]
  #write.csv(head(res, 2), file = paste0(i, ".csv"))
  #write.csv(res, file = paste0(i, ".csv"))
  #source("tsmodels_tests.R")
  #source("plots_tests.R")
})

# ===============
# re-installing R
# (1) before uninstalling, save all R packages to a file....use the link below
# http://ianmadd.github.io/pages/EasyRPackageReinstall.html
# (2) to uninstall manually....use link below
# https://osxuninstaller.com/uninstall-guides/uninstall-r/
# (3) after removing R and R studio contents from Mac and Trash.....install R and RStudio....use link below
# https://medium.com/@GalarnykMichael/install-r-and-rstudio-on-mac-e911606ce4f4
# (4) after re-installing R and RStudio, load all R packages that were saved to a file....use the link below
# http://ianmadd.github.io/pages/EasyRPackageReinstall.html
# ===============

setwd("/Users/akuppam/Documents/Hprog/R/")
packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages")
View(packages)  # 234 packages

setwd("/Users/akuppam/Documents/Hprog/R/")
load("Rpackages") 
for (p in setdiff(packages, installed.packages()[,"Package"]))
install.packages(p)

setwd("/Users/akuppam/Documents/Hprog/R/")
packages2 <- installed.packages()[,"Package"]
save(packages2, file="Rpackages2")
View(packages2)   # 240 packages

