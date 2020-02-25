# -------------------------------------------------------------
# -------------------------------------------------------------
# examining distributions of 'stay percentages' by year/month
# -------------------------------------------------------------
# -------------------------------------------------------------
if(!sum(installed.packages()[,1]=="fitdistrplus")){install.packages("fitdistrplus")}

library(Matrix)
library(survival)
library(fitdistrplus)

stay <- read.csv("/Users/akuppam/Documents/Stay/stay.csv")
View(stay)
stay2 <- read.csv("/Users/akuppam/Documents/Stay/stay2.csv")
View(stay2)

str(stay2)
plotdist(stay2$X2017.1, histo = TRUE, demp = TRUE, na.rm = TRUE)
summary(stay2$X2017.1)
descdist(stay2$X2017.1)

fw <- fitdist(stay2$X2017.1, "weibull")
summary(fw)

fw <- fitdist(stay2$X2017.1, "weibull")
fg <- fitdist(stay2$X2017.1, "gamma")
fln <- fitdist(stay2$X2017.1, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)

# ----------------

fw <- fitdist(stay2$X2017.2, "weibull")
fg <- fitdist(stay2$X2017.2, "gamma")
fln <- fitdist(stay2$X2017.2, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)

# ------------------
# stay2 <- na.omit(stay2)  # this removes all rows that has atleast 1 NA - not really correct way

plotdist(stay2$X2017.1, histo = TRUE, demp = TRUE)
plotdist(stay2$X2018.1, histo = TRUE, demp = TRUE)
plotdist(stay2$X2016.2, histo = TRUE, demp = TRUE)
plotdist(stay2$X2017.2, histo = TRUE, demp = TRUE)

summary(stay2)


# ------------
# synthpop
# -----------

if(!sum(installed.packages()[,1]=="synthpop")){install.packages("synthpop")}

library(synthpop)

summary(SD2011)

vars <- c("sex", "agegr", "placesize", "edu", "socprof", "marital", "income", "smoke")
ods <- SD2011[, vars]
sds <- syn(ods)
sds

sds <- syn(ods, m = 5, method = "cart", cart.minbucket = 10, 
           cont.na = list(income = c(NA, -8)), smoothing = list(income = "density"))

compare(sds, ods, vars = "income")

# ------

library(synthpop)
summary(stay)

vars <- c("mon1","mon2","mon3","mon4","mon5","mon6","mon7","mon8","mon9","mon10")
ods <- stay[, vars]
sds <- syn(ods)
sds

sds <- syn(ods, m = 5, method = "cart", cart.minbucket = 10, 
           cont.na = list(income = c(NA, -8)), smoothing = list(income = "density"))

compare(sds, ods, vars = "mon1")

# --------
# reading 2015,2016,2017,2018 booking reservation data

# takes about 7:15 to read the 3.249 GB fil5

library(data.table)  # for fast and memory efficient reading of large files
setwd("/users/akuppam/documents/Data/RoverData/")
data2015 <- data.table(read.csv("BookingReservationData_NA_2015.csv"), header = TRUE)
summary(data2015)
str(data2015)

library(data.table)  # for fast and memory efficient reading of large files
setwd("/users/akuppam/documents/Data/RoverData/")
data2016 <- data.table(read.csv("BookingReservationData_NA_2016.csv"), header = TRUE)
# takes about 7:15 to read the 3.249 GB file
summary(data2016)
str(data2016)

library(data.table)  # for fast and memory efficient reading of large files
setwd("/users/akuppam/documents/Data/RoverData/")
data2017 <- data.table(read.csv("BookingReservationData_NA_2017.csv"), header = TRUE)
# takes about 7:15 to read the 3.249 GB file
summary(data2017)
str(data2017)

library(data.table)  # for fast and memory efficient reading of large files
setwd("/users/akuppam/documents/Data/RoverData/")
data2018 <- data.table(read.csv("BookingReservationData_NA_20180925.csv"), header = TRUE)
# takes about 7:15 to read the 3.249 GB file
summary(data2018)
str(data2018)

table(data2015$totalParty)
hist(table(data2015$totalParty))
plot(table(data2015$totalParty))
summary(data2015$totalParty)

table(data2016$totalParty)
hist(table(data2016$totalParty))
plot(table(data2016$totalParty))
summary(data2016$totalParty)

table(data2017$totalParty)
hist(table(data2017$totalParty))
plot(table(data2017$totalParty))
summary(data2017$totalParty)

table(data2018$totalParty)
hist(table(data2018$totalParty))
plot(table(data2018$totalParty))
summary(data2018$totalParty)

smoothScatter(data2015$lenghtOfStay, data2015$totalParty)

prop.table(table(data2015$Country))
table(data2015$Country)

prop.table(table(data2016$Country))
table(data2016$Country)

prop.table(table(data2017$Country))
table(data2017$Country)

prop.table(table(data2018$Country))
table(data2018$Country)
