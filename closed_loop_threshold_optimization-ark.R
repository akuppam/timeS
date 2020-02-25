#############Regression Discontinuity Design for Closed Loops V1.0#################
##########Author: Sherry Feng (qfeng@homeaway.com)##############################
###########2017-08-27########################################
#############Finalizing results##############################



library(tidyverse)
library(cluster)
library(rdrobust)
library(rddtools)
library(VGAM)
library(AER)
library(interplot)
library(rootSolve)
setwd("/Users/qfeng/Documents/RDD")

cbPalette <- c("#009933", "#FF3300")

##########using Angee's updated data, adjustments 
PrePostData <- read.csv("PrePostData.csv")
HASales_June102016_June102017<- read.csv("HASales_June102016_June102017.csv")

PrePostData[is.na(PrePostData)] <- 0
##summary(PrePostData$geogroup)

############merge with HA sales###################
HASales_June102016_June102017 <- HASales_June102016_June102017 %>% mutate(userguid = UserGUID)

PrePostData_for_use <- merge(x=PrePostData, y=HASales_June102016_June102017, by="userguid", all.x = TRUE)


#####get rid of nulls###########
PrePostData_for_use[is.na(PrePostData_for_use)] <- 0

PrePostData_for_use <- PrePostData_for_use %>% filter(nonolblistings==0,
                                                      PartnerType %in% c("FRBO", "PM"),
                                                      geogroup %in% c("Americas", "Europe"),
                                                      partnertype %in% c("FRBO", "PM"),
                                                      exceptionflag %in% c("0", "1")) %>%
  mutate(X2017Post_NBPV=100*X2017Post_KnownNetBookings/X2017Post_PV,
         X2017Pre_NBPV=100*X2017Pre_60DayKnownNetBookings/X2017Pre_60DayPV,
         conversion_increase17 = 100*(X2017Post_NBPV/X2017Pre_NBPV-1),
         Type=as.factor(ifelse(exceptionflag=="0","ClosedLoop","ExceptionList")))

PrePostData_for_use$exceptionflag <- as.numeric(levels(PrePostData_for_use$exceptionflag)[PrePostData_for_use$exceptionflag])


##PrePostData_for_use %>% filter(PartnerType=="FRBO",
  ##                             geogroup=="Americas",
    ##                           exceptionflag=="1") %>%
  ##X2017Pre_NBPV<=0.47) %>%
  ##summarise(count=n())

data_for_readout <- PrePostData_for_use %>% filter(X2017Pre_30DayPV>0,
                                                   X2017Pre_60DayPV>0,
                                                   X2017Post_PV>0) 
data_for_readout %>% filter(OwningTeam=="Europe",
                            partnertype=="PM",
                            Type=="ExceptionList",
                            X2017Pre_NBPV>=0.21) %>%
  summarise(count=n())

############filters##################
mydata <- PrePostData_for_use %>% filter(X2017Pre_60DayPV>0,
                                         X2017Pre_30DayPV>0,
                                         X2017Post_PV>0,
                                         X2017Pre_NBPV>0.07,
                                         X2017Pre_NBPV<4)

summary(mydata)

#######################deciles
mydata <- mydata %>%
  group_by(OwningTeam,
           PartnerType) %>%
  within(NBPV_quantile100 <- as.factor(as.integer(cut(X2017Pre_NBPV, quantile(X2017Pre_NBPV, probs=0:10/10), include.lowest=TRUE))))


##########################regression analysis#########################################
####-----                      regression                             ------------ ###
####-----                                                            ------------ ###
####-----                                                            ------------ ###
####-----                                                            ------------ ###
######################################################################################
##NA_FRBO
##NA_FRBO<- mydata %>% filter(OwningTeam=="Americas",
  ##                          PartnerType=="FRBO",
    ##                        HASales>quantile(HASales,probs=0.7),
      ##                      HASales<quantile(HASales,probs=0.9))
NA_FRBO<- mydata %>% filter(OwningTeam=="Americas",
                            PartnerType=="FRBO")
##runningPercentage>0.7,
##runningPercentage<0.9)
sum(NA_FRBO$HASales)

PrePostData_for_use %>% filter(OwningTeam=="Europe",
                               PartnerType=="PM",
                               Type=="ExceptionList",
                   X2017Pre_NBPV>=0.21,
                   X2017Pre_NBPV!=Inf) %>%
  summarise(sum_NA_FRBO_exception=sum(HASales),
            counts=n())

PrePostData_for_use %>% filter(OwningTeam=="Europe",
                               PartnerType=="PM") %>%
  summarise(sum_NA_FRBO_exception=sum(HASales),
            counts=n())


summary(NA_FRBO_reg2_weightPV <- lm(conversion_increase17~
                                      exceptionflag
                                    +I(X2017Pre_NBPV*exceptionflag)
                                    ##+I(X2017Pre_NBPV^2*exceptionflag)
                                    +poly(X2017Pre_NBPV,2)
                                    +poly(X2017Pre_60DayKnownNBV,2)
                                    +poly(HASales,2)
                                    +poly(X2017Pre_60DayBookingRequests,2)
                                    +poly(X2017Pre_60DayInquiries,2)
                                    , weights =X2017Post_PV
                                    , data = NA_FRBO))
###NA PM
NA_PM<- mydata %>% filter(OwningTeam=="Americas",
                          PartnerType=="PM")
#,
##                         HASales>quantile(HASales,probs=0.7),
##                       HASales<quantile(HASales,probs=0.9))


summary(NA_PM_reg2_weightPV <- lm(conversion_increase17~
                                    exceptionflag
                                  +I(X2017Pre_NBPV*exceptionflag)
                                  ##+I(X2017Pre_NBPV^2*exceptionflag)
                                  +poly(X2017Pre_NBPV,2)
                                  +poly(X2017Pre_60DayKnownNBV,2)
                                  +poly(HASales,2)
                                  +poly(X2017Pre_60DayBookingRequests,2)
                                  +poly(X2017Pre_60DayInquiries,2)
                                  , weights =X2017Post_PV
                                  , data = NA_PM))

##EU FRBO
EU_FRBO<- mydata %>% filter(OwningTeam=="Europe",
                            PartnerType=="FRBO")
##,
##                        HASales>quantile(HASales,probs=0.7),
##                      HASales<quantile(HASales,probs=0.9))


summary(EU_FRBO_reg2_weightPV <- lm(conversion_increase17~
                                      exceptionflag
                                    +I(X2017Pre_NBPV*exceptionflag)
                                    +I(X2017Pre_NBPV^2*exceptionflag)
                                    ##+I(X2017Pre_NBPV^3*exceptionflag)
                                    +poly(X2017Pre_NBPV,2)
                                    +poly(X2017Pre_60DayKnownNBV,2)
                                    +poly(HASales,2)
                                    +poly(X2017Pre_60DayBookingRequests,2)
                                    +poly(X2017Pre_60DayInquiries,2)
                                    , weights =X2017Post_PV
                                    , data = EU_FRBO))

##x<- seq(0,1,length=100)
##y <- 1414*x*x-876*x+97
##plot(x,y)

##EU PM
EU_PM<- mydata %>% filter(OwningTeam=="Europe",
                          PartnerType=="PM")
##,
##                        HASales>quantile(HASales,probs=0.7),
##                      HASales<quantile(HASales,probs=0.9))


summary(EU_PM_reg2_weightPV <- lm(conversion_increase17~
                                    exceptionflag
                                  +I(X2017Pre_NBPV*exceptionflag)
                                  +I(X2017Pre_NBPV^2*exceptionflag)
                                  +poly(X2017Pre_NBPV,2)
                                  +poly(X2017Pre_60DayKnownNBV,2)
                                  +poly(HASales,2)
                                  +poly(X2017Pre_60DayBookingRequests,2)
                                  +poly(X2017Pre_60DayInquiries,2)
                                  , weights =X2017Post_PV
                                  , data = EU_PM))




###########################threshold analysis##########################################
#######--------------------marginal effects and confidence intervals---------##########
######----                                                           -------- #########
######----                                                           -------- #########
######----                                                           -------- #########
#######################################################################################
###NA FRBO
cov_NA_FRBO <- vcov(NA_FRBO_reg2_weightPV)
cov_NA_FRBO

marginal_NA_FRBO <- NA_FRBO_reg2_weightPV$coefficients[2]+ NA_FRBO$X2017Pre_NBPV*NA_FRBO_reg2_weightPV$coefficients[3]
se_NA_FRBO <- sqrt(cov_NA_FRBO[2,2]+cov_NA_FRBO[3,3]*NA_FRBO$X2017Pre_NBPV^2+2*NA_FRBO$X2017Pre_NBPV*cov_NA_FRBO[2,3])

CI_upper_NA_FRBO <- marginal_NA_FRBO+1.96*se_NA_FRBO
CI_lower_NA_FRBO <- marginal_NA_FRBO-1.96*se_NA_FRBO

NA_FRBO_CI_threshold <- data.frame(NA_FRBO$X2017Pre_NBPV,marginal_NA_FRBO,se_NA_FRBO,CI_upper_NA_FRBO,CI_lower_NA_FRBO)

NA_FRBO_CI_threshold <- NA_FRBO_CI_threshold %>% mutate(Scenario=ifelse(-marginal_NA_FRBO>0,"Closed Loop Wins","Exception Wins"))



xxx<- NA_FRBO_CI_threshold  %>% ggplot(aes(x=NA_FRBO.X2017Pre_NBPV,y=-marginal_NA_FRBO,color=Scenario))+
                                scale_fill_manual(values=cbPalette)+
                                scale_colour_manual(values=cbPalette)+
                                geom_ribbon(aes(x=NA_FRBO.X2017Pre_NBPV, ymax=-CI_lower_NA_FRBO, ymin=-CI_upper_NA_FRBO),color="grey",alpha=.15)+
                                geom_line(size=3)

cutoff <- -NA_FRBO_reg2_weightPV$coefficients[2]/NA_FRBO_reg2_weightPV$coefficients[3]

##fun <- function (x) NA_FRBO_reg2_weightPV$coefficients[2]+x*NA_FRBO_reg2_weightPV$coefficients[3]+x^2*NA_FRBO_reg2_weightPV$coefficients[4]
##uni <- uniroot(fun, c(0, 1))$root



c <- sort(c(seq(min(NA_FRBO_CI_threshold$NA_FRBO.X2017Pre_NBPV), max(NA_FRBO_CI_threshold$NA_FRBO.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  ##geom_line(aes(y=-CI_upper_NA_FRBO),colour="grey",linetype="dashed")+
  ##geom_line(aes(y=-CI_lower_NA_FRBO),colour="grey",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_cartesian(xlim=c(0.07,1.3),
                  ylim = c(-30,40))+
  scale_x_continuous(breaks = c)+
  xlab("NA FRBO Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Closed Loop (%)")+
  ggtitle("Marginal Effects and 95 % Confidence Intervals")+ 
  theme_light()




#########NA PM#######################
cov_NA_PM <- vcov(NA_PM_reg2_weightPV)


marginal_NA_PM <- NA_PM_reg2_weightPV$coefficients[2]+ NA_PM$X2017Pre_NBPV*NA_PM_reg2_weightPV$coefficients[3]
se_NA_PM <- sqrt(cov_NA_PM[2,2]+cov_NA_PM[3,3]*NA_PM$X2017Pre_NBPV^2+2*NA_PM$X2017Pre_NBPV*cov_NA_PM[2,3])


CI_upper_NA_PM <- marginal_NA_PM+1.96*se_NA_PM
CI_lower_NA_PM <- marginal_NA_PM-1.96*se_NA_PM

NA_PM_CI_threshold <- data.frame(NA_PM$X2017Pre_NBPV,marginal_NA_PM,se_NA_PM,CI_upper_NA_PM,CI_lower_NA_PM)

NA_PM_CI_threshold <- NA_PM_CI_threshold %>% mutate(Scenario=ifelse(-marginal_NA_PM>0,"Closed Loop Wins","Exception Wins"))

xxx<- NA_PM_CI_threshold  %>% ggplot(aes(x=NA_PM.X2017Pre_NBPV,y=-marginal_NA_PM,color=Scenario))+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  geom_ribbon(aes(x=NA_PM.X2017Pre_NBPV, ymax=-CI_lower_NA_PM, ymin=-CI_upper_NA_PM), color="grey", alpha=.15)+
  geom_line(size=3)

cutoff <- -NA_PM_reg2_weightPV$coefficients[2]/NA_PM_reg2_weightPV$coefficients[3]
c <- sort(c(seq(min(NA_PM_CI_threshold$NA_PM.X2017Pre_NBPV), max(NA_PM_CI_threshold$NA_PM.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  ##geom_line(aes(y=-CI_upper_NA_FRBO),colour="grey",linetype="dashed")+
  ##geom_line(aes(y=-CI_lower_NA_FRBO),colour="grey",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_cartesian(xlim=c(0.07,1.3),
                  ylim = c(-40,40))+
  scale_x_continuous(breaks = c)+
  xlab("NA PM Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Closed Loop (%)")+
  ggtitle("Marginal Effects and 95 % Confidence Intervals")+ 
  theme_light()



############EU FRBO#####################
cov_EU_FRBO <- vcov(EU_FRBO_reg2_weightPV)


marginal_EU_FRBO <- EU_FRBO_reg2_weightPV$coefficients[2]+ EU_FRBO$X2017Pre_NBPV*EU_FRBO_reg2_weightPV$coefficients[3]
se_EU_FRBO <- sqrt(cov_EU_FRBO[2,2]+cov_EU_FRBO[3,3]*EU_FRBO$X2017Pre_NBPV^2+2*EU_FRBO$X2017Pre_NBPV*cov_EU_FRBO[2,3])
CI_upper_EU_FRBO <- marginal_EU_FRBO+1.96*se_EU_FRBO
CI_lower_EU_FRBO <- marginal_EU_FRBO-1.96*se_EU_FRBO

EU_FRBO_CI_threshold <- data.frame(EU_FRBO$X2017Pre_NBPV,marginal_EU_FRBO,se_EU_FRBO,CI_upper_EU_FRBO,CI_lower_EU_FRBO)

xxx<- EU_FRBO_CI_threshold  %>% ggplot(aes(x=EU_FRBO.X2017Pre_NBPV,y=marginal_EU_FRBO))+geom_line()

cutoff <- -EU_FRBO_reg2_weightPV$coefficients[2]/EU_FRBO_reg2_weightPV$coefficients[3]
c <- sort(c(seq(min(EU_FRBO_CI_threshold$EU_FRBO.X2017Pre_NBPV), max(EU_FRBO_CI_threshold$EU_FRBO.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  geom_line(aes(y=CI_upper_EU_FRBO),colour="blue",linetype="dashed")+
  geom_line(aes(y=CI_lower_EU_FRBO),colour="blue",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff),colour="red", linetype="dashed")+
  coord_cartesian(xlim=c(0,4),
                  ylim = c(-40,70))+
  scale_x_continuous(breaks = c)+
  xlab("EU FRBO Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Exception on Conversion Lift(%)")


###############EU PM#######################
cov_EU_PM <- vcov(EU_PM_reg2_weightPV)


marginal_EU_PM <- EU_PM_reg2_weightPV$coefficients[2]+ EU_PM$X2017Pre_NBPV*EU_PM_reg2_weightPV$coefficients[3]
se_EU_PM <- sqrt(cov_EU_PM[2,2]+cov_EU_PM[3,3]*EU_PM$X2017Pre_NBPV^2+2*EU_PM$X2017Pre_NBPV*cov_EU_PM[2,3])
CI_upper_EU_PM <- marginal_EU_PM+1.96*se_EU_PM
CI_lower_EU_PM <- marginal_EU_PM-1.96*se_EU_PM

EU_PM_CI_threshold <- data.frame(EU_PM$X2017Pre_NBPV,marginal_EU_PM,se_EU_PM,CI_upper_EU_PM,CI_lower_EU_PM)

xxx<- EU_PM_CI_threshold  %>% ggplot(aes(x=EU_PM.X2017Pre_NBPV,y=marginal_EU_PM))+geom_line()

cutoff <- -EU_PM_reg2_weightPV$coefficients[2]/EU_PM_reg2_weightPV$coefficients[3]
c <- sort(c(seq(min(EU_PM_CI_threshold$EU_PM.X2017Pre_NBPV), max(EU_PM_CI_threshold$EU_PM.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  geom_line(aes(y=CI_upper_EU_PM),colour="blue",linetype="dashed")+
  geom_line(aes(y=CI_lower_EU_PM),colour="blue",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff),colour="red", linetype="dashed")+
  coord_cartesian(xlim=c(0,1.3),
                  ylim = c(-40,70))+
  scale_x_continuous(breaks = c)+
  xlab("EU PM Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Exception on Conversion Lift(%)")




###########################threshold analysis##########################################
#######--------------------marginal effects and confidence intervals---------##########
######----      second order interaction                             -------- #########
######----                                                           -------- #########
######----                                                           -------- #########
#######################################################################################
###NA FRBO
cov_NA_FRBO <- vcov(NA_FRBO_reg2_weightPV)
cov_NA_FRBO

marginal_NA_FRBO <- NA_FRBO_reg2_weightPV$coefficients[2]+NA_FRBO$X2017Pre_NBPV*NA_FRBO_reg2_weightPV$coefficients[3]+NA_FRBO$X2017Pre_NBPV^2*NA_FRBO_reg2_weightPV$coefficients[4]
se_NA_FRBO <- sqrt(cov_NA_FRBO[2,2]+
                     cov_NA_FRBO[3,3]*NA_FRBO$X2017Pre_NBPV^2+
                     cov_NA_FRBO[4,4]*NA_FRBO$X2017Pre_NBPV^4+
                     2*NA_FRBO$X2017Pre_NBPV*cov_NA_FRBO[2,3]+
                     2*NA_FRBO$X2017Pre_NBPV^2*cov_NA_FRBO[2,4]+
                     2*NA_FRBO$X2017Pre_NBPV^3*cov_NA_FRBO[3,4])
CI_upper_NA_FRBO <- marginal_NA_FRBO+1.96*se_NA_FRBO
CI_lower_NA_FRBO <- marginal_NA_FRBO-1.96*se_NA_FRBO

NA_FRBO_CI_threshold <- data.frame(NA_FRBO$X2017Pre_NBPV,marginal_NA_FRBO,se_NA_FRBO,CI_upper_NA_FRBO,CI_lower_NA_FRBO)

xxx<- NA_FRBO_CI_threshold  %>% ggplot(aes(x=NA_FRBO.X2017Pre_NBPV,y=marginal_NA_FRBO))+geom_line()

##cutoff <- -NA_FRBO_reg2_weightPV$coefficients[2]/NA_FRBO_reg2_weightPV$coefficients[3]

fun <- function (x) NA_FRBO_reg2_weightPV$coefficients[2]+x*NA_FRBO_reg2_weightPV$coefficients[3]+x^2*NA_FRBO_reg2_weightPV$coefficients[4]
cutoff <- uniroot(fun, c(0, 1))$root



c <- sort(c(seq(min(NA_FRBO_CI_threshold$NA_FRBO.X2017Pre_NBPV), max(NA_FRBO_CI_threshold$NA_FRBO.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  geom_line(aes(y=CI_upper_NA_FRBO),colour="blue",linetype="dashed")+
  geom_line(aes(y=CI_lower_NA_FRBO),colour="blue",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff),colour="red", linetype="dashed")+
  coord_cartesian(xlim=c(0,1.3),
                  ylim = c(-20,40))+
  scale_x_continuous(breaks = c)+
  xlab("NA FRBO Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Exception (%)")



#########NA PM#######################
cov_NA_PM <- vcov(NA_PM_reg2_weightPV)


marginal_NA_PM <- NA_PM_reg2_weightPV$coefficients[2]+NA_PM$X2017Pre_NBPV*NA_PM_reg2_weightPV$coefficients[3]+NA_PM$X2017Pre_NBPV^2*NA_PM_reg2_weightPV$coefficients[4]
se_NA_PM <- sqrt(cov_NA_PM[2,2]+
                   cov_NA_PM[3,3]*NA_PM$X2017Pre_NBPV^2+
                   cov_NA_PM[4,4]*NA_PM$X2017Pre_NBPV^4+
                   2*NA_PM$X2017Pre_NBPV*cov_NA_PM[2,3]+
                   2*NA_PM$X2017Pre_NBPV^2*cov_NA_PM[2,4]+
                   2*NA_PM$X2017Pre_NBPV^3*cov_NA_PM[3,4])


CI_upper_NA_PM <- marginal_NA_PM+1.96*se_NA_PM
CI_lower_NA_PM <- marginal_NA_PM-1.96*se_NA_PM

NA_PM_CI_threshold <- data.frame(NA_PM$X2017Pre_NBPV,marginal_NA_PM,se_NA_PM,CI_upper_NA_PM,CI_lower_NA_PM)

xxx<- NA_PM_CI_threshold  %>% ggplot(aes(x=NA_PM.X2017Pre_NBPV,y=marginal_NA_PM))+geom_line()

##cutoff <- -NA_PM_reg2_weightPV$coefficients[2]/NA_PM_reg2_weightPV$coefficients[3]
fun <- function (x) NA_PM_reg2_weightPV$coefficients[2]+x*NA_PM_reg2_weightPV$coefficients[3]+x^2*NA_PM_reg2_weightPV$coefficients[4]
cutoff <- uniroot(fun, c(0, 1))$root

c <- sort(c(seq(min(NA_PM_CI_threshold$NA_PM.X2017Pre_NBPV), max(NA_PM_CI_threshold$NA_PM.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  geom_line(aes(y=CI_upper_NA_PM),colour="blue",linetype="dashed")+
  geom_line(aes(y=CI_lower_NA_PM),colour="blue",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff),colour="red", linetype="dashed")+
  coord_cartesian(xlim=c(0,1.3),
                  ylim = c(-40,70))+
  scale_x_continuous(breaks = c)+
  xlab("NA PM Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Exception on Conversion Lift(%)")

############EU FRBO#####################
cov_EU_FRBO <- vcov(EU_FRBO_reg2_weightPV)


marginal_EU_FRBO <- EU_FRBO_reg2_weightPV$coefficients[2]+EU_FRBO$X2017Pre_NBPV*EU_FRBO_reg2_weightPV$coefficients[3]+EU_FRBO$X2017Pre_NBPV^2*EU_FRBO_reg2_weightPV$coefficients[4]
se_EU_FRBO<- sqrt(cov_EU_FRBO[2,2]+
                    cov_EU_FRBO[3,3]*EU_FRBO$X2017Pre_NBPV^2+
                    cov_EU_FRBO[4,4]*EU_FRBO$X2017Pre_NBPV^4+
                    2*EU_FRBO$X2017Pre_NBPV*cov_EU_FRBO[2,3]+
                    2*EU_FRBO$X2017Pre_NBPV^2*cov_EU_FRBO[2,4]+
                    2*EU_FRBO$X2017Pre_NBPV^3*cov_EU_FRBO[3,4])


CI_upper_EU_FRBO <- marginal_EU_FRBO+1.96*se_EU_FRBO
CI_lower_EU_FRBO <- marginal_EU_FRBO-1.96*se_EU_FRBO

EU_FRBO_CI_threshold <- data.frame(EU_FRBO$X2017Pre_NBPV,marginal_EU_FRBO,se_EU_FRBO,CI_upper_EU_FRBO,CI_lower_EU_FRBO)



EU_FRBO_CI_threshold <- EU_FRBO_CI_threshold %>% mutate(Scenario=ifelse(-marginal_EU_FRBO>0,"Closed Loop Wins","Exception Wins"))

xxx<- EU_FRBO_CI_threshold  %>% filter(EU_FRBO.X2017Pre_NBPV<1.5)%>%
  ggplot(aes(x=EU_FRBO.X2017Pre_NBPV,y=-marginal_EU_FRBO,color=Scenario))+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  geom_ribbon(aes(x=EU_FRBO.X2017Pre_NBPV, ymax=-CI_lower_EU_FRBO, ymin=-CI_upper_EU_FRBO),  color="grey",alpha=.15)+
  geom_line(size=3)

##cutoff <- -EU_FRBO_reg2_weightPV$coefficients[2]/EU_FRBO_reg2_weightPV$coefficients[3]

fun <- function (x) EU_FRBO_reg2_weightPV$coefficients[2]+x*EU_FRBO_reg2_weightPV$coefficients[3]+x^2*EU_FRBO_reg2_weightPV$coefficients[4]
cutoff <- uniroot(fun, c(0, 1))$root

c <- sort(c(seq(min(EU_FRBO_CI_threshold$EU_FRBO.X2017Pre_NBPV), max(EU_FRBO_CI_threshold$EU_FRBO.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  ##geom_line(aes(y=-CI_upper_NA_FRBO),colour="grey",linetype="dashed")+
  ##geom_line(aes(y=-CI_lower_NA_FRBO),colour="grey",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_cartesian(xlim=c(0.07,1.3),
                  ylim = c(-30,40))+
  scale_x_continuous(breaks = c)+
  xlab("EU FRBO Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Closed Loop (%)")+
  ggtitle("Marginal Effects and 95 % Confidence Intervals")+ 
  theme_light()




###############EU PM#######################
cov_EU_PM <- vcov(EU_PM_reg2_weightPV)

marginal_EU_PM <- EU_PM_reg2_weightPV$coefficients[2]+EU_PM$X2017Pre_NBPV*EU_PM_reg2_weightPV$coefficients[3]+EU_PM$X2017Pre_NBPV^2*EU_PM_reg2_weightPV$coefficients[4]
se_EU_PM<- sqrt(cov_EU_PM[2,2]+
                  cov_EU_PM[3,3]*EU_PM$X2017Pre_NBPV^2+
                  cov_EU_PM[4,4]*EU_PM$X2017Pre_NBPV^4+
                  2*EU_PM$X2017Pre_NBPV*cov_EU_PM[2,3]+
                  2*EU_PM$X2017Pre_NBPV^2*cov_EU_PM[2,4]+
                  2*EU_PM$X2017Pre_NBPV^3*cov_EU_PM[3,4])

CI_upper_EU_PM <- marginal_EU_PM+1.96*se_EU_PM
CI_lower_EU_PM <- marginal_EU_PM-1.96*se_EU_PM

EU_PM_CI_threshold <- data.frame(EU_PM$X2017Pre_NBPV,marginal_EU_PM,se_EU_PM,CI_upper_EU_PM,CI_lower_EU_PM)

EU_PM_CI_threshold <- EU_PM_CI_threshold %>% mutate(Scenario=ifelse(-marginal_EU_PM>0,"Closed Loop Wins","Exception Wins"))

xxx<- EU_PM_CI_threshold  %>% filter(EU_PM.X2017Pre_NBPV<1.7)%>%
  ggplot(aes(x=EU_PM.X2017Pre_NBPV,y=-marginal_EU_PM,color=Scenario))+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+geom_ribbon(aes(x=EU_PM.X2017Pre_NBPV, ymax=-CI_lower_EU_PM, ymin=-CI_upper_EU_PM),  color="grey",alpha=.15)+
  geom_line(size=3)

##cutoff <- -EU_FRBO_reg2_weightPV$coefficients[2]/EU_FRBO_reg2_weightPV$coefficients[3]

fun <- function (x) EU_PM_reg2_weightPV$coefficients[2]+x*EU_PM_reg2_weightPV$coefficients[3]+x^2*EU_PM_reg2_weightPV$coefficients[4]
cutoff <- uniroot(fun, c(0, 1))$root

c <- sort(c(seq(min(EU_PM_CI_threshold$EU_PM.X2017Pre_NBPV), max(EU_PM_CI_threshold$EU_PM.X2017Pre_NBPV), length.out=5), cutoff))
c <- unname(c)

xxx+
  ##geom_line(aes(y=-CI_upper_NA_FRBO),colour="grey",linetype="dashed")+
  ##geom_line(aes(y=-CI_lower_NA_FRBO),colour="grey",linetype="dashed")+
  geom_vline(aes(xintercept=cutoff))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  coord_cartesian(xlim=c(0.07,1.3),
                  ylim = c(-30,40))+
  scale_x_continuous(breaks = c)+
  xlab("EU PM Pre17 NBPV Conversion (%)")+
  ylab("Marginal Effects of Closed Loop (%)")+
  ggtitle("Marginal Effects and 95 % Confidence Intervals")+ 
  theme_light()





##################deciles for angee##############################
NA_decile <- quantile(NA_FRBO$X2017Pre_NBPV, probs=0:10/10)
write.csv(NA_decile,"NA_decile.csv")

NA_PM_decile <- quantile(NA_PM$X2017Pre_NBPV, probs=0:10/10)
write.csv(NA_PM_decile,"NA_PM_decile.csv")

EU_FRBO_decile <- quantile(EU_FRBO$X2017Pre_NBPV, probs=0:10/10)
write.csv(EU_FRBO_decile,"EU_FRBO_decile.csv")

EU_PM_decile <- quantile(EU_PM$X2017Pre_NBPV, probs=0:10/10)
write.csv(EU_PM_decile,"EU_PM_decile.csv")

PrePostData_for_use %>% filter(X2017Pre_60DayPV>0,
                               X2017Pre_30DayPV>0,
                               X2017Post_PV>0,
                               X2017Pre_NBPV>0.07) %>% 
                      group_by(OwningTeam,
                               PartnerType,
                               Type) %>%
  summarise(min_NBPV=min(X2017Pre_NBPV),
            max_NBPV=max(X2017Pre_NBPV))

################################regression fits for calculating $$########################

###########################visualization of fitted models#####################################
mydata <- mydata %>% group_by(OwningTeam,
                              PartnerType) %>%
  mutate(regression_fitted_value_linear_interaction= lm(conversion_increase17~
                                                          exceptionflag
                                                        +I(X2017Pre_NBPV*exceptionflag)
                                                        ##+I(X2017Pre_NBPV^2*exceptionflag)
                                                        +poly(X2017Pre_NBPV,2)
                                                        +poly(X2017Pre_60DayKnownNBV,2)
                                                        +poly(HASales,2)
                                                        +poly(X2017Pre_60DayBookingRequests,2)
                                                        +poly(X2017Pre_60DayInquiries,2)
                                                        , weights =X2017Post_PV)$fitted.values,
         regression_fitted_value_quadratic_interaction= lm(conversion_increase17~
                                                             exceptionflag
                                                           +I(X2017Pre_NBPV*exceptionflag)
                                                           +I(X2017Pre_NBPV^2*exceptionflag)
                                                           +poly(X2017Pre_NBPV,2)
                                                           +poly(X2017Pre_60DayKnownNBV,2)
                                                           +poly(HASales,2)
                                                           +poly(X2017Pre_60DayBookingRequests,2)
                                                           +poly(X2017Pre_60DayInquiries,2)
                                                           , weights =X2017Post_PV)$fitted.values)


regression <- mydata %>% filter(OwningTeam=="Europe",
                                X2017Pre_NBPV<1.3)%>%
  ggplot(aes(x=X2017Pre_NBPV, y=regression_fitted_value_linear_interaction,color=Type)) + 
  ##geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"),level=0.5)+
  geom_smooth(method = "lm", formula = y ~ x,level=0.9)+
  ##geom_smooth()+
  ##stat_smooth(span = 0.5, level = 0.5)+
  xlab("Pre NB_PV Conversion (in %)")+
  ylab("Fitted Value of Conversion % Increase")+
  facet_wrap(~PartnerType)
regression
regression+coord_cartesian(xlim=c(0,1.3),
                           ylim = c(-100,200))


######################decile for closed loop action final final final##########################'
######################dollar wins##############################################################



######################Financial impact counterfactual analysis##########################
############                    user level                    ##########################
############                                                             ###############
############                                                             ###############
############                                                             ###############
############                                                             ###############
########################################################################################

##########NA FRBO#############################
NA_FRBO <- NA_FRBO %>% mutate(marginal_effect=-NA_FRBO_CI_threshold$marginal_NA_FRBO,
                              marginal_effect_upper=-NA_FRBO_CI_threshold$CI_lower_NA_FRBO,
                              marginal_effect_lower=-NA_FRBO_CI_threshold$CI_upper_NA_FRBO)
summary(NA_FRBO_exception_counterfactual)

NA_FRBO_exception_counterfactual <- NA_FRBO%>% filter(Type=="ExceptionList") %>%
                                               mutate(counterfactual_conversion_lift=conversion_increase17+marginal_effect,
                                                      Post_NBV_per_booking=ifelse(X2017Post_KnownNetBookings>0,X2017Post_KnownNBV/X2017Post_KnownNetBookings,X2017Pre_60DayKnownNBV/X2017Pre_60DayKnownNetBookings),
                                                      Predicted_post_NBPV=X2017Pre_NBPV*(1+counterfactual_conversion_lift/100),
                                                      Predicted_post_NetBookings=Predicted_post_NBPV/100*X2017Post_PV,
                                                      Predicted_post_NBV=Predicted_post_NetBookings*Post_NBV_per_booking,
                                                      Delta_post_NBV=Predicted_post_NBV-X2017Post_KnownNBV,
                                                      upper_counterfactual_conversion_lift=conversion_increase17+marginal_effect_upper,
                                                      upper_Predicted_post_NBPV=X2017Pre_NBPV*(1+upper_counterfactual_conversion_lift/100),
                                                      upper_Predicted_post_NetBookings=upper_Predicted_post_NBPV/100*X2017Post_PV,
                                                      upper_Predicted_post_NBV=upper_Predicted_post_NetBookings*Post_NBV_per_booking,
                                                      upper_Delta_post_NBV=upper_Predicted_post_NBV-X2017Post_KnownNBV,
                                                      lower_counterfactual_conversion_lift=conversion_increase17+marginal_effect_lower,
                                                      lower_Predicted_post_NBPV=X2017Pre_NBPV*(1+lower_counterfactual_conversion_lift/100),
                                                      lower_Predicted_post_NetBookings=lower_Predicted_post_NBPV/100*X2017Post_PV,
                                                      lower_Predicted_post_NBV=lower_Predicted_post_NetBookings*Post_NBV_per_booking,
                                                      lower_Delta_post_NBV=lower_Predicted_post_NBV-X2017Post_KnownNBV)


NA_FRBO_financial_impact <- NA_FRBO_exception_counterfactual %>% group_by(NBPV_quantile100) %>%
                                     summarise(counts=n(),
                                               Aggreate_delta_NBV=sum(Delta_post_NBV),
                                               Upper_aggregate_delta_NBV=sum(upper_Delta_post_NBV),
                                               Lower_aggregate_delta_NBV=sum(lower_Delta_post_NBV),
                                               Mean_marginal_effect=mean(marginal_effect),
                                               Mean_counterfactual_conversion_lift=mean(counterfactual_conversion_lift),
                                               Mean_converion_lift=mean(conversion_increase17))

write_csv(NA_FRBO_financial_impact, "NA_FRBO_financial_impact.csv")


################NA PM########################################
NA_PM <- NA_PM %>% mutate(marginal_effect=-NA_PM_CI_threshold$marginal_NA_PM,
                              marginal_effect_upper=-NA_PM_CI_threshold$CI_lower_NA_PM,
                              marginal_effect_lower=-NA_PM_CI_threshold$CI_upper_NA_PM)


NA_PM_exception_counterfactual <- NA_PM %>% filter(Type=="ExceptionList") %>%
  mutate(counterfactual_conversion_lift=conversion_increase17+marginal_effect,
         Post_NBV_per_booking=ifelse(X2017Post_KnownNetBookings>0,X2017Post_KnownNBV/X2017Post_KnownNetBookings,X2017Pre_60DayKnownNBV/X2017Pre_60DayKnownNetBookings),
         Predicted_post_NBPV=X2017Pre_NBPV*(1+counterfactual_conversion_lift/100),
         Predicted_post_NetBookings=Predicted_post_NBPV/100*X2017Post_PV,
         Predicted_post_NBV=Predicted_post_NetBookings*Post_NBV_per_booking,
         Delta_post_NBV=Predicted_post_NBV-X2017Post_KnownNBV,
         upper_counterfactual_conversion_lift=conversion_increase17+marginal_effect_upper,
         upper_Predicted_post_NBPV=X2017Pre_NBPV*(1+upper_counterfactual_conversion_lift/100),
         upper_Predicted_post_NetBookings=upper_Predicted_post_NBPV/100*X2017Post_PV,
         upper_Predicted_post_NBV=upper_Predicted_post_NetBookings*Post_NBV_per_booking,
         upper_Delta_post_NBV=upper_Predicted_post_NBV-X2017Post_KnownNBV,
         lower_counterfactual_conversion_lift=conversion_increase17+marginal_effect_lower,
         lower_Predicted_post_NBPV=X2017Pre_NBPV*(1+lower_counterfactual_conversion_lift/100),
         lower_Predicted_post_NetBookings=lower_Predicted_post_NBPV/100*X2017Post_PV,
         lower_Predicted_post_NBV=lower_Predicted_post_NetBookings*Post_NBV_per_booking,
         lower_Delta_post_NBV=lower_Predicted_post_NBV-X2017Post_KnownNBV)


NA_PM_financial_impact <- NA_PM_exception_counterfactual %>% group_by(NBPV_quantile100) %>%
  summarise(counts=n(),
            Aggreate_delta_NBV=sum(Delta_post_NBV),
            Upper_aggregate_delta_NBV=sum(upper_Delta_post_NBV),
            Lower_aggregate_delta_NBV=sum(lower_Delta_post_NBV),
            Mean_marginal_effect=mean(marginal_effect),
            Mean_counterfactual_conversion_lift=mean(counterfactual_conversion_lift),
            Mean_converion_lift=mean(conversion_increase17))

write_csv(NA_PM_financial_impact, "NA_PM_financial_impact.csv")


################EU FRBO Financial impact
EU_FRBO <- EU_FRBO %>% mutate(marginal_effect=-EU_FRBO_CI_threshold$marginal_EU_FRBO,
                          marginal_effect_upper=-EU_FRBO_CI_threshold$CI_lower_EU_FRBO,
                          marginal_effect_lower=-EU_FRBO_CI_threshold$CI_upper_EU_FRBO)


EU_FRBO_exception_counterfactual <- EU_FRBO %>% filter(Type=="ExceptionList") %>%
  mutate(counterfactual_conversion_lift=conversion_increase17+marginal_effect,
         Post_NBV_per_booking=ifelse(X2017Post_KnownNetBookings>0,X2017Post_KnownNBV/X2017Post_KnownNetBookings,X2017Pre_60DayKnownNBV/X2017Pre_60DayKnownNetBookings),
         Predicted_post_NBPV=X2017Pre_NBPV*(1+counterfactual_conversion_lift/100),
         Predicted_post_NetBookings=Predicted_post_NBPV/100*X2017Post_PV,
         Predicted_post_NBV=Predicted_post_NetBookings*Post_NBV_per_booking,
         Delta_post_NBV=Predicted_post_NBV-X2017Post_KnownNBV,
         upper_counterfactual_conversion_lift=conversion_increase17+marginal_effect_upper,
         upper_Predicted_post_NBPV=X2017Pre_NBPV*(1+upper_counterfactual_conversion_lift/100),
         upper_Predicted_post_NetBookings=upper_Predicted_post_NBPV/100*X2017Post_PV,
         upper_Predicted_post_NBV=upper_Predicted_post_NetBookings*Post_NBV_per_booking,
         upper_Delta_post_NBV=upper_Predicted_post_NBV-X2017Post_KnownNBV,
         lower_counterfactual_conversion_lift=conversion_increase17+marginal_effect_lower,
         lower_Predicted_post_NBPV=X2017Pre_NBPV*(1+lower_counterfactual_conversion_lift/100),
         lower_Predicted_post_NetBookings=lower_Predicted_post_NBPV/100*X2017Post_PV,
         lower_Predicted_post_NBV=lower_Predicted_post_NetBookings*Post_NBV_per_booking,
         lower_Delta_post_NBV=lower_Predicted_post_NBV-X2017Post_KnownNBV)


EU_FRBO_financial_impact <- EU_FRBO_exception_counterfactual %>% group_by(NBPV_quantile100) %>%
  summarise(counts=n(),
            Aggreate_delta_NBV=sum(Delta_post_NBV),
            Upper_aggregate_delta_NBV=sum(upper_Delta_post_NBV),
            Lower_aggregate_delta_NBV=sum(lower_Delta_post_NBV),
            Mean_marginal_effect=mean(marginal_effect),
            Mean_counterfactual_conversion_lift=mean(counterfactual_conversion_lift),
            Mean_converion_lift=mean(conversion_increase17))

write_csv(EU_FRBO_financial_impact, "EU_FRBO_financial_impact.csv")

###############EU PM###################
EU_PM <- EU_PM %>% mutate(marginal_effect=-EU_PM_CI_threshold$marginal_EU_PM,
                              marginal_effect_upper=-EU_PM_CI_threshold$CI_lower_EU_PM,
                              marginal_effect_lower=-EU_PM_CI_threshold$CI_upper_EU_PM)


EU_PM_exception_counterfactual <- EU_PM %>% filter(Type=="ExceptionList") %>%
  mutate(counterfactual_conversion_lift=conversion_increase17+marginal_effect,
         Post_NBV_per_booking=ifelse(X2017Post_KnownNetBookings>0,X2017Post_KnownNBV/X2017Post_KnownNetBookings,X2017Pre_60DayKnownNBV/X2017Pre_60DayKnownNetBookings),
         Predicted_post_NBPV=X2017Pre_NBPV*(1+counterfactual_conversion_lift/100),
         Predicted_post_NetBookings=Predicted_post_NBPV/100*X2017Post_PV,
         Predicted_post_NBV=Predicted_post_NetBookings*Post_NBV_per_booking,
         Delta_post_NBV=Predicted_post_NBV-X2017Post_KnownNBV,
         upper_counterfactual_conversion_lift=conversion_increase17+marginal_effect_upper,
         upper_Predicted_post_NBPV=X2017Pre_NBPV*(1+upper_counterfactual_conversion_lift/100),
         upper_Predicted_post_NetBookings=upper_Predicted_post_NBPV/100*X2017Post_PV,
         upper_Predicted_post_NBV=upper_Predicted_post_NetBookings*Post_NBV_per_booking,
         upper_Delta_post_NBV=upper_Predicted_post_NBV-X2017Post_KnownNBV,
         lower_counterfactual_conversion_lift=conversion_increase17+marginal_effect_lower,
         lower_Predicted_post_NBPV=X2017Pre_NBPV*(1+lower_counterfactual_conversion_lift/100),
         lower_Predicted_post_NetBookings=lower_Predicted_post_NBPV/100*X2017Post_PV,
         lower_Predicted_post_NBV=lower_Predicted_post_NetBookings*Post_NBV_per_booking,
         lower_Delta_post_NBV=lower_Predicted_post_NBV-X2017Post_KnownNBV)


EU_PM_financial_impact <- EU_PM_exception_counterfactual %>% group_by(NBPV_quantile100) %>%
  summarise(counts=n(),
            Aggreate_delta_NBV=sum(Delta_post_NBV),
            Upper_aggregate_delta_NBV=sum(upper_Delta_post_NBV),
            Lower_aggregate_delta_NBV=sum(lower_Delta_post_NBV),
            Mean_marginal_effect=mean(marginal_effect),
            Mean_counterfactual_conversion_lift=mean(counterfactual_conversion_lift),
            Mean_converion_lift=mean(conversion_increase17))

write_csv(EU_PM_financial_impact, "EU_PM_financial_impact.csv")


##for_graph_quantile100 <- mydata %>% group_by(OwningTeam,
  ##                                           PartnerType,
    ##                                         NBPV_quantile100,
      ##                                       Type) %>% 
  ##summarise(mean_pre_conversion=mean(X2017Pre_NBPV),
    ##        mean_conversion_growth=mean(conversion_increase17),
      ##      mean_post_pageview=mean(X2017Post_PV),
        ##    mean_post_averageNBV=sum(X2017Post_KnownNBV)/sum(X2017Post_KnownNetBookings),
          ##  mean_pre_pageview=mean(X2017Pre_60DayPV),
          ##  mean_pre_averageNBV=sum(X2017Pre_60DayKnownNBV)/sum(X2017Pre_60DayKnownNetBookings),
          ##  count=n())

for_graph_quantile100 <- mydata %>% group_by(OwningTeam,
                                             PartnerType,
                                             NBPV_quantile100,
                                             Type) %>% 
  summarise(mean_pre_conversion=sum(X2017Pre_60DayKnownNetBookings)/sum(X2017Pre_60DayPV),
            mean_conversion_growth=mean(conversion_increase17),
            mean_post_pageview=mean(X2017Post_PV),
            mean_post_averageNBV=sum(X2017Post_KnownNBV)/sum(X2017Post_KnownNetBookings),
            mean_pre_pageview=mean(X2017Pre_60DayPV),
            mean_pre_averageNBV=sum(X2017Pre_60DayKnownNBV)/sum(X2017Pre_60DayKnownNetBookings),
            count=n())


#################NA FRBO#####################

NA_FRBO_exception_conterfactual <- for_graph_quantile100 %>% filter(OwningTeam=="Americas",
                                                  PartnerType=="FRBO",
                                                  Type=="ExceptionList") %>% 
  group_by(NBPV_quantile100) %>%
  mutate(marginal_effect_NA_FRBO=-NA_FRBO_reg2_weightPV$coefficients[2]- mean_pre_conversion*NA_FRBO_reg2_weightPV$coefficients[3],
         se_NA_FRBO = sqrt(cov_NA_FRBO[2,2]+cov_NA_FRBO[3,3]*mean_pre_conversion^2+2*mean_pre_conversion*cov_NA_FRBO[2,3]),
         marginal_effect_NA_FRBO_upper=marginal_effect_NA_FRBO+1.96*se_NA_FRBO,
         marginal_effect_NA_FRBO_lower=marginal_effect_NA_FRBO-1.96*se_NA_FRBO,
         closed_loop_effect=mean_conversion_growth+marginal_effect_NA_FRBO,
         dollar_win=marginal_effect_NA_FRBO*mean_post_pageview*mean_post_averageNBV,
         dollar_win_upper=marginal_effect_NA_FRBO_upper*mean_post_pageview*mean_post_averageNBV,
         dollar_win_lower=marginal_effect_NA_FRBO_lower*mean_post_pageview*mean_post_averageNBV)

write_csv(NA_FRBO_exception_conterfactual,"NA_FRBO_exception_conterfactual.csv")


#################NA PM######################

NA_PM_graph <- for_graph_quantile100 %>% filter(OwningTeam=="Americas",
                                                PartnerType=="PM") %>%
  mutate(marginal_effect_NA_PM=-NA_PM_reg2_weightPV$coefficients[2]- mean_pre_conversion*NA_PM_reg2_weightPV$coefficients[3],
         se_NA_PM = sqrt(cov_NA_PM[2,2]+cov_NA_PM[3,3]*mean_pre_conversion^2+2*mean_pre_conversion*cov_NA_PM[2,3]),
         marginal_effect_NA_PM_upper=marginal_effect_NA_PM+1.96*se_NA_PM,
         marginal_effect_NA_PM_lower=marginal_effect_NA_PM-1.96*se_NA_PM,
         closed_loop_effect=mean_conversion_growth+marginal_effect_NA_PM,
         dollar_win=marginal_effect_NA_PM*mean_post_pageview*mean_post_averageNBV,
         dollar_win_upper=marginal_effect_NA_PM_upper*mean_post_pageview*mean_post_averageNBV,
         dollar_win_lower=marginal_effect_NA_PM_lower*mean_post_pageview*mean_post_averageNBV,
         count=n())

write_csv(NA_PM_graph,"NA_PM_money.csv")


################EU FRBO###################
EU_FRBO_graph <- for_graph_quantile100 %>% filter(OwningTeam=="Europe",
                                                PartnerType=="FRBO") %>%
  mutate(marginal_effect_EU_FRBO=-EU_FRBO_reg2_weightPV$coefficients[2]-mean_pre_conversion*EU_FRBO_reg2_weightPV$coefficients[3]-mean_pre_conversion^2*EU_FRBO_reg2_weightPV$coefficients[4],
         se_EU_FRBO = sqrt(cov_EU_FRBO[2,2]+
                           cov_EU_FRBO[3,3]*mean_pre_conversion^2+
                           cov_EU_FRBO[4,4]*mean_pre_conversion^4+
                           2*mean_pre_conversion*cov_EU_FRBO[2,3]+
                           2*mean_pre_conversion^2*cov_EU_FRBO[2,4]+
                           2*mean_pre_conversion^3*cov_EU_FRBO[3,4]),
         marginal_effect_EU_FRBO_upper=marginal_effect_EU_FRBO+1.96*se_EU_FRBO,
         marginal_effect_EU_FRBO_lower=marginal_effect_EU_FRBO-1.96*se_EU_FRBO,
         closed_loop_effect=mean_conversion_growth+marginal_effect_EU_FRBO,
         dollar_win=marginal_effect_EU_FRBO*mean_post_pageview*mean_post_averageNBV,
         dollar_win_upper=marginal_effect_EU_FRBO_upper*mean_post_pageview*mean_post_averageNBV,
         dollar_win_lower=marginal_effect_EU_FRBO_lower*mean_post_pageview*mean_post_averageNBV,
         count=n())

write_csv(EU_FRBO_graph,"EU_FRBO_money.csv")

#############EU PM######################
EU_PM_graph <- for_graph_quantile100 %>% filter(OwningTeam=="Europe",
                                                  PartnerType=="PM") %>%
  mutate(marginal_effect_EU_PM=-EU_PM_reg2_weightPV$coefficients[2]-mean_pre_conversion*EU_PM_reg2_weightPV$coefficients[3]-mean_pre_conversion^2*EU_PM_reg2_weightPV$coefficients[4],
         se_EU_PM = sqrt(cov_EU_PM[2,2]+
                             cov_EU_PM[3,3]*mean_pre_conversion^2+
                             cov_EU_PM[4,4]*mean_pre_conversion^4+
                             2*mean_pre_conversion*cov_EU_PM[2,3]+
                             2*mean_pre_conversion^2*cov_EU_PM[2,4]+
                             2*mean_pre_conversion^3*cov_EU_PM[3,4]),
         marginal_effect_EU_PM_upper=marginal_effect_EU_PM+1.96*se_EU_PM,
         marginal_effect_EU_PM_lower=marginal_effect_EU_PM-1.96*se_EU_PM,
         closed_loop_effect=mean_conversion_growth+marginal_effect_EU_PM,
         dollar_win=marginal_effect_EU_PM*mean_post_pageview*mean_post_averageNBV,
         dollar_win_upper=marginal_effect_EU_PM_upper*mean_post_pageview*mean_post_averageNBV,
         dollar_win_lower=marginal_effect_EU_PM_lower*mean_post_pageview*mean_post_averageNBV,
         count=n())

write_csv(EU_PM_graph,"EU_PM_money.csv")
