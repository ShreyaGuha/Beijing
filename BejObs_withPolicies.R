####Meteorological detrending for PM2.5 and ozone for Beijing 2011-18####
###Goal: To check relations between different meteorological signals and atmospheric pollutants###


###Preparing the Data###
##using specific library functions##
install.packages("data.table") #installing package
library(data.table) #calling library function
library(dplyr)

#statistical libraries
install.packages("Metrics")
install.packages("tdr")
library(Metrics)
library(tdr)

#read data as csv files
Bej_daily <- read.csv("BODwP.csv")

#Adjusting Date
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
Bej_daily$date_new <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
Bej_daily$date_new <- as.Date (Bej_daily$date_new)

##Manipulate the erroneous values##
#Replace negative numbers in PM2.5 data with NA
Bej_daily$PM2.5[Bej_daily$PM2.5 < 0] <- NA
Bej_daily$Ozone[Bej_daily$Ozone < 0] <- NA
Bej_daily$Temp[Bej_daily$Temp < 0] <- NA
Bej_daily$Td[Bej_daily$Td < 0] <- NA
Bej_daily$Pres[Bej_daily$Pres < 0] <- NA
Bej_daily$Ws[Bej_daily$Ws < 0] <- NA
Bej_daily$Wd[Bej_daily$Wd < 0] <- NA
Bej_daily$Sky[Bej_daily$Sky < 0] <- NA
Bej_daily$Prec1[Bej_daily$Prec1 < 0] <- NA
Bej_daily$Prec6[Bej_daily$Prec6 < 0] <- NA


#replace NAs with zero
Bej_daily[is.na(Bej_daily)] = 0

#changing wind direction to different factors
#angle measured in degrees from true north and direction from which wind is blowing
Bej_daily$Wdf <- round(Bej_daily$Wd/45, digits = 0) + 1


##changing sky cover to percentage
#originally sky cover is given in different oktas
Bej_daily$Sky_p <- Bej_daily$Sky/8 * 100

#assigning numbers 1 to 7 to days
Bej_daily$days <- wday(Bej_daily$date_new)
#1 and 7 here are weekends

#subseting to remove non-numeric column
Beijing_numeric <- subset(Bej_daily, select = -c(ï..Date, date_new)) 

##storing minimum and maximum values for un-scaling in future
meanvec <- sapply(Beijing_numeric,mean)
sdvec <- sapply(Beijing_numeric,sd)

##scaling data##
#define scaling function8
scaledf <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}


#not intending to scale PM2.5 or ozone, hence excluded
#only scaling columns having meteorological variables
#not scaling wind direction and sky cover
lapply(Bej_daily[c(3,4,5,7,9,10)], scaledf)

###Introducing Detrending by KZ filter###
###Using KZ filter###
library("kza")

###Long-term###
Bej_daily$kz_PM_annual <- kz(Bej_daily$PM2.5, m = 365, k = 3)
Bej_daily$kz_ozone_annual <- kz(Bej_daily$Ozone, m = 365, k = 3)
Bej_daily$kz_Temp_annual <- kz(Bej_daily$Temp, m = 365, k = 3)
Bej_daily$kz_Td_annual <- kz(Bej_daily$Td, m = 365, k = 3)
Bej_daily$kz_pres_annual <- kz(Bej_daily$Pres, m = 365, k = 3)
Bej_daily$kz_Ws_annual <- kz(Bej_daily$Ws, m = 365, k = 3)
Bej_daily$kz_Prec1_annual <- kz(Bej_daily$Prec1, m = 365, k =3)
Bej_daily$kz_Prec6_annual <- kz(Bej_daily$Prec6, m = 365, k =3)

##longterm, remaining##
Bej_daily$PM_LT_rem <- Bej_daily$PM2.5 - Bej_daily$kz_PM_annual
Bej_daily$ozone_LT_rem <- Bej_daily$Ozone - Bej_daily$kz_ozone_annual
Bej_daily$Temp_LT_rem <- Bej_daily$Temp - Bej_daily$kz_Temp_annual
Bej_daily$Td_LT_rem <- Bej_daily$Td - Bej_daily$kz_Td_annual
Bej_daily$pres_LT_rem <- Bej_daily$Pres - Bej_daily$kz_pres_annual
Bej_daily$Ws_LT_rem <- Bej_daily$Ws - Bej_daily$kz_Ws_annual
Bej_daily$Prec1_LT_rem <- Bej_daily$Prec1 - Bej_daily$kz_Prec1_annual
Bej_daily$Prec6_LT_rem <- Bej_daily$Prec6 - Bej_daily$kz_Prec6_annual


##Seasonal##
Bej_daily$kz_PM_seasonal <- kz(Bej_daily$PM_LT_rem, m = 15, k = 5)
Bej_daily$kz_ozone_seasonal <- kz(Bej_daily$ozone_LT_rem, m = 15, k = 5)
Bej_daily$kz_Temp_seasonal <- kz(Bej_daily$Temp_LT_rem, m = 15, k = 5)
Bej_daily$kz_Td_seasonal <- kz(Bej_daily$Td_LT_rem, m = 15, k = 5)
Bej_daily$kz_pres_seasonal <- kz(Bej_daily$pres_LT_rem, m = 15, k = 5)
Bej_daily$kz_Ws_seasonal <- kz(Bej_daily$Ws_LT_rem, m = 15, k = 5)
Bej_daily$kz_Prec1_seasonal <- kz(Bej_daily$Prec1_LT_rem, m = 15, k = 5)
Bej_daily$kz_Prec6_seasonal <- kz(Bej_daily$Prec6_LT_rem, m = 15, k = 5)


###Short-term###
Bej_daily$PM_STM <- Bej_daily$PM_LT_rem - Bej_daily$kz_PM_seasonal
Bej_daily$ozone_STM <- Bej_daily$ozone_LT_rem - Bej_daily$kz_ozone_seasonal
Bej_daily$Temp_STM <- Bej_daily$Temp_LT_rem - Bej_daily$kz_Temp_seasonal
Bej_daily$Td_STM <- Bej_daily$Td_LT_rem - Bej_daily$kz_Td_seasonal
Bej_daily$pres_STM <- Bej_daily$pres_LT_rem - Bej_daily$kz_pres_seasonal
Bej_daily$Ws_STM <- Bej_daily$Ws_LT_rem - Bej_daily$kz_Ws_seasonal
Bej_daily$Prec1_STM <- Bej_daily$Prec1_LT_rem - Bej_daily$kz_Prec1_annual
Bej_daily$Prec6_STM <- Bej_daily$Prec6_LT_rem - Bej_daily$kz_Prec6_annual

plot(Bej_daily$PM_STM, type = 'l')


#Model 1: KZ-GLM#
lm_1 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM + pres_STM + Ws_STM + Sky_p +
                    Prec1_STM + as.factor(days) + as.factor(Wdf), data = Bej_daily)
summary(lm_1)

lm_2 <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM + pres_STM + Ws_STM + Sky_p +
             Prec1_STM + as.factor(days) + as.factor(Wdf), data = Bej_daily)
summary(lm_2)



###Linear regression models for short-term data, detrended###
##Model 1##
##Simplest model##
#For PM2.5
lm_comb_1 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  pres_STM + Ws_STM + Prec1_STM + 
                  Pol1 + Pol2 + Pol3 + Pol4 + Pol5 + Pol6 +
                  Pol7 + Pol8 + Pol9, data = Bej_daily)
summary(lm_comb_1)

#non-detrended#
lm_pol_1 <- lm(PM2.5 ~ -1 + Temp + Td + Pres + Ws +
                 Prec1 + Pol1 + Pol2 + Pol3 + Pol4 + 
                 Pol5 + Pol6 + Pol7 + Pol8 + Pol9, 
                 data = Bej_daily)
summary(lm_pol_1)

