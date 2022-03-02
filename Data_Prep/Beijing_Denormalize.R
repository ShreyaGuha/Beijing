####Meteorological detrending for PM2.5 and ozone for Beijing 2011-18####
###Goal: To check relations between different meteorological signals and atmospheric pollutants###


###Preparing the Data###
##using specific library functions##
install.packages("data.table") #installing package
library(data.table) #calling library function

##read data
Beijing_2011_18 = read.csv("Beijing_2011_18.csv")

#calculate wind speed
Beijing_2011_18$wind <- sqrt (Beijing_2011_18$u10^2 + Beijing_2011_18$v10^2)

##Manipulate the erroneous values##
#Replace negative numbers in PM2.5 data with NA
Beijing_2011_18$PM2.5_avg_conc[Beijing_2011_18$PM2.5_avg_conc < 0] <- NA
Beijing_2011_18$TCO[Beijing_2011_18$TCO < 0] <- NA
Beijing_2011_18$PRCP[Beijing_2011_18$PRCP < 0] <- NA
Beijing_2011_18$TAVG[Beijing_2011_18$TAVG < 0] <- NA
Beijing_2011_18$RH[Beijing_2011_18$RH < 0] <- NA
Beijing_2011_18$wind[Beijing_2011_18$wind < 0] <- NA
Beijing_2011_18$PBL[Beijing_2011_18$PBL < 0] <- NA
Beijing_2011_18$Ozone[Beijing_2011_18$Ozone < 0] <- NA

#replace NAs with zero
Beijing_2011_18[is.na(Beijing_2011_18)] = 0


##create new column for date##
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
Beijing_2011_18$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
Beijing_2011_18$year <- as.factor(format(Beijing_2011_18$date, "%y"))
Beijing_2011_18$month <- as.numeric(format(Beijing_2011_18$date, "%m"))
Beijing_2011_18$dayno <- seq(1,2922, by = 1) #dummy variable for days

#subseting to remove non-numeric column
Beijing_numeric <- subset(Beijing_2011_18, select = -c(X, date_new, date, dayno, year, u10, v10)) 

##storing minimum and maximum values for denormalizing in future
minvec <- sapply(Beijing_numeric,min)
maxvec <- sapply(Beijing_numeric,max)

##scaling data##
#define normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
Beijing_2011_18_norm <- as.data.frame(lapply(Beijing_numeric, normalize))
Beijing_2011_18 <- Beijing_2011_18_norm


##PM2.5 and PRCP have logarithmic distributions, linear regression models assume normal distributions
#Transforming into normal distribution
Beijing_2011_18$PM2.5_avg_conc <- log10(Beijing_2011_18$PM2.5_avg_conc + 1)
Beijing_2011_18$PRCP <- log10(Beijing_2011_18$PRCP + 1)

###Introducing Detrending by KZ filter###
###Using KZ filter###
install.packages("kza")
library("kza")

###Long-term###
Beijing_2011_18$kz_PM_annual <- kz(Beijing_2011_18$PM2.5_avg_conc, m = 365, k = 3)
Beijing_2011_18$kz_TCO_annual <- kz(Beijing_2011_18$TCO, m = 365, k = 3)
Beijing_2011_18$kz_ozone_annual <- kz(Beijing_2011_18$Ozone, m = 365, k = 3)
Beijing_2011_18$kz_prec_annual <- kz(Beijing_2011_18$PRCP, m = 365, k = 3)
Beijing_2011_18$kz_temp_annual <- kz(Beijing_2011_18$TAVG, m = 365, k = 3)
Beijing_2011_18$kz_RH_annual <- kz(Beijing_2011_18$RH, m = 365, k = 3)
Beijing_2011_18$kz_wind_annual <- kz(Beijing_2011_18$wind, m = 365, k =3)
Beijing_2011_18$kz_pbl_annual <- kz(Beijing_2011_18$PBL, m = 365, k =3)

##longterm, remaining##
Beijing_2011_18$PM_LT_rem <- Beijing_2011_18$PM2.5_avg_conc - Beijing_2011_18$kz_PM_annual
Beijing_2011_18$TCO_LT_rem <- Beijing_2011_18$TCO - Beijing_2011_18$kz_TCO_annual
Beijing_2011_18$Ozone_LT_rem <- Beijing_2011_18$Ozone - Beijing_2011_18$kz_ozone_annual
Beijing_2011_18$prec_LT_rem <- Beijing_2011_18$PRCP - Beijing_2011_18$kz_prec_annual
Beijing_2011_18$temp_LT_rem <- Beijing_2011_18$TAVG - Beijing_2011_18$kz_temp_annual
Beijing_2011_18$rh_LT_rem <- Beijing_2011_18$RH - Beijing_2011_18$kz_RH_annual
Beijing_2011_18$wind_LT_rem <- Beijing_2011_18$wind - Beijing_2011_18$kz_wind_annual
Beijing_2011_18$pbl_LT_rem <- Beijing_2011_18$PBL - Beijing_2011_18$kz_pbl_annual

##Seasonal##
Beijing_2011_18$kz_PM_seasonal <- kz(Beijing_2011_18$PM_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_TCO_seasonal <- kz(Beijing_2011_18$TCO_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_Ozone_seasonal <- kz(Beijing_2011_18$Ozone_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_prec_seasonal <- kz(Beijing_2011_18$prec_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_temp_seasonal <- kz(Beijing_2011_18$temp_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_rh_seasonal <- kz(Beijing_2011_18$rh_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_wind_seasonal <- kz(Beijing_2011_18$wind_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_pbl_seasonal <- kz(Beijing_2011_18$pbl_LT_rem, m = 15, k = 5)


###Short-term###
Beijing_2011_18$PM_STM <- Beijing_2011_18$PM_LT_rem - Beijing_2011_18$kz_PM_seasonal
Beijing_2011_18$TCO_STM <- Beijing_2011_18$TCO_LT_rem - Beijing_2011_18$kz_TCO_seasonal
Beijing_2011_18$Ozone_STM <- Beijing_2011_18$Ozone_LT_rem - Beijing_2011_18$kz_Ozone_seasonal
Beijing_2011_18$prec_STM <- Beijing_2011_18$prec_LT_rem - Beijing_2011_18$kz_prec_seasonal
Beijing_2011_18$temp_STM <- Beijing_2011_18$temp_LT_rem - Beijing_2011_18$kz_temp_seasonal
Beijing_2011_18$rh_STM <- Beijing_2011_18$rh_LT_rem - Beijing_2011_18$kz_rh_seasonal
Beijing_2011_18$wind_STM <- Beijing_2011_18$wind_LT_rem - Beijing_2011_18$kz_wind_annual
Beijing_2011_18$pbl_STM <- Beijing_2011_18$pbl_LT_rem - Beijing_2011_18$kz_pbl_annual


##correlation_models for PM2.5 for short-term data to check yearly contributions##
lm_comb <- lm(PM_STM ~ -1 + temp_STM * year +
                prec_STM *year + rh_STM *year +
                wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb)

#extract coefficients
coef_lm_comb <- coef(lm_comb)
#convert it into data frame
coef_lm_comb <- data.frame

#Denormalizing the data
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}
Beijing_2011_18_denorm <- as.data.frame(Map(denormalize, Beijing_2011_18_norm, minvec, maxvec))
Beijing_2011_18 <- Beijing_2011_18_denorm


#Recreate yearly contributions
Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_comb$temp_STM * Beijing_2012$TAVG) + (coef_lm_comb$temp_STM.year12 * Beijing_2012$temp_STM)
PM_RH_12 <- (coef_lm_comb$rh_STM * Beijing_2012$RH) + (coef_lm_comb$year12.rh_STM * Beijing_2012$rh_STM)
PM_wind_12 <- (coef_lm_comb$wind_STM * Beijing_2012$wind) + (coef_lm_comb$year12.wind_STM * Beijing_2012$wind_STM)
PM_prec_12 <- (coef_lm_comb$prec_STM * Beijing_2012$PRCP) + (coef_lm_comb$year12.prec_STM * Beijing_2012$prec_STM)
PM_pbl_12 <- (coef_lm_comb$pbl_STM * Beijing_2012$PBL) + (coef_lm_comb$year12.pbl_STM * Beijing_2012$pbl_STM)
PM_12 <- coef_lm_comb$year12 + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_comb$temp_STM * Beijing_2013$TAVG) + (coef_lm_comb$temp_STM.year13 * Beijing_2013$temp_STM)
PM_RH_13 <- (coef_lm_comb$rh_STM * Beijing_2013$RH) + (coef_lm_comb$year13.rh_STM * Beijing_2013$rh_STM)
PM_wind_13 <- (coef_lm_comb$wind_STM * Beijing_2013$wind) + (coef_lm_comb$year13.wind_STM * Beijing_2013$wind_STM)
PM_prec_13 <- (coef_lm_comb$prec_STM * Beijing_2013$PRCP) + (coef_lm_comb$year13.prec_STM * Beijing_2013$prec_STM)
PM_pbl_13 <- (coef_lm_comb$pbl_STM * Beijing_2013$PBL) + (coef_lm_comb$year13.pbl_STM * Beijing_2013$pbl_STM)
PM_13 <- coef_lm_comb$year13 + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_comb$temp_STM * Beijing_2014$TAVG) + (coef_lm_comb$temp_STM.year14 * Beijing_2014$temp_STM)
PM_RH_14 <- (coef_lm_comb$rh_STM * Beijing_2014$RH) + (coef_lm_comb$year14.rh_STM * Beijing_2014$rh_STM)
PM_wind_14 <- (coef_lm_comb$wind_STM * Beijing_2014$wind) + (coef_lm_comb$year14.wind_STM * Beijing_2014$wind_STM)
PM_prec_14 <- (coef_lm_comb$prec_STM * Beijing_2014$PRCP) + (coef_lm_comb$year14.prec_STM * Beijing_2014$prec_STM)
PM_pbl_14 <- (coef_lm_comb$pbl_STM * Beijing_2014$PBL) + (coef_lm_comb$year14.pbl_STM * Beijing_2014$pbl_STM)
PM_14 <- coef_lm_comb$year14 + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_comb$temp_STM * Beijing_2015$TAVG) + (coef_lm_comb$temp_STM.year15 * Beijing_2015$temp_STM)
PM_RH_15 <- (coef_lm_comb$rh_STM * Beijing_2015$RH) + (coef_lm_comb$year15.rh_STM * Beijing_2015$rh_STM)
PM_wind_15 <- (coef_lm_comb$wind_STM * Beijing_2015$wind) + (coef_lm_comb$year15.wind_STM * Beijing_2015$wind_STM)
PM_prec_15 <- (coef_lm_comb$prec_STM * Beijing_2015$PRCP) + (coef_lm_comb$year15.prec_STM * Beijing_2015$prec_STM)
PM_pbl_15 <- (coef_lm_comb$pbl_STM * Beijing_2015$PBL) + (coef_lm_comb$year15.pbl_STM * Beijing_2015$pbl_STM)
PM_15 <- coef_lm_comb$year15 + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_comb$temp_STM * Beijing_2016$TAVG) + (coef_lm_comb$temp_STM.year16 * Beijing_2016$temp_STM)
PM_RH_16 <- (coef_lm_comb$rh_STM * Beijing_2016$RH) + (coef_lm_comb$year16.rh_STM * Beijing_2016$rh_STM)
PM_wind_16 <- (coef_lm_comb$wind_STM * Beijing_2016$wind) + (coef_lm_comb$year16.wind_STM * Beijing_2016$wind_STM)
PM_prec_16 <- (coef_lm_comb$prec_STM * Beijing_2016$PRCP) + (coef_lm_comb$year16.prec_STM * Beijing_2016$prec_STM)
PM_pbl_16 <- (coef_lm_comb$pbl_STM * Beijing_2016$PBL) + (coef_lm_comb$year16.pbl_STM * Beijing_2016$pbl_STM)
PM_16 <- coef_lm_comb$year16 + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_comb$temp_STM * Beijing_2017$TAVG) + (coef_lm_comb$temp_STM.year17 * Beijing_2017$temp_STM)
PM_RH_17 <- (coef_lm_comb$rh_STM * Beijing_2017$RH) + (coef_lm_comb$year17.rh_STM * Beijing_2017$rh_STM)
PM_wind_17 <- (coef_lm_comb$wind_STM * Beijing_2017$wind) + (coef_lm_comb$year17.wind_STM * Beijing_2017$wind_STM)
PM_prec_17 <- (coef_lm_comb$prec_STM * Beijing_2017$PRCP) + (coef_lm_comb$year17.prec_STM * Beijing_2017$prec_STM)
PM_pbl_17 <- (coef_lm_comb$pbl_STM * Beijing_2017$PBL) + (coef_lm_comb$year17.pbl_STM * Beijing_2017$pbl_STM)
PM_17 <- coef_lm_comb$year17 + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_comb$temp_STM * Beijing_2018$TAVG) + (coef_lm_comb$temp_STM.year18 * Beijing_2018$temp_STM)
PM_RH_18 <- (coef_lm_comb$rh_STM * Beijing_2018$RH) + (coef_lm_comb$year18.rh_STM * Beijing_2018$rh_STM)
PM_wind_18 <- (coef_lm_comb$wind_STM * Beijing_2018$wind) + (coef_lm_comb$year18.wind_STM * Beijing_2018$wind_STM)
PM_prec_18 <- (coef_lm_comb$prec_STM * Beijing_2018$PRCP) + (coef_lm_comb$year18.prec_STM * Beijing_2018$prec_STM)
PM_pbl_18 <- (coef_lm_comb$pbl_STM * Beijing_2018$PBL) + (coef_lm_comb$year18.pbl_STM * Beijing_2018$pbl_STM)
PM_18 <- coef_lm_comb$year18 + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18





##plot yearly contributions using ggplot##
library(tidyverse)
library(reshape)
library(ggplot2)

#build a subset with only the variables required
Beijing_plot <- subset(Beijing_2011_18, select = c(PM_STM, Ozone_STM, prec_STM, temp_STM, 
                                                   rh_STM, wind_STM, TCO_STM, pbl_STM,
                                                   year, date))
#use melt function to define the x-axis, against what you're plotting
Beijing_plot.melt <- melt( Beijing_plot, id.vars = c( 'year', 'date'))
Beijing_plot.melt$date <- as.Date( Beijing_plot.melt$date)

#set background theme
theme_set(theme_bw())
#plot
ggplot( Beijing_plot.melt) + (aes( x = date, y = value,
                                   group = variable, color = variable)) +
  geom_line() + 
  facet_wrap( . ~ variable)