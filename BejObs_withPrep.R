###Beijing observational data: 2013-2018###
##Source: China Meteorological Data, with help from Anthony##
##Beijing International Airport station (40.0799° N, 116.6031° E) code: 545110 ##
##Goal: Extract observed meteorological data for Beijing##

##Preparing the Data##
#using specific library functions
library("tools")
#getting and setting specific directory
getwd()
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Beijing_obs")

#read data as data table
Bej_obs_11 <- read.table("545110-99999-2011")
Bej_obs_12 <- read.table("545110-99999-2012")
Bej_obs_13 <- read.table("545110-99999-2013")
Bej_obs_14 <- read.table("545110-99999-2014")
Bej_obs_15 <- read.table("545110-99999-2015")
Bej_obs_16 <- read.table("545110-99999-2016")
Bej_obs_17 <- read.table("545110-99999-2017")
Bej_obs_18 <- read.table("545110-99999-2018")

#export data as a .csv file
write.csv(Bej_obs_11, file = "Bej_obs_11.csv", row.names = F)
write.csv(Bej_obs_12, file = "Bej_obs_12.csv", row.names = F)
write.csv(Bej_obs_13, file = "Bej_obs_13.csv", row.names = F)
write.csv(Bej_obs_14, file = "Bej_obs_14.csv", row.names = F)
write.csv(Bej_obs_15, file = "Bej_obs_15.csv", row.names = F)
write.csv(Bej_obs_16, file = "Bej_obs_16.csv", row.names = F)
write.csv(Bej_obs_17, file = "Bej_obs_17.csv", row.names = F)
write.csv(Bej_obs_18, file = "Bej_obs_18.csv", row.names = F)


#combine data
Bej_obs <- rbind (Bej_obs_11, Bej_obs_12, Bej_obs_13, Bej_obs_14, 
                  Bej_obs_15, Bej_obs_16, Bej_obs_17, Bej_obs_18)
#export data as a .csv file
write.csv(Bej_obs, file = "Bej_obs.csv", row.names = F)


#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Beijing_PM2.5")

#read data as csv files
Bej_11 <- read.csv("Beijing_PM2.5_2011_YTD.csv")
Bej_12 <- read.csv("Beijing_PM2.5_2012_YTD.csv")
Bej_13 <- read.csv("Beijing_PM2.5_2013_YTD.csv")
Bej_14 <- read.csv("Beijing_PM2.5_2014_YTD.csv")
Bej_15 <- read.csv("Beijing_PM2.5_2015_YTD.csv")
Bej_16 <- read.csv("Beijing_PM2.5_2016_YTD.csv")
Bej_17 <- read.csv("Beijing_PM2.5_2017_YTD.csv")
Bej_18 <- read.csv("Beijing_PM2.5_2018_YTD.csv")

#combine data
Bej_PM2.5 <- rbind(Bej_11, Bej_12, Bej_13, Bej_14,
                   Bej_15, Bej_16, Bej_17, Bej_18)

#export data as a .csv file
write.csv(Bej_PM2.5, file = "Bej_PM2.5.csv", row.names = F)

##read data
Beijing_obs = read.csv("Bej_obs.csv")
Bej_PM2.5 = read.csv("Bej_PM2.5.csv")

#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents")

##read data again in main directory
Beijing_obs = read.csv("Bej_obs.csv")
Bej_PM2.5 = read.csv("Bej_PM2.5.csv")

#merging data
Bej_obs <-merge(Beijing_obs, Bej_PM2.5, all = TRUE)
write.csv(Bej_obs, file = "Bej_obs.csv", row.names = F)

#read joined data
Bej_obs = read.csv("Bej_obs.csv")
Bej_obs = read.csv("Bej_obs_2.csv")

#create single date column
Bej_obs$date <- paste(Bej_obs$Year, Bej_obs$Month, Bej_obs$Day, Bej_obs$Hour, sep= "-")
Bej_obs$date <- as.Date(Bej_obs$date)


#Find daily averages from hourly averages and export data as a .csv file
Temp <- aggregate.data.frame(Bej_obs$Temp, list(Bej_obs$date), FUN = mean)
write.csv(Temp, file = "Temp.csv", row.names = F)
Td <- aggregate.data.frame(Bej_obs$Td, list(Bej_obs$date), FUN = mean)
write.csv(Td, file = "Td.csv", row.names = F)
Pres <- aggregate.data.frame(Bej_obs$Pres, list(Bej_obs$date), FUN = mean)
write.csv(Pres, file = "Pres.csv", row.names = F)
Wd <- aggregate.data.frame(Bej_obs$Wd, list(Bej_obs$date), FUN = mean)
write.csv(Wd, file = "Wd.csv", row.names = F)
Ws <- aggregate.data.frame(Bej_obs$Ws, list(Bej_obs$date), FUN = mean)
write.csv(Ws, file = "Ws.csv", row.names = F)
Sky <- aggregate.data.frame(Bej_obs$Sky, list(Bej_obs$date), FUN = mean)
write.csv(Sky, file = "Sky.csv", row.names = F)
Prec1 <- aggregate.data.frame(Bej_obs$Prec1, list(Bej_obs$date), FUN = mean)
write.csv(Prec1, file = "Prec1.csv", row.names = F)
Prec6 <- aggregate.data.frame(Bej_obs$Prec6, list(Bej_obs$date), FUN = mean)
write.csv(Prec6, file = "Prec6.csv", row.names = F)
PM2.5 <- aggregate.data.frame(Bej_obs$Raw.Conc., list(Bej_obs$date), FUN = mean)
write.csv(PM2.5, file = "PM2.5.csv", row.names = F)

#read data as csv files
Temp <- read.csv("Temp.csv")
Td <- read.csv("Td.csv")
Pres <- read.csv("Pres.csv")
Ws <- read.csv("Ws.csv")
Wd <- read.csv("Wd.csv")
Sky <- read.csv("Sky.csv")
Prec1 <- read.csv("Prec1.csv")
Prec6 <- read.csv("Prec6.csv")
PM2.5 <- read.csv("PM2.5.csv")

#combine data
Bej_daily_obs <- cbind (PM2.5, Temp, Td, Pres, Wd, Ws, Sky, Prec1, Prec6)
write.csv(Bej_daily_obs, file = "Bej_daily_obs.csv", row.names = F)

#read data as csv files
Bej_daily <- read.csv("Bej_daily.csv")

#Adjusting Date
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

#subseting to remove non-numeric column
Beijing_numeric <- subset(Bej_daily, select = -c(Date, date_new)) 

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
lapply(Bej_daily[-c(1,2,11,12)], scaledf)

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
Bej_daily$kz_Wd_annual <- kz(Bej_daily$Wd, m = 365, k = 3)
Bej_daily$kz_Sky_annual <- kz(Bej_daily$Sky, m = 365, k =3)
Bej_daily$kz_Prec1_annual <- kz(Bej_daily$Prec1, m = 365, k =3)
Bej_daily$kz_Prec6_annual <- kz(Bej_daily$Prec6, m = 365, k =3)

##longterm, remaining##
Bej_daily$PM_LT_rem <- Bej_daily$PM2.5 - Bej_daily$kz_PM_annual
Bej_daily$ozone_LT_rem <- Bej_daily$Ozone - Bej_daily$kz_ozone_annual
Bej_daily$Temp_LT_rem <- Bej_daily$Temp - Bej_daily$kz_Temp_annual
Bej_daily$Td_LT_rem <- Bej_daily$Td - Bej_daily$kz_Td_annual
Bej_daily$pres_LT_rem <- Bej_daily$Pres - Bej_daily$kz_pres_annual
Bej_daily$Ws_LT_rem <- Bej_daily$Ws - Bej_daily$kz_Ws_annual
Bej_daily$Wd_LT_rem <- Bej_daily$Wd - Bej_daily$kz_Wd_annual
Bej_daily$Sky_LT_rem <- Bej_daily$Sky - Bej_daily$kz_Sky_annual
Bej_daily$Prec1_LT_rem <- Bej_daily$Prec1 - Bej_daily$kz_Prec1_annual
Bej_daily$Prec6_LT_rem <- Bej_daily$Prec6 - Bej_daily$kz_Prec6_annual


##Seasonal##
Bej_daily$kz_PM_seasonal <- kz(Bej_daily$PM_LT_rem, m = 15, k = 5)
Bej_daily$kz_ozone_seasonal <- kz(Bej_daily$ozone_LT_rem, m = 15, k = 5)
Bej_daily$kz_Temp_seasonal <- kz(Bej_daily$Temp_LT_rem, m = 15, k = 5)
Bej_daily$kz_Td_seasonal <- kz(Bej_daily$Td_LT_rem, m = 15, k = 5)
Bej_daily$kz_pres_seasonal <- kz(Bej_daily$pres_LT_rem, m = 15, k = 5)
Bej_daily$kz_Ws_seasonal <- kz(Bej_daily$Ws_LT_rem, m = 15, k = 5)
Bej_daily$kz_Wd_seasonal <- kz(Bej_daily$Wd_LT_rem, m = 15, k = 5)
Bej_daily$kz_Sky_seasonal <- kz(Bej_daily$Sky_LT_rem, m = 15, k = 5)
Bej_daily$kz_Prec1_seasonal <- kz(Bej_daily$Prec1_LT_rem, m = 15, k = 5)
Bej_daily$kz_Prec6_seasonal <- kz(Bej_daily$Prec6_LT_rem, m = 15, k = 5)


###Short-term###
Bej_daily$PM_STM <- Bej_daily$PM_LT_rem - Bej_daily$kz_PM_seasonal
Bej_daily$ozone_STM <- Bej_daily$ozone_LT_rem - Bej_daily$kz_ozone_seasonal
Bej_daily$Temp_STM <- Bej_daily$Temp_LT_rem - Bej_daily$kz_Temp_seasonal
Bej_daily$Td_STM <- Bej_daily$Td_LT_rem - Bej_daily$kz_Td_seasonal
Bej_daily$pres_STM <- Bej_daily$pres_LT_rem - Bej_daily$kz_pres_seasonal
Bej_daily$Ws_STM <- Bej_daily$Ws_LT_rem - Bej_daily$kz_Ws_seasonal
Bej_daily$Wd_STM <- Bej_daily$Wd_LT_rem - Bej_daily$kz_Wd_seasonal
Bej_daily$Sky_STM <- Bej_daily$Sky_LT_rem - Bej_daily$kz_Sky_annual
Bej_daily$Prec1_STM <- Bej_daily$Prec1_LT_rem - Bej_daily$kz_Prec1_annual
Bej_daily$Prec6_STM <- Bej_daily$Prec6_LT_rem - Bej_daily$kz_Prec6_annual

plot(Bej_daily$PM_STM, type = 'l')

###Linear regression models for short-term data, detrended###
##Model 1##
##Simplest model##
#For PM2.5
lm_comb_1 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  pres_STM + Ws_STM + Prec1_STM , data = Bej_daily)
summary(lm_comb_1)
#temp, td, Wind speed significant
#pres, prec values are zero due to data manipulation
#wind direction and sky cover are included in model#2
lm_comb_1a <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                  pres_STM + Ws_STM + Prec1_STM , data = Bej_daily)
summary(lm_comb_1a)

#assigning numbers 1 to 7 to days
Bej_daily$days <- wday(Bej_daily$date_new)


lm_comb_2 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  pres_STM + Ws_STM + Prec1_STM + 
                  Wdf + Sky_c + days,
                data = Bej_daily)
summary(lm_comb_2)


#removing pressure and precipitation, as they are insignificant
lm_comb_3 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + Wd + Sky + days,
                data = Bej_daily)
summary(lm_comb_3)

lm_comb_3a <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + Wd + Sky + days,
                data = Bej_daily)
summary(lm_comb_3a)

#taking wind direction as factor
lm_comb_4 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + Sky + days + as.factor(Wd),
                data = Bej_daily)
summary(lm_comb_4)
#we do not need all the directions


###General Additive Models(GAMs)###
library(mgcv)

#Simple GAM by component, smoothing parameter = 3
#Variables taken: Temp, dew point temp, Wind speed
gam1 <- gam(Bej_daily$PM_STM ~ s(Bej_daily$Temp_STM, sp = 3) + 
              s(Bej_daily$Td_STM, sp = 3) + s(Bej_daily$Ws_STM, sp = 3),
            method = "REML" )
summary(gam1)
#visualize
plot(gam1, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

gam1a <- gam(Bej_daily$ozone_STM ~ s(Bej_daily$Temp_STM, sp = 3) + 
              s(Bej_daily$Td_STM, sp = 3) + s(Bej_daily$Ws_STM, sp = 3),
            method = "REML" )
summary(gam1a)
#visualize
plot(gam1a, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


#GAM treating each variable as factor
#For weekday/weekend
gam2a <- gam(Bej_daily$PM_STM ~ s(Bej_daily$PM_STM, by = Bej_daily$days))
summary(gam2a)
plot(gam2a, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#For wind direction
gam2b <- gam(Bej_daily$PM_STM ~ s(Bej_daily$PM_STM, by = Bej_daily$Wd))
summary(gam2b)
plot(gam2b, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#For Sky cover
gam2c <- gam(Bej_daily$PM_STM ~ s(Bej_daily$PM_STM, by = Bej_daily$Sky))
summary(gam2c)
plot(gam2c, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)



##Artificial Neural Network (ANN)##
install.packages("neuralnet")
library(neuralnet)

#2 hidden layers
nn2 = neuralnet(PM_STM ~ -1 + Temp_STM + Td_STM +
               Ws_STM + Wd + Sky + days,data = Bej_daily, hidden = 2, 
               act.fct = "logistic", linear.output = FALSE)
summary(nn2)
# plot neural network
plot(nn2)


#3 hidden layers
nn3 = neuralnet(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + Wd + days,data = Bej_daily, hidden = 3, 
                act.fct = "logistic", linear.output = FALSE)
summary(nn3)
# plot neural network
plot(nn3)


##Random Forest(RF)##
install.packages("party") #required for other libraries used in rf
install.packages("randomForest")
library(party)
library(randomForest)
rf1 = randomForest(PM_STM ~ -1 + Temp_STM + Td_STM +
                     Ws_STM + Wd + Sky + days, data = Bej_daily, importance=TRUE)

summary(rf1) #summary
print(rf1) #view forest results
plot(rf1)

#getting values for a particular tree
rf1_tree23 <- getTree(rf1, k=23, labelVar=TRUE)
plot(rf1_tree23)


install.packages("ggRandomForests")
library(ggRandomForests)

#Variable Importance
plot(gg_vimp(rf1), relative=TRUE)



#no of trees= 1000
rf2 = randomForest(PM_STM ~ -1 + Temp_STM + Td_STM +
                     Ws_STM + Wd + Sky + days, data = Bej_daily, ntree=1000,
                   importance=TRUE)

summary(rf2) #summary
print(rf2) #view forest results
plot(rf2)

##Conditional Inference Trees##
ctree1 <- ctree(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + Wd + Sky + days, data = Bej_daily)
summary(ctree1)
plot(ctree1, type="simple")

