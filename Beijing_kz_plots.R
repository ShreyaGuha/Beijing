#using specific library functions
install.packages("data.table") #installing package
library(data.table) #calling library function

#read data
Beijing_2008_18 = read.csv("Beijing_2008_18.csv")

#Replace negative numbers in PM2.5 data with NA
Beijing_2008_18$PM2.5_avg_conc[Beijing_2008_18$PM2.5_avg_conc < 0] <- NA
Beijing_2008_18$PRCP[Beijing_2008_18$PRCP < 0] <- NA
Beijing_2008_18$TAVG[Beijing_2008_18$TAVG < 0] <- NA

#create new column for date
install.packages("lubridate")
library(lubridate)
Beijing_2008_18$date <- seq(ymd('2008-01-01'), ymd('2018-12-31'), by = "1 day")
Beijing_2008_18$dow <- 1:7
Beijing_2008_18$days <- wday(Beijing_2008_18$date)
                    
install.packages("seas")
library("seas")
# add months/seasons
Beijing_2008_18$month  <- mkseas(Beijing_2008_18,"mon") # add months
Beijing_2008_18$seas <- mkseas(Beijing_2008_18,"DJF")   # add seasons

#kz filter
install.packages("kza")
library("kza")

#longterm
Beijing_2008_18$kz_PM_annual <- kz(Beijing_2008_18$PM2.5_avg_conc, m = 365, k = 3)
Beijing_2008_18$kz_prec_annual <- kz(Beijing_2008_18$PRCP, m = 365, k = 3)
Beijing_2008_18$kz_temp_annual <- kz(Beijing_2008_18$TAVG, m = 365, k = 3)
Beijing_2008_18$kz_days_annual <- kz(Beijing_2008_18$dow, m = 365, k = 3)


#longterm, remaining
Beijing_2008_18$PM_LT_rem <- Beijing_2008_18$PM2.5_avg_conc - Beijing_2008_18$kz_PM_annual
Beijing_2008_18$prec_LT_rem <- Beijing_2008_18$PRCP - Beijing_2008_18$kz_prec_annual
Beijing_2008_18$temp_LT_rem <- Beijing_2008_18$TAVG - Beijing_2008_18$kz_temp_annual
Beijing_2008_18$days_LT_rem <- Beijing_2008_18$dow - Beijing_2008_18$kz_days_annual

#seasonal
Beijing_2008_18$kz_PM_annual2 <- kz(Beijing_2008_18$PM_LT_rem, m = 15, k = 5)
Beijing_2008_18$kz_prec_annual2 <- kz(Beijing_2008_18$prec_LT_rem, m = 15, k = 5)
Beijing_2008_18$kz_temp_annual2 <- kz(Beijing_2008_18$temp_LT_rem, m = 15, k = 5)
Beijing_2008_18$kz_days_annual2 <- kz(Beijing_2008_18$days_LT_rem, m = 15, k = 5)

#short-term
Beijing_2008_18$PM_STM <- Beijing_2008_18$PM_LT_rem - Beijing_2008_18$kz_PM_annual2
Beijing_2008_18$prec_STM <- Beijing_2008_18$prec_LT_rem - Beijing_2008_18$kz_prec_annual2
Beijing_2008_18$temp_STM <- Beijing_2008_18$temp_LT_rem - Beijing_2008_18$kz_temp_annual2
Beijing_2008_18$days_STM <- Beijing_2008_18$days_LT_rem - Beijing_2008_18$kz_days_annual2

#correlation model
lm4 <- lm(Beijing_2008_18$PM_STM ~ Beijing_2008_18$temp_STM + Beijing_2008_18$prec_STM + Beijing_2008_18$days_STM)
summary(lm4)
plot(lm4)

#Factor regression, day of week, done seperately
lm_dow <- lm(Beijing_2008_18$PM2.5_avg_conc ~ as.factor(Beijing_2008_18$dow) -1 )
summary(lm_dow)
plot(lm_dow)



