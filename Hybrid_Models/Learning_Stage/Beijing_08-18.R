#using specific library functions
install.packages("data.table") #installing package
library(data.table) #calling library function

#read data
Beijing_2011_18 = read.csv("Beijing_2011_18.csv")

#Replace negative numbers in PM2.5 data with NA
Beijing_2011_18$PM2.5_avg_conc[Beijing_2011_18$PM2.5_avg_conc < 0] <- NA
Beijing_2011_18$PRCP[Beijing_2011_18$PRCP < 0] <- NA
Beijing_2011_18$TAVG[Beijing_2011_18$TAVG < 0] <- NA
Beijing_2011_18$RH[Beijing_2011_18$RH < 0] <- NA

#create new column for date
install.packages("lubridate")
library(lubridate)
Beijing_2011_18$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
Beijing_2011_18$days <- wday(Beijing_2011_18$date)

#kz filter
install.packages("kza")
library("kza")

#longterm
Beijing_2011_18$kz_PM_annual <- kz(Beijing_2011_18$PM2.5_avg_conc, m = 365, k = 3)
Beijing_2011_18$kz_prec_annual <- kz(Beijing_2011_18$PRCP, m = 365, k = 3)
Beijing_2011_18$kz_temp_annual <- kz(Beijing_2011_18$TAVG, m = 365, k = 3)
Beijing_2011_18$kz_RH_annual <- kz(Beijing_2011_18$RH, m = 365, k = 3)
Beijing_2011_18$kz_days_annual <- kz(Beijing_2011_18$days, m = 365, k = 3)

#longterm, remaining
Beijing_2011_18$PM_LT_rem <- Beijing_2011_18$PM2.5_avg_conc - Beijing_2011_18$kz_PM_annual
Beijing_2011_18$prec_LT_rem <- Beijing_2011_18$PRCP - Beijing_2011_18$kz_prec_annual
Beijing_2011_18$temp_LT_rem <- Beijing_2011_18$TAVG - Beijing_2011_18$kz_temp_annual
Beijing_2011_18$rh_LT_rem <- Beijing_2011_18$RH - Beijing_2011_18$kz_RH_annual
Beijing_2011_18$days_LT_rem <- Beijing_2011_18$days - Beijing_2011_18$kz_days_annual

#seasonal
Beijing_2011_18$kz_PM_seasonal <- kz(Beijing_2011_18$PM_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_prec_seasonal <- kz(Beijing_2011_18$prec_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_temp_seasonal <- kz(Beijing_2011_18$temp_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_rh_seasonal <- kz(Beijing_2011_18$rh_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_days_seasonal <- kz(Beijing_2011_18$days_LT_rem, m = 15, k = 5)

#short-term
Beijing_2011_18$PM_STM <- Beijing_2011_18$PM_LT_rem - Beijing_2011_18$kz_PM_seasonal
Beijing_2011_18$prec_STM <- Beijing_2011_18$prec_LT_rem - Beijing_2011_18$kz_prec_seasonal
Beijing_2011_18$temp_STM <- Beijing_2011_18$temp_LT_rem - Beijing_2011_18$kz_temp_seasonal
Beijing_2011_18$rh_STM <- Beijing_2011_18$rh_LT_rem - Beijing_2011_18$kz_rh_seasonal
Beijing_2011_18$days_STM <- Beijing_2011_18$days_LT_rem - Beijing_2011_18$kz_days_seasonal

#correlation model
lm <- lm(Beijing_2011_18$PM_STM ~ Beijing_2011_18$temp_STM + Beijing_2011_18$prec_STM + Beijing_2011_18$rh_STM + Beijing_2011_18$days_STM)
summary(lm) 
plot(lm)

#PM2.5 plots
plot(x = Beijing_2011_18$date, y = Beijing_2011_18$PM2.5_avg_conc,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$kz_PM_annual,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18) after applying long-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$kz_PM_seasonal,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18)after applying seasonal KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid4", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$PM_STM,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18) after applying short-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumpurple4", las = 1)

