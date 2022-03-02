library(data.table)
library(dplyr)
library(lubridate) 

#read data
Beijing_PBL = read.csv("PBL.csv")
summary(Beijing_PBL$Date)
Beijing_PBL$dateNew <- seq(ymd_h('2011-01-01 0'), ymd_h('2018-12-31 24'), by = "6 hours")
Beijing_PBL$datem <- as.Date(Beijing_PBL$dateNew, format = "%mm%dd%yyyy")


#Find daily averages from 6-hourly averages
Beijing_PBL <- aggregate.data.frame(Beijing_PBL$PBL, list(Beijing_PBL$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_PBL, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_PBL.csv", row.names = FALSE)
