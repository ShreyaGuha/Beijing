#using specific library functions

install.packages("data.table") #installing package
library(data.table) #calling library function

library(dplyr) #used for grouping function

install.packages("lubridate")
library(lubridate) #used for dates


#read data
Beijing_RH = read.csv("RH.csv")

#Find daily averages from 6-hourly averages
Beijing_rh <- aggregate.data.frame(Beijing_RH$RH, list(Beijing_RH$Date), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_rh, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_rh.csv", row.names = FALSE)

#read data
Beijing_TCO = read.csv("TCO.csv")

#Find daily averages from 6-hourly averages
Beijing_TCO <- aggregate.data.frame(Beijing_TCO$TCO, list(Beijing_TCO$Date), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_TCO, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_TCO.csv", row.names = FALSE)

