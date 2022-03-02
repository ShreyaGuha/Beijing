#using specific library functions

install.packages("data.table") #installing package
library(data.table) #calling library function

library(dplyr) #used for grouping function

install.packages("lubridate")
library(lubridate) #used for dates


#read data
Beijing_u_10 = read.csv("u-10.csv")
Beijing_v_10 = read.csv("v-10.csv")

#Find daily averages from 6-hourly averages
Beijing_u_10 <- aggregate.data.frame(Beijing_u_10$u-10, list(Beijing_u_10$Date), FUN = mean)
Beijing_v_10 <- aggregate.data.frame(Beijing_v_10$v-10, list(Beijing_v_10$Date), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_u_10, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_u_10.csv", row.names = FALSE)
write.csv(Beijing_v_10, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_v_10.csv", row.names = FALSE)

