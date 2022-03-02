#using specific library functions

install.packages("data.table") #installing package
library(data.table) #calling library function
library(dplyr) #used for grouping function
install.packages("lubridate")
library(lubridate) #used for dates

#read data
Beijing_ozone_11 = read.csv("Beijing_OZONE_2011_YTD.csv")
#new date columns
Beijing_ozone_11$dateNew <- seq(ymd_h('2011-01-01 0'), ymd_h('2011-12-31 23'), by = "1 hour")
Beijing_ozone_11$datem <- as.Date(Beijing_ozone_11$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_11 <- aggregate.data.frame(Beijing_ozone_11$Raw.Conc, list(Beijing_ozone_11$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_11, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_11.csv", row.names = FALSE)


#read data
Beijing_ozone_12 = read.csv("Beijing_OZONE_2012_YTD.csv")
#new date columns
Beijing_ozone_12$dateNew <- seq(ymd_h('2012-01-01 0'), ymd_h('2012-12-31 23'), by = "1 hour")
Beijing_ozone_12$datem <- as.Date(Beijing_ozone_12$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_12 <- aggregate.data.frame(Beijing_ozone_12$Raw.Conc, list(Beijing_ozone_12$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_12, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_12.csv", row.names = FALSE)


#read data
Beijing_ozone_13 = read.csv("Beijing_OZONE_2013_YTD.csv")
#new date columns
Beijing_ozone_13$dateNew <- seq(ymd_h('2013-01-01 0'), ymd_h('2013-12-31 23'), by = "1 hour")
Beijing_ozone_13$datem <- as.Date(Beijing_ozone_13$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_13 <- aggregate.data.frame(Beijing_ozone_13$Raw.Conc, list(Beijing_ozone_13$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_13, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_13.csv", row.names = FALSE)


#read data
Beijing_ozone_14 = read.csv("Beijing_OZONE_2014_YTD.csv")
#new date columns
Beijing_ozone_14$dateNew <- seq(ymd_h('2014-01-01 0'), ymd_h('2014-12-31 23'), by = "1 hour")
Beijing_ozone_14$datem <- as.Date(Beijing_ozone_14$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_14 <- aggregate.data.frame(Beijing_ozone_14$Raw.Conc, list(Beijing_ozone_14$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_14, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_14.csv", row.names = FALSE)


#read data
Beijing_ozone_15 = read.csv("Beijing_OZONE_2015_YTD.csv")
#new date columns
Beijing_ozone_15$dateNew <- seq(ymd_h('2015-01-01 0'), ymd_h('2015-12-31 23'), by = "1 hour")
Beijing_ozone_15$datem <- as.Date(Beijing_ozone_15$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_15 <- aggregate.data.frame(Beijing_ozone_15$Raw.Conc, list(Beijing_ozone_15$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_15, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_15.csv", row.names = FALSE)


#read data
Beijing_ozone_16 = read.csv("Beijing_OZONE_2016_YTD.csv")
#new date columns
Beijing_ozone_16$dateNew <- seq(ymd_h('2016-01-01 0'), ymd_h('2016-12-31 23'), by = "1 hour")
Beijing_ozone_16$datem <- as.Date(Beijing_ozone_16$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_16 <- aggregate.data.frame(Beijing_ozone_16$Raw.Conc, list(Beijing_ozone_16$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_16, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_16.csv", row.names = FALSE)


#read data
Beijing_ozone_17 = read.csv("Beijing_OZONE_2017_YTD.csv")
#new date columns
Beijing_ozone_17$dateNew <- seq(ymd_h('2017-01-01 0'), ymd_h('2017-12-31 23'), by = "1 hour")
Beijing_ozone_17$datem <- as.Date(Beijing_ozone_17$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_17 <- aggregate.data.frame(Beijing_ozone_17$Raw.Conc, list(Beijing_ozone_17$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_17, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_17.csv", row.names = FALSE)


#read data
Beijing_ozone_18 = read.csv("Beijing_OZONE_2018_YTD.csv")
#new date columns
Beijing_ozone_18$dateNew <- seq(ymd_h('2018-01-01 0'), ymd_h('2018-12-31 23'), by = "1 hour")
Beijing_ozone_18$datem <- as.Date(Beijing_ozone_18$dateNew, format = "%mm%dd%yyyy")

#Find daily averages from hourly averages
Beijing_ozone_18 <- aggregate.data.frame(Beijing_ozone_18$Raw.Conc, list(Beijing_ozone_18$datem), FUN = mean)

#save data in csv format in PC
write.csv(Beijing_ozone_18, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\Beijing_ozone_18.csv", row.names = FALSE)

