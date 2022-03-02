#China spatial plots
#installing packages
install.packages("ggmap")
library("ggmap")

#drawing a preliminary map from google maps
china <- c(left = 75, bottom = 15, right = 135, top = 60)
china_states <- get_stamenmap(china, zoom = 3, maptype = "toner-lite") %>% ggmap()

#drawing a detailed map of China
#installing libraries
install.packages('hchinamap', build_vignettes = TRUE)
library(dplyr)
library(magrittr)
library(hchinamap)

#going to directory
dir <- tempdir()
download.file('https://czxb.github.io/br/chinadf.rda', file.path(dir, 'chinadf.rda'))
load(file.path(dir, 'chinadf.rda'), verbose = TRUE)
china <- chinadf %>%
  dplyr::filter(region == "China")

#reading file
china_2013_pol <- read.csv("China_laws.csv")

#plotting map
china_rough <- hchinamap(name = china$name, value = china$value, 
                         region = "China",
                         width = "100%", height = "400px",
                         title = "Provinces of China implementing Air Pollution and Prevention Control",
                         titleSize = "20px" )

#Additional steps since viewer is not working
#common issue with windows 10 and R 4.0
dev.off()
jpeg('china_rough.jpeg')
#still cannot be visualized!


## try an sf object
library( sf)
library( data.table)
library( ggplot2)
library( maptools)
dir <- tempdir()

# download file
# tried this version, but it's way too detailed and takes too long to plot
# download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_CHN_1_sf.rds', 
#               file.path(dir, 'gadm36_CHN_1_sf.rds'))
# china_sf <- readRDS(file.path(dir, 'province.shp'))

# try this shapefile
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DBJ3BX
# no crs included—will need to guess one eventually maybe

china_sf <-  st_read('province.shp')
plot( china_sf)

# check out the province names
china_sf$NAME_PINGY

#reading file
china_2013_pol <- fread("China_laws.csv")

# check out our province names
china_2013_pol$Province
china_2013_pol$Value

# remove province, municipality, region
china_2013_pol[, Name2 := gsub( 'Province|Municipality|Region|Administrative|Autonomous|Special|Zhuang', '', Name)]
china_2013_pol[, Name2 := gsub( '\\s*$', '', Name2)]

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2013_pol, by.x = "NAME_PINGY", by.y = "Province")

# check missing provinces -- Shreya to ID missing overlaps
china_2013_pol[ which( !( china_2013_pol$Name2 %in% china_sf_dat$NAME_1))]
china_sf$NAME_1

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = china_2013_pol$Value)) + 
  scale_fill_binned() + 
  theme_bw() + 
  theme( legend.position = 'bottom')





