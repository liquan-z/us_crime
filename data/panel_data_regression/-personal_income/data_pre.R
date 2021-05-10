income <- read.csv("download.csv",skip = 4,header = T,nrows = 153)
income_per <- income[income$LineCode==3,]
# only keep year 2010-2019 data
income_per <- subset(income_per,select = c("GeoName","X2010",      
  "X2011","X2012","X2013","X2014","X2015",      
  "X2016","X2017","X2018","X2019"))
names(income_per) <- c("GeoName",'2010':'2019')
# remove asterisk of GeoName
income_per$GeoName <- gsub('[[:punct:]]+', '', income_per$GeoName)
library(tidyverse)
library(dplyr)
income_per$GeoName <- str_trim(income_per$GeoName)
# transform data into column
income_per <- gather(income_per,year,number,'2010':'2019')

write.csv(income_per,'income.csv')
