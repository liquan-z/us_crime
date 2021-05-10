library(readxl)
# 2010 land area ------------------------------------------------------------
land_area_2010 <- read_excel("area_2010census.xlsx",col_names = F,
                           range = 'A7:D57')
land_area_2010 <- subset(land_area_2010,select = c(...1,...4))
names(land_area_2010) <- c('region','land_area')


library(tidyverse)
# remove white space of state name  and change name into lower characters
land_area_2010$region <- str_trim(land_area_2010$region)
land_area_2010$region <- tolower(land_area_2010$region)



# 2000 land area------------------------------------------------------------
land_area_2000 <- read.csv("land_area_2000.csv",skip=3,header = F,
                           nrow=51,colClasses = c(NA,NA,'NULL'))
names(land_area_2000) <- c('region','land_area')

land_area_2000$region <- gsub('[[:digit:]]+', '', land_area_2000$region)
land_area_2000$region <- gsub('[[:punct:]]+', '', land_area_2000$region)

library(tidyverse)
# remove white space of state name  and change name into lower characters
land_area_2000$region <- str_trim(land_area_2000$region)
land_area_2000$region <- tolower(land_area_2000$region)

# 1990 land area------------------------------------------------------------
land_area_1990 <- read.csv("land_area_1990.csv",skip=3,header = F,
                           nrow=51,colClasses = c(NA,NA,'NULL'))
names(land_area_1990) <- c('region','land_area')

land_area_1990$region <- gsub('[[:digit:]]+', '', land_area_1990$region)
land_area_1990$region <- gsub('[[:punct:]]+', '', land_area_1990$region)

library(tidyverse)
# remove white space of state name  and change name into lower characters
land_area_1990$region <- str_trim(land_area_1990$region)
land_area_1990$region <- tolower(land_area_1990$region)

# read data crime_cluster-------------------------------------------------------
crime_data <- readRDS('crime_cluster.rds')

# pick crime_data not PR
# remove P.R, then we have 50 states and DC
crime_data <- crime_data[crime_data$state != 'puerto rico',]


# calculate 1995-1999 population density----------------------------------------

# choose the data from 1995 to 1999
crime99 <- crime_data[crime_data$year %in% 1995:1999, ] 

# repeat land area 5 times
region99 <- rep(land_area_1990$region,5)
land_area1999 <- rep(land_area_1990$land_area,5)

# check if the order is same
sum(region99==crime99$state)

# calculate density
crime99$land_area1999 <- land_area1999
crime99$density <- crime99$population/crime99$land_area1999


# calculate 2000-2009 population density----------------------------------------

# choose the data from 2000 to 2009
crime09 <- crime_data[crime_data$year %in% 2000:2009, ] 

# repeat land area 10 times
region09 <- rep(land_area_2000$region,10)
land_area2009 <- rep(land_area_2000$land_area,10)

# check if the order is same
sum(region09==crime09$state)

# calculate density
crime09$land_area2009 <- land_area2009
crime09$density <- crime09$population/crime09$land_area2009

# calculate 2010-2018 population density----------------------------------------

# choose the data from 2010 to 2018
crime18 <- crime_data[crime_data$year %in% 2010:2018, ] 

# repeat land area 9 times
region18 <- rep(land_area_2010$region,9)
land_area2018 <- rep(land_area_2010$land_area,9)

# check if the order is same
sum(region18==crime18$state)

# calculate density
crime18$land_area2018 <- land_area2018
crime18$density <- crime18$population/crime18$land_area2018

# combine-----------------------------------------------------------------------
names(crime99)[6] <- 'land_area'
names(crime09)[6] <- 'land_area'
names(crime18)[6] <- 'land_area'
popu_dens <- rbind(crime99,crime09,crime18)

# select columns to save
popu_dens <- subset(popu_dens,select = c(year,state,property_crime,
                                         violent_crime,density))
write.csv(popu_dens,'popu_dens.csv')

