# get data prepared for cluster analysis
# merge data from year 1995 to year 2019

library(tidyverse)
library(dplyr)
# merge 1995-2019 Crime Data sets
crime1995 <- read.csv("../../data/1995-2019_reorganized_data/crime_1995.csv")
crime1996 <- read.csv("../../data/1995-2019_reorganized_data/crime_1996.csv")
crime1997 <- read.csv("../../data/1995-2019_reorganized_data/crime_1997.csv")
crime1998 <- read.csv("../../data/1995-2019_reorganized_data/crime_1998.csv")
crime1999 <- read.csv("../../data/1995-2019_reorganized_data/crime_1999.csv")
crime2000 <- read.csv("../../data/1995-2019_reorganized_data/crime_2000.csv")
crime2001 <- read.csv("../../data/1995-2019_reorganized_data/crime_2001.csv")
crime2002 <- read.csv("../../data/1995-2019_reorganized_data/crime_2002.csv")
crime2003 <- read.csv("../../data/1995-2019_reorganized_data/crime_2003.csv")
crime2004 <- read.csv("../../data/1995-2019_reorganized_data/crime_2004.csv")
crime2005 <- read.csv("../../data/1995-2019_reorganized_data/crime_2005.csv")
crime2006 <- read.csv("../../data/1995-2019_reorganized_data/crime_2006.csv")
crime2007 <- read.csv("../../data/1995-2019_reorganized_data/crime_2007.csv")
crime2008 <- read.csv("../../data/1995-2019_reorganized_data/crime_2008.csv")
crime2009 <- read.csv("../../data/1995-2019_reorganized_data/crime_2009.csv")
crime2010 <- read.csv("../../data/1995-2019_reorganized_data/crime_2010.csv")
crime2011 <- read.csv("../../data/1995-2019_reorganized_data/crime_2011.csv")
crime2012 <- read.csv("../../data/1995-2019_reorganized_data/crime_2012.csv")
crime2013 <- read.csv("../../data/1995-2019_reorganized_data/crime_2013.csv")
crime2014 <- read.csv("../../data/1995-2019_reorganized_data/crime_2014.csv")
crime2015 <- read.csv("../../data/1995-2019_reorganized_data/crime_2015.csv")
crime2016 <- read.csv("../../data/1995-2019_reorganized_data/crime_2016.csv")
crime2017 <- read.csv("../../data/1995-2019_reorganized_data/crime_2017.csv")
crime2018 <- read.csv("../../data/1995-2019_reorganized_data/crime_2018.csv")
crime2019 <- read.csv("../../data/1995-2019_reorganized_data/crime_2019.csv")


crime <- rbind(crime1995,crime1996,crime1997,crime1998,crime1999,
               crime2000,crime2001,crime2002,crime2003,crime2004,
               crime2005,crime2006,crime2007,crime2008,crime2009,
               crime2010,crime2011,crime2012,crime2013,crime2014,
               crime2015,crime2016 ,crime2017,crime2018,crime2019)
str(crime)
table(crime$year)

# select total and state total
crimeDat <- crime[which(crime$area =="State Total"|crime$area =="Total"),]
crimeDat <- subset(crimeDat, select = -c(X,area,actual_rate))

# change population
crimeDat$population[which(crimeDat$population==100000)] <- NA
crimeDat <- crimeDat %>% fill(population)

# get the crime number per 100,000 inhabitants
crimeDat <- crimeDat[which(crimeDat$report_type =="rate"),]
table(crimeDat$year)


# give columns new name
names(crimeDat)[6] <- "crime_rate"

# remove white space of state name and change name into lower characters
crimeDat$state <- str_trim(crimeDat$state)
crimeDat$state <- tolower(crimeDat$state)

table(crimeDat$state)

# select two types of crime: violent crime and property crime
crimeDat_2 <- filter(crimeDat,crime_type %in% c("violent_crime","property_crime")) 

# make crime types as column
crimeDat_2 <- crimeDat_2 %>% spread(crime_type,crime_rate)

# select year,state,violent crime and property crime 
crimeDat_2 <- subset(crimeDat_2, select = -report_type)

# str
str(crimeDat_2)

# crimeDat_2$year <- as.factor(crimeDat_2$year)
# str(crimeDat_2)

# save the data
saveRDS(crimeDat_2,"crime_cluster.rds")

