#Sara Rahim 
#Use gganimate to plot varying counts of a crime-type in the individual areas of a given state over a period of time


#Load 2010-2019 datasets into R
crime10 <- read.csv('../../data/1995-2019_reorganized_data/crime_2010.csv')
crime11 <- read.csv('../../data/1995-2019_reorganized_data/crime_2011.csv')
crime12 <- read.csv('../../data/1995-2019_reorganized_data/crime_2012.csv')
crime13 <- read.csv('../../data/1995-2019_reorganized_data/crime_2013.csv')
crime14 <- read.csv('../../data/1995-2019_reorganized_data/crime_2014.csv')
crime15 <- read.csv('../../data/1995-2019_reorganized_data/crime_2015.csv')
crime16 <- read.csv('../../data/1995-2019_reorganized_data/crime_2016.csv')
crime17 <- read.csv('../../data/1995-2019_reorganized_data/crime_2017.csv')
crime18 <- read.csv('../../data/1995-2019_reorganized_data/crime_2018.csv')
crime19 <- read.csv('../../data/1995-2019_reorganized_data/crime_2019.csv')

#Merge all datasets
data_all <- rbind(crime10, crime11, crime12, crime13, crime14, crime15, crime16, crime17, crime18, crime19)

#Remove certain states that are not compatible and do not have enough data:
data_n <- subset(data_all, state!= "DISTRICT OF COLUMBIA" & state!= "NEW JERSEY" & state!= "RHODE ISLAND" & state!= "HAWAII")

#Check dataframe
View(data_n)

#Make a new column for the area with better names
data_n$area_n <- 
  ifelse(data_n$area == 'Metropolitan Statistical Area',
         'Metropolitan Area',
         ifelse(data_n$area == 'Cities outside metropolitan areas',
                'Cities Outside Metro Area',
                ifelse(data_n$area == 'Nonmetropolitan counties',
                       'Non-metro Counties',
                       '')))

#Code for the interactive plot using gganimate

#Load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(gifski)
library(graphics)

#Can easily change crime-type and state 
#Top 5 states w/ the highest all-type crime rates: New Mexico, Louisiana, Alaska, Nevada, Arkansas
p <- ggplot(data_n %>% dplyr::filter(actual_rate == 1 & crime_type == 'vehicle_theft' & state == 'LOUISIANA'),
aes(x = area_n, y = number)) + geom_bar(stat = 'identity', width = 0.4, fill= "darkcyan") + 
theme_minimal() + theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0, size = 11)) +
scale_y_continuous(labels = scales::comma) + labs(title = "Crime by Area", subtitle = "State: LOUISIANA, Crime: VEHICLE THEFT") + labs(title = "", x = "Area Type", y = "Total Count") +
gganimate::transition_states(year, transition_length = 1, state_length = 1) + labs(title = "Year: {closest_state}")
gganimate::animate(p, duration = 10, fps = 20, width = 520, height = 520, renderer = gganimate::gifski_renderer())
gganimate::anim_save("output.gif")

