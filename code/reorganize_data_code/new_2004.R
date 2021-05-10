#Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
#2004 Dataset Cleaning

library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

#Assign the 2004 crime dataset from github a name
crime2004 <- rio::import('https://github.com/RuofanChen/US-crime-data/raw/main/2004tbl05.xls',sheet = 1, col_names = T,range = "A4:k555")

# set Rhode pop as 0
crime2004[424,2] <- '0'

# remove rows include None
row_none <- subset(crime2004,Population=='None')
crime2004 <- crime2004[-as.numeric(rownames(row_none)),]

# look the structure of the data frame
str(crime2004)

# set population as numerical 
crime2004$Population <- gsub(",","",crime2004$Population)
cols <- names(crime2004)[2] # or column index (change the index if needed)
crime2004[cols] <- lapply(crime2004[cols], as.numeric)


# create the state column
crime2004$State <- ifelse(is.na(crime2004$Population)==T & is.na(crime2004$`Violent crime`)==T,crime2004$Area,NA)  

# create area(new one) column
crime2004$area <- ifelse(crime2004$Population>1|crime2004$Population==0,crime2004$Area,NA)

# create actual reporting column
crime2004$Report_Type <- ifelse(crime2004$Population>0 & crime2004$Population<=1,crime2004$Area,NA)

# delete the original column Area
crime2004 <- subset(crime2004, select = -Area )

# create a new column state_: move State to row+1 in state_
crime2004$state_ <- NA
for (i in 1:(length(crime2004$State)-1)){
  crime2004$state_[i+1]=crime2004$State[i]
}

# remove State column
crime2004 <- subset(crime2004, select = -State )

# remove all NA rows
row.all.na <- apply(crime2004, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime2004 <- crime2004[!row.all.na,]


#Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime2004 <- crime2004 %>% fill(c(state_, area))
View(crime2004)

#####################################################

#Create dataset of Metropolitan 
crime04_Metro <- filter(crime2004,area=="Metropolitan Statistical Area") 
#Create dataset between Metropolitan and Nonmetropolitan
crime04_Between <- filter(crime2004,area=="Cities outside metropolitan areas") 
#Create dataset Nonmetropolitan counties
crime04_NonMetro <- filter(crime2004,area=="Nonmetropolitan counties")
#Create dataset StateTotal or Total
crime04_total <- crime2004[grepl('Total',crime2004$area),]



##################################
#Metropolitan
crime_Metro_main <- filter(crime04_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime04_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,13)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
#Between
crime_Between_main <- filter(crime04_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime04_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,13)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
#Non-Metropolitan
crime_NonMetro_main <- filter(crime04_NonMetro,is.na(Report_Type) == FALSE) 
names(crime_NonMetro_main)[1] <- "Actual_Rate"
crime_NonMetro_population <- filter(crime04_NonMetro,is.na(Report_Type) == TRUE) 
crime_NonMetro_population <- crime_NonMetro_population[,c(1,13)]
crime_NonMetro_df <- merge(crime_NonMetro_main,crime_NonMetro_population,by.x="state_",by.y="state_")


############### Total#############################
# set population of rate: num per 100,000 inhabitants
crime04_total$Population[is.na(crime04_total$Population)] <- 100000
# set the report type of total as 'rate' and 'total'
crime04_total$Report_Type <- ifelse(crime04_total$Population==100000,'rate','total')
# set the actual rate of total as 99
crime04_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_NonMetro_df,crime04_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(2004,dim(crime_dfbystate)[1])
crime_df2004 <- data.frame(Year,crime_dfbystate)
#Check the results
View(crime_df2004)

# check number of state
length(table(crime_df2004$state_))

# check number of rows, recheck in line 138
dim(crime_df2004)

# remove number in state name
crime_df2004$state_ <- gsub('[[:digit:]]+', '', crime_df2004$state_ )

# give new names for columns
names(crime_df2004) <- c('year','state','actual_rate','violent_crime',
                         'murder','rape','robbery','assault','property_crime',
                         'burglary','larceny_theft','vehicle_theft','area','report_type',
                         'population'
)
# reorganized order of columns
d_crime_df2004 <- crime_df2004[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df2004 <- gather(d_crime_df2004,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,
                         burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df2004$state))

# recheck number of rows for each crime type  
table(u_crime_df2004$crime_type)

# output the csv file
write.csv(u_crime_df2004,'../../data/1995-2019_reorganized_data/crime_2004.csv')
