#Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
#2015 Dataset Cleaning


library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

#Assign the 2015 crime dataset from github a name
crime2015 <- rio::import('https://github.com/RuofanChen/US-crime-data/raw/main/2015-table_5_crime_in_the_united_states_by_state_2015.xls',sheet = 1,col_names = T,range = "A4:N506")
crime <- crime2015

#Open the tableset with the variable names
names(crime)[3] <- 'Report_Type'

# set Delaware, Rhode pop as 0
crime[73,4] <- '0'
crime[388,4] <- '0'

# remove rows include None
row_none <- subset(crime,Population=='None')
crime <- crime[-as.numeric(rownames(row_none)),]

# look the structure of the data frame
str(crime)

# set all the columns are numerical except Area
crime$Population <- gsub(",","",crime$Population)
cols <- names(crime)[4] # or column index (change the index if needed)
crime[cols] <- lapply(crime[cols], as.numeric)


#Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime <- crime %>% fill(c(State, Area))
View(crime)

#####################################################

#Creat dataset of Metropolitan 
crime_Metro <- filter(crime,Area=="Metropolitan Statistical Area") 
#Creat dataset between Metropolitan and Nonmetropolitan
crime_Between <- filter(crime,Area=="Cities outside metropolitan areas") 
#Create dataset Nonmetropolitan counties
crime_NonMetro <- filter(crime,Area=="Nonmetropolitan counties")
#Create dataset StateTotal
crime_total <- crime[grepl('Total',crime$Area),]
#write.csv(crime_Metro_df,"crime_metro_2015.csv")


##################################
#Metropolitan
crime_Metro_main <- filter(crime_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[4] <- "Actual_Rate"
crime_Metro_population <- filter(crime_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,4)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="State",by.y="State")
#Between
crime_Between_main <- filter(crime_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[4] <- "Actual_Rate"
crime_Between_population <- filter(crime_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,4)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="State",by.y="State")
#Non-Metropolitan
crime_NonMetro_main <- filter(crime_NonMetro,is.na(Report_Type) == FALSE) 
names(crime_NonMetro_main)[4] <- "Actual_Rate"
crime_NonMetro_population <- filter(crime_NonMetro,is.na(Report_Type) == TRUE) 
crime_NonMetro_population <- crime_NonMetro_population[,c(1,4)]
crime_NonMetro_df <- merge(crime_NonMetro_main,crime_NonMetro_population,by.x="State",by.y="State")
# State Total
crime_total_df <- crime_total
# set population of rate: num per 100,000 inhabitants #
crime_total_df$Population[is.na(crime_total_df$Population)] <- 100000
# set the report type of total as 'rate' and 'total' #
crime_total_df$Report_Type <- ifelse(crime_total_df$Population==100000,'rate','total')
# set the actual rate of total as 99 #
crime_total_df$Actual_Rate <- 99

###########################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_NonMetro_df,crime_total_df)
crime_dfbystate <- crime_df[order(crime_df$State),]
Year<-rep(2015,dim(crime_dfbystate)[1])
crime_df2015 <- data.frame(Year,crime_dfbystate)



#Check the results
View(crime_df2015)

# check number of state
length(table(crime_df2015$State))

# check number of rows, recheck in line 138
dim(crime_df2015)

# remove number in state name
crime_df2015$State <- gsub('[[:digit:]]+', '', crime_df2015$State )

# Check names
names(crime_df2015)

# give new names for columns
names(crime_df2015) <- c('year','state','area','report_type','actual_rate','violent_crime',
                         'murder','rape','rape_d','robbery','assault','property_crime',
                         'burglary','larceny_theft','vehicle_theft','population')

# Order the Column's name
crime_df2015 <- crime_df2015[,c('year','state','area','population','actual_rate','report_type',
                                'violent_crime','murder','rape','robbery','assault','property_crime',
                                'burglary','larceny_theft','vehicle_theft')]

# create a new column called crime_type indicate the type of the crime
u_crime_df2015 <- gather(crime_df2015,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df2015$state))

# recheck number of rows for each crime type  
table(u_crime_df2015$crime_type)


# output the csv file
write.csv(u_crime_df2015,'../../data/1995-2019_reorganized_data/crime_2015.csv')
