# Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
# 1999 Dataset Cleaning

library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

# Assign the 1999 crime dataset from github a name
crime1999 <- rio::import('https://github.com/RuofanChen/US-crime-data/raw/main/1999tbl05.xls',sheet = 1,col_names = T,range = "A4:N631")

# set line 476,column 2 as 0
crime1999[476,2] <- '0'

# remove rows include None
row_none <- subset(crime1999,Population=='None')
crime1999 <- crime1999[-as.numeric(rownames(row_none)),]

# delete two empty columns
crime1999 <- subset(crime1999, select = -c(`Modified Crime Index Total1`,...11))


# remove all NA rows
row.all.na <- apply(crime1999, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1999 <- crime1999[!row.all.na,]

# look the structure of the data frame
str(crime1999)

# set all the columns are numerical except Area
crime1999$Population <- gsub(",","",crime1999$Population)
crime1999$`Murder and non-negligent man-     slaughter` <- gsub(",","",crime1999$`Murder and non-negligent man-     slaughter`)
crime1999$`Forcible rape` <- gsub(",","",crime1999$`Forcible rape`)
crime1999$Robbery <- gsub(",","",crime1999$Robbery)
cols <- names(crime1999)[2:12] # or column index (change the index if needed)
crime1999[cols] <- lapply(crime1999[cols], as.numeric)


# create the state column
crime1999$State <- ifelse(is.na(crime1999$Population)==T & is.na(crime1999$`Violent Crime2`)==T,crime1999$Area,NA)  

# create area(new one) column
crime1999$area <- ifelse(crime1999$Population>1 |crime1999$Population==0,crime1999$Area,NA)

# create actual reporting column
crime1999$Report_Type <- ifelse(crime1999$Population>0 & crime1999$Population<=1,crime1999$Area,NA)

# delete the original column Area
crime1999 <- subset(crime1999, select = -Area )

# create a new column state_: move State to row+1 in state_
crime1999$state_ <- NA
for (i in 1:(length(crime1999$State)-1)){
  crime1999$state_[i+1]=crime1999$State[i]
}

# remove State column
crime1999 <- subset(crime1999, select = -State )

# remove all NA rows
row.all.na <- apply(crime1999, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1999 <- crime1999[!row.all.na,]


# Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime1999 <- crime1999 %>% fill(c(state_, area))
View(crime1999)

#####################################################

# Create dataset of Metropolitan 
crime1999_Metro <- filter(crime1999,area=="Metropolitan Statistical Area")

# Create dataset between Metropolitan and Nonmetropolitan
crime1999_Between <- filter(crime1999,area=="Cities outside metropolitan areas")

# Create dataset Rural
crime1999_Rural <- filter(crime1999,area=="Rural")

# Create dataset StateTotal or Total
crime1999_total <- crime1999[grepl('Total',crime1999$area),]



##################################
# Metropolitan
crime_Metro_main <- filter(crime1999_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime1999_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,14)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
# Between
crime_Between_main <- filter(crime1999_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime1999_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,14)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
# Rural
crime_Rural_main <- filter(crime1999_Rural,is.na(Report_Type) == FALSE) 
names(crime_Rural_main)[1] <- "Actual_Rate"
crime_Rural_population <- filter(crime1999_Rural,is.na(Report_Type) == TRUE) 
crime_Rural_population <- crime_Rural_population[,c(1,14)]
crime_Rural_df <- merge(crime_Rural_main,crime_Rural_population,by.x="state_",by.y="state_")


############### Total#############################
# set population of rate: num per 100,000 inhabitants
crime1999_total$Population[is.na(crime1999_total$Population)] <- 100000

# set the report type of total as 'rate' and 'total'
crime1999_total$Report_Type <- ifelse(crime1999_total$Population==100000,'rate','total')

# set the actual rate of total as 99
crime1999_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_Rural_df,crime1999_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(1999,dim(crime_dfbystate)[1])
crime_df1999 <- data.frame(Year,crime_dfbystate)

#Check the results
View(crime_df1999)

# check number of state
length(table(crime_df1999$state_))

# check number of rows, recheck in line 138
dim(crime_df1999)

# remove number in state name
crime_df1999$state_ <- gsub('[[:digit:]]+', '', crime_df1999$state_ )

# give new names for columns
names(crime_df1999) <- c('year','state','actual_rate','crime_index_t','violent_crime',
                        'property_crime','murder','rape','robbery','assault',
                        'burglary','larceny_theft','vehicle_theft','area','report_type',
                        'population'
                        )
# drop crime_index_t column
d_crime_df1999 <- crime_df1999[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df1999 <- gather(d_crime_df1999,crime_type,number,violent_crime,property_crime,
                      murder,rape,robbery,assault,
                        burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df1999$state))

# recheck number of rows for each crime type  
table(u_crime_df1999$crime_type)

# output the csv file
write.csv(u_crime_df1999,'../../data/1995-2019_reorganized_data/crime_1999.csv')

