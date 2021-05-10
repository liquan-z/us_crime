#Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
#2000 Dataset Cleaning

library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

#Assign the 2000 crime dataset from github a name
crime2000 <- rio::import('https://github.com/RuofanChen/US-crime-data/raw/main/2000tbl05.xls',sheet = 1,col_names = T,range = "A4:N636")

# set line 482,column 2 as 0
crime2000[482,2] <- '0'

# remove rows include None
row_none <- subset(crime2000,Population=='None')
crime2000 <- crime2000[-as.numeric(rownames(row_none)),]

# delete two empty columns
crime2000 <- subset(crime2000, select = -c(`Modified Crime Index Total1`,...11))

# remove all NA rows
row.all.na <- apply(crime2000, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime2000 <- crime2000[!row.all.na,]

# look the structure of the data frame
str(crime2000)

# set all the columns are numerical except Area
crime2000$Population <- gsub(",","",crime2000$Population)
crime2000$Robbery <- gsub(",","",crime2000$Robbery)
crime2000$`Murder and non-negligent man-     slaughter` <- gsub(",","",crime2000$`Murder and non-negligent man-     slaughter`)

cols <- names(crime2000)[2:12] # or column index (change the index if needed)
crime2000[cols] <- lapply(crime2000[cols], as.numeric)


# create the state column
crime2000$State <- ifelse(is.na(crime2000$Population)==T & is.na(crime2000$`Violent Crime2`)==T,crime2000$Area,NA)  

# create area(new one) column
crime2000$area <- ifelse(crime2000$Population>1|crime2000$Population==0,crime2000$Area,NA)

# create actual reporting column
crime2000$Report_Type <- ifelse(crime2000$Population<=1 & crime2000$Population>0,crime2000$Area,NA)

# delete the original column Area
crime2000 <- subset(crime2000, select = -Area )

# create a new column state_: move State to row+1 in state_
crime2000$state_ <- NA
for (i in 1:(length(crime2000$State)-1)){
  crime2000$state_[i+1]=crime2000$State[i]
}

# remove State column
crime2000 <- subset(crime2000, select = -State )

# remove all NA rows
row.all.na <- apply(crime2000, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime2000 <- crime2000[!row.all.na,]


# Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime2000 <- crime2000 %>% fill(c(state_, area))
View(crime2000)

#####################################################

# Create dataset of Metropolitan 
crime2000_Metro <- filter(crime2000,area=="Metropolitan Statistical Area")

# Create dataset between Metropolitan and Nonmetropolitan
crime2000_Between <- filter(crime2000,area=="Cities outside metropolitan areas")

# Create dataset Rural
crime2000_Rural <- filter(crime2000,area=="Rural")

# Create dataset StateTotal or Total
crime2000_total <- crime2000[grepl('Total',crime2000$area),]



##################################
# Metropolitan
crime_Metro_main <- filter(crime2000_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime2000_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,14)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
# Between
crime_Between_main <- filter(crime2000_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime2000_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,14)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
# Rural
crime_Rural_main <- filter(crime2000_Rural,is.na(Report_Type) == FALSE) 
names(crime_Rural_main)[1] <- "Actual_Rate"
crime_Rural_population <- filter(crime2000_Rural,is.na(Report_Type) == TRUE) 
crime_Rural_population <- crime_Rural_population[,c(1,14)]
crime_Rural_df <- merge(crime_Rural_main,crime_Rural_population,by.x="state_",by.y="state_")


############### Total#############################
# set population of rate: num per 100,000 inhabitants
crime2000_total$Population[is.na(crime2000_total$Population)] <- 100000

# set the report type of total as 'rate' and 'total'
crime2000_total$Report_Type <- ifelse(crime2000_total$Population==100000,'rate','total')

# set the actual rate of total as 99
crime2000_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_Rural_df,crime2000_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(2000,dim(crime_dfbystate)[1])
crime_df2000 <- data.frame(Year,crime_dfbystate)

#Check the results
View(crime_df2000)

# check number of state
length(table(crime_df2000$state_))

# check number of rows, recheck in line 138
dim(crime_df2000)

# remove number in state name
crime_df2000$state_ <- gsub('[[:digit:]]+', '', crime_df2000$state_ )

# give new names for columns
names(crime_df2000) <- c('year','state','actual_rate','crime_index_t','violent_crime',
                         'property_crime','murder','rape','robbery','assault',
                         'burglary','larceny_theft','vehicle_theft','area','report_type',
                         'population'
)
# drop crime_index_t column
d_crime_df2000 <- crime_df2000[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df2000 <- gather(d_crime_df2000,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,
                         burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df2000$state))

# recheck number of rows for each crime type  
table(u_crime_df2000$crime_type)

# output the csv file
write.csv(u_crime_df2000,'../../data/1995-2019_reorganized_data/crime_2000.csv')
