# Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
# 1995 Dataset Cleaning
# Note:Assume the TOTAL population of Pureto Rico in 1995 is 3,683,000(Based on World Bank)
library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

# Assign the 1995 crime dataset from github a name
crime1995 <- read_excel("../../data/1995-1998_raw_data/1995tbl05.xlsx", sheet = 1,col_names = T, range = "A1:M651")

names(crime1995) <- c('Area','Population','Crime_Index','Crime_modified','violent_crime',
                      'property_crime','murder','rape','robbery','assault',
                      'burglary','larceny_theft','vehicle_theft')

# set population line as 0 done manually for Rhode island 
#crime1995[476,2] <- 0

# delete two empty columns
crime1995 <- subset(crime1995, select = -c(Crime_Index,Crime_modified))

# remove rows include None
row_none <- which(crime1995$Population=='NONE')
crime1995 <- crime1995[-row_none,]

# remove all NA rows
row.all.na <- apply(crime1995, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1995 <- crime1995[!row.all.na,]

# look the structure of the data frame
str(crime1995)

# set all the columns are numerical except Area
cols <- names(crime1995)[2:11] # or column index (change the index if needed)
crime1995[cols] <- lapply(crime1995[cols], as.numeric)

str(crime1995)

# create the state column
#crime1995$State <- ifelse(is.na(crime1995$Population)==T & is.na(crime1995$violent_crime)==T,crime1995$Area,NA)  
crime1995$State <- ifelse(crime1995$Area %in% c("Metropolitan Statistical Area","Area actually reporting",
                                                "Estimated totals","Cities outside metropolitan areas",
                                                "Rural","State Total","Rate per 100,000 inhabitants","Total"),
                          NA,crime1995$Area)

# create area(new one) column
crime1995$area <- ifelse(crime1995$Population>1|crime1995$Population==0,crime1995$Area,NA)

# create actual reporting column
crime1995$Report_Type <- ifelse(crime1995$Population<=1&crime1995$Population>0,crime1995$Area,NA)

# delete the original column Area
crime1995 <- subset(crime1995, select = -Area )

# create a new column state_: move State to row+1 in state_
crime1995$state_ <- NA
for (i in 1:(length(crime1995$State)-1)){
  crime1995$state_[i+1]=crime1995$State[i]
}

# remove State column
crime1995 <- subset(crime1995, select = -State )

# remove all NA rows
row.all.na <- apply(crime1995, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1995 <- crime1995[!row.all.na,]


# Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime1995 <- crime1995 %>% fill(c(state_, area))
View(crime1995)

#####################################################

# Create dataset of Metropolitan 
crime1995_Metro <- filter(crime1995,area=="Metropolitan Statistical Area")

# Create dataset between Metropolitan and Nonmetropolitan
crime1995_Between <- filter(crime1995,area=="Cities outside metropolitan areas")

# Create dataset Rural
crime1995_Rural <- filter(crime1995,area=="Rural")

# Create dataset StateTotal or Total
crime1995_total <- crime1995[grepl('Total',crime1995$area),]



##################################
# Metropolitan
crime_Metro_main <- filter(crime1995_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime1995_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,13)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
# Between
crime_Between_main <- filter(crime1995_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime1995_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,13)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
# Rural
crime_Rural_main <- filter(crime1995_Rural,is.na(Report_Type) == FALSE) 
names(crime_Rural_main)[1] <- "Actual_Rate"
crime_Rural_population <- filter(crime1995_Rural,is.na(Report_Type) == TRUE) 
crime_Rural_population <- crime_Rural_population[,c(1,13)]
crime_Rural_df <- merge(crime_Rural_main,crime_Rural_population,by.x="state_",by.y="state_")


############### Total#############################
# set population of rate: num per 100,000 inhabitants
crime1995_total$Population[is.na(crime1995_total$Population)] <- 100000

# set the report type of total as 'rate' and 'total'
crime1995_total$Report_Type <- ifelse(crime1995_total$Population==100000,'rate','total')

# set the actual rate of total as 99
crime1995_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_Rural_df,crime1995_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(1995,dim(crime_dfbystate)[1])
crime_df1995 <- data.frame(Year,crime_dfbystate)

#Check the results
View(crime_df1995)

# check number of state
length(table(crime_df1995$state_))

# check number of rows, recheck in line 138
dim(crime_df1995)

# remove number in state name
crime_df1995$state_ <- gsub('[[:digit:]]+', '', crime_df1995$state_ )


names(crime_df1995)
# give new names for columns
names(crime_df1995) <- c('year','state','actual_rate','violent_crime',
                         'property_crime','murder','rape','robbery','assault',
                         'burglary','larceny_theft','vehicle_theft','area','report_type',
                         'population')

# drop crime_index_t column
d_crime_df1995 <- crime_df1995[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df1995 <- gather(d_crime_df1995,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,
                         burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df1995$state))

# recheck number of rows for each crime type  
table(u_crime_df1995$crime_type)

# output the csv file
write.csv(u_crime_df1995,'../../data/1995-2019_reorganized_data/crime_1995.csv')
