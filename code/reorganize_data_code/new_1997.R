# Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
# 1997 Dataset Cleaning
# Note:Assume the TOTAL population of Pureto Rico in 1997 is 3,759,000(Based on World Bank)
library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

# Assign the 1997 crime dataset from github a name
crime1997 <- read_excel("../../data/1995-1998_raw_data/1997tbl05.xlsx", sheet = 1,col_names = T, range = "A1:M609")

names(crime1997) <- c('Area','Population','Crime_Index','Crime_modified','violent_crime',
                      'property_crime','murder','rape','robbery','assault',
                      'burglary','larceny_theft','vehicle_theft')

# set population line as 0 done manually for Rhode island 
#crime1997[476,2] <- 0

# delete two empty columns
crime1997 <- subset(crime1997, select = -c(Crime_Index,Crime_modified))

# remove rows include None
row_none <- which(crime1997$Population=='NONE')
crime1997 <- crime1997[-row_none,]

# remove all NA rows
row.all.na <- apply(crime1997, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1997 <- crime1997[!row.all.na,]

# look the structure of the data frame
str(crime1997)

# set all the columns are numerical except Area
cols <- names(crime1997)[2:11] # or column index (change the index if needed)
crime1997[cols] <- lapply(crime1997[cols], as.numeric)

str(crime1997)

# create the state column
#crime1997$State <- ifelse(is.na(crime1997$Population)==T & is.na(crime1997$violent_crime)==T,crime1997$Area,NA)  
crime1997$State <- ifelse(crime1997$Area %in% c("Metropolitan Statistical Area","Area actually reporting",
                                                "Estimated totals","Cities outside metropolitan areas",
                                                "Rural","State Total","Rate per 100,000 inhabitants","Total"),
                          NA,crime1997$Area)

# create area(new one) column
crime1997$area <- ifelse(crime1997$Population>1|crime1997$Population==0,crime1997$Area,NA)

# create actual reporting column
crime1997$Report_Type <- ifelse(crime1997$Population<=1&crime1997$Population>0,crime1997$Area,NA)

# delete the original column Area
crime1997 <- subset(crime1997, select = -Area )

# create a new column state_: move State to row+1 in state_
crime1997$state_ <- NA
for (i in 1:(length(crime1997$State)-1)){
  crime1997$state_[i+1]=crime1997$State[i]
}

# remove State column
crime1997 <- subset(crime1997, select = -State )

# remove all NA rows
row.all.na <- apply(crime1997, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1997 <- crime1997[!row.all.na,]


# Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime1997 <- crime1997 %>% fill(c(state_, area))
View(crime1997)

#####################################################

# Create dataset of Metropolitan 
crime1997_Metro <- filter(crime1997,area=="Metropolitan Statistical Area")

# Create dataset between Metropolitan and Nonmetropolitan
crime1997_Between <- filter(crime1997,area=="Cities outside metropolitan areas")

# Create dataset Rural
crime1997_Rural <- filter(crime1997,area=="Rural")

# Create dataset StateTotal or Total
crime1997_total <- crime1997[grepl('Total',crime1997$area),]



##################################
# Metropolitan
crime_Metro_main <- filter(crime1997_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime1997_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,13)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
# Between
crime_Between_main <- filter(crime1997_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime1997_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,13)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
# Rural
crime_Rural_main <- filter(crime1997_Rural,is.na(Report_Type) == FALSE) 
names(crime_Rural_main)[1] <- "Actual_Rate"
crime_Rural_population <- filter(crime1997_Rural,is.na(Report_Type) == TRUE) 
crime_Rural_population <- crime_Rural_population[,c(1,13)]
crime_Rural_df <- merge(crime_Rural_main,crime_Rural_population,by.x="state_",by.y="state_")


############### Total#############################
# set population of rate: num per 100,000 inhabitants
crime1997_total$Population[is.na(crime1997_total$Population)] <- 100000

# set the report type of total as 'rate' and 'total'
crime1997_total$Report_Type <- ifelse(crime1997_total$Population==100000,'rate','total')

# set the actual rate of total as 99
crime1997_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_Rural_df,crime1997_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(1997,dim(crime_dfbystate)[1])
crime_df1997 <- data.frame(Year,crime_dfbystate)

#Check the results
View(crime_df1997)

# check number of state
length(table(crime_df1997$state_))

# check number of rows, recheck in line 138
dim(crime_df1997)

# remove number in state name
crime_df1997$state_ <- gsub('[[:digit:]]+', '', crime_df1997$state_ )


names(crime_df1997)
# give new names for columns
names(crime_df1997) <- c('year','state','actual_rate','violent_crime',
                         'property_crime','murder','rape','robbery','assault',
                         'burglary','larceny_theft','vehicle_theft','area','report_type',
                         'population')

# drop crime_index_t column
d_crime_df1997 <- crime_df1997[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df1997 <- gather(d_crime_df1997,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,
                         burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df1997$state))

# recheck number of rows for each crime type  
table(u_crime_df1997$crime_type)

# output the csv file
write.csv(u_crime_df1997,'../../data/1995-2019_reorganized_data/crime_1997.csv')

