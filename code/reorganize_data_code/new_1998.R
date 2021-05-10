# Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
# 1998 Dataset Cleaning
# Note:Assume the TOTAL population of Pureto Rico in 1998 is 3,781,000(Based on World Bank)
library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

# Assign the 1998 crime dataset from github a name
crime1998 <- read_excel("../../data/1995-1998_raw_data/1998tbl05.xlsx", sheet = 1,col_names = T, range = "A1:M569")

names(crime1998) <- c('Area','Population','Crime_Index','Crime_modified','violent_crime',
                      'property_crime','murder','rape','robbery','assault',
                      'burglary','larceny_theft','vehicle_theft')

# set population line as 0 done manually for Rhode island 
#crime1998[476,2] <- 0

# delete two empty columns
crime1998 <- subset(crime1998, select = -c(Crime_Index,Crime_modified))

# remove rows include None
row_none <- which(crime1998$Population=='None')
crime1998 <- crime1998[-row_none,]

# remove all NA rows
row.all.na <- apply(crime1998, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1998 <- crime1998[!row.all.na,]

# look the structure of the data frame
str(crime1998)

# set all the columns are numerical except Area
cols <- names(crime1998)[2:11] # or column index (change the index if needed)
crime1998[cols] <- lapply(crime1998[cols], as.numeric)

str(crime1998)

# create the state column
#crime1998$State <- ifelse(is.na(crime1998$Population)==T & is.na(crime1998$violent_crime)==T,crime1998$Area,NA)  
crime1998$State <- ifelse(crime1998$Area %in% c("Metropolitan Statistical Area","Area actually reporting",
                                                "Estimated totals","Cities outside metropolitan areas",
                                                "Rural","State Total","Rate per 100,000 inhabitants","Total"),
                          NA,crime1998$Area)

# create area(new one) column
crime1998$area <- ifelse(crime1998$Population>1|crime1998$Population==0,crime1998$Area,NA)

# create actual reporting column
crime1998$Report_Type <- ifelse(crime1998$Population<=1&crime1998$Population>0,crime1998$Area,NA)

# delete the original column Area
crime1998 <- subset(crime1998, select = -Area )

# create a new column state_: move State to row+1 in state_
crime1998$state_ <- NA
for (i in 1:(length(crime1998$State)-1)){
  crime1998$state_[i+1]=crime1998$State[i]
}

# remove State column
crime1998 <- subset(crime1998, select = -State )

# remove all NA rows
row.all.na <- apply(crime1998, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime1998 <- crime1998[!row.all.na,]


# Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime1998 <- crime1998 %>% fill(c(state_, area))
View(crime1998)

#####################################################

# Create dataset of Metropolitan 
crime1998_Metro <- filter(crime1998,area=="Metropolitan Statistical Area")

# Create dataset between Metropolitan and Nonmetropolitan
crime1998_Between <- filter(crime1998,area=="Cities outside metropolitan areas")

# Create dataset Rural
crime1998_Rural <- filter(crime1998,area=="Rural")

# Create dataset StateTotal or Total
crime1998_total <- crime1998[grepl('Total',crime1998$area),]



##################################
# Metropolitan
crime_Metro_main <- filter(crime1998_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime1998_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,13)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
# Between
crime_Between_main <- filter(crime1998_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime1998_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,13)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
# Rural
crime_Rural_main <- filter(crime1998_Rural,is.na(Report_Type) == FALSE) 
names(crime_Rural_main)[1] <- "Actual_Rate"
crime_Rural_population <- filter(crime1998_Rural,is.na(Report_Type) == TRUE) 
crime_Rural_population <- crime_Rural_population[,c(1,13)]
crime_Rural_df <- merge(crime_Rural_main,crime_Rural_population,by.x="state_",by.y="state_")


############### Total#############################
# set population of rate: num per 100,000 inhabitants
crime1998_total$Population[is.na(crime1998_total$Population)] <- 100000

# set the report type of total as 'rate' and 'total'
crime1998_total$Report_Type <- ifelse(crime1998_total$Population==100000,'rate','total')

# set the actual rate of total as 99
crime1998_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_Rural_df,crime1998_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(1998,dim(crime_dfbystate)[1])
crime_df1998 <- data.frame(Year,crime_dfbystate)

#Check the results
View(crime_df1998)

# check number of state
length(table(crime_df1998$state_))

# check number of rows, recheck in line 138
dim(crime_df1998)

# remove number in state name
crime_df1998$state_ <- gsub('[[:digit:]]+', '', crime_df1998$state_ )


names(crime_df1998)
# give new names for columns
names(crime_df1998) <- c('year','state','actual_rate','violent_crime',
                         'property_crime','murder','rape','robbery','assault',
                         'burglary','larceny_theft','vehicle_theft','area','report_type',
                         'population')

# drop crime_index_t column
d_crime_df1998 <- crime_df1998[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df1998 <- gather(d_crime_df1998,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,
                         burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df1998$state))

# recheck number of rows for each crime type  
table(u_crime_df1998$crime_type)

# output the csv file
write.csv(u_crime_df1998,'../../data/1995-2019_reorganized_data/crime_1998.csv')



