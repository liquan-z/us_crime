#Contributors: Ruofan Chen, Sara Rahim, Liquan Zhong
#2002 Dataset Cleaning

library(rio)
library(readxl)
library(tidyverse)
library(dplyr)

#Assign the 2002 crime dataset from github a name
crime2002 <- rio::import('https://github.com/RuofanChen/US-crime-data/raw/main/2002tbl05.xls',sheet = 1,col_names = T,range = "A4:N654")

# set Rhode pop as 0
crime2002[502,2] <- '0'

# remove rows include None
row_none <- subset(crime2002,Population=='None')
crime2002 <- crime2002[-as.numeric(rownames(row_none)),]


# delete two empty columns
crime2002 <- subset(crime2002, select = -c(`Modified Crime Index1`,...11))


# remove all NA rows
row.all.na <- apply(crime2002, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime2002 <- crime2002[!row.all.na,]

# look the structure of the data frame
str(crime2002)

# set all the columns are numerical except Area

crime2002$Population <- gsub(",","",crime2002$Population)
crime2002$`Larceny-theft` <- gsub(",","",crime2002$`Larceny-theft`)

cols <- names(crime2002)[2:12] # or column index (change the index if needed)
crime2002[cols] <- lapply(crime2002[cols], as.numeric)


# create the state column
crime2002$State <- ifelse(is.na(crime2002$Population)==T & is.na(crime2002$`Violent crime2`)==T,crime2002$Area,NA)  

# create area(new one) column
crime2002$area <- ifelse(crime2002$Population>1|crime2002$Population==0,crime2002$Area,NA)

# create actual reporting column
crime2002$Report_Type <- ifelse(crime2002$Population>0 & crime2002$Population<=1,crime2002$Area,NA)

# delete the original column Area
crime2002 <- subset(crime2002, select = -Area )

# create a new column state_: move State to row+1 in state_
crime2002$state_ <- NA
for (i in 1:(length(crime2002$State)-1)){
  crime2002$state_[i+1]=crime2002$State[i]
}

# remove State column
crime2002 <- subset(crime2002, select = -State )

# remove all NA rows


row.all.na <- apply(crime2002, 1, function(x){all(is.na(x))})
sum(row.all.na)
crime2002 <- crime2002[!row.all.na,]


#Fill in the missing observations with the previous one for 'State' and 'Area' using fill function from tidyr

crime2002 <- crime2002 %>% fill(c(state_, area))
View(crime2002)

#####################################################

#Create dataset of Metropolitan 
crime02_Metro <- filter(crime2002,area=="Metropolitan Statistical Area") 
#Create dataset between Metropolitan and Nonmetropolitan
crime02_Between <- filter(crime2002,area=="Cities outside metropolitan areas") 
#Create dataset Rural
crime02_Rural <- filter(crime2002,area=="Rural")
#Create dataset StateTotal or Total
crime02_total <- crime2002[grepl('Total',crime2002$area),]



##################################
#Metropolitan
crime_Metro_main <- filter(crime02_Metro,is.na(Report_Type) == FALSE) 
names(crime_Metro_main)[1] <- "Actual_Rate"
crime_Metro_population <- filter(crime02_Metro,is.na(Report_Type) == TRUE) 
crime_Metro_population <- crime_Metro_population[,c(1,14)]
crime_Metro_df <- merge(crime_Metro_main,crime_Metro_population,by.x="state_",by.y="state_")
#Between
crime_Between_main <- filter(crime02_Between,is.na(Report_Type) == FALSE) 
names(crime_Between_main)[1] <- "Actual_Rate"
crime_Between_population <- filter(crime02_Between,is.na(Report_Type) == TRUE) 
crime_Between_population <- crime_Between_population[,c(1,14)]
crime_Between_df <- merge(crime_Between_main,crime_Between_population,by.x="state_",by.y="state_")
#Rural
crime_Rural_main <- filter(crime02_Rural,is.na(Report_Type) == FALSE) 
names(crime_Rural_main)[1] <- "Actual_Rate"
crime_Rural_population <- filter(crime02_Rural,is.na(Report_Type) == TRUE) 
crime_Rural_population <- crime_Rural_population[,c(1,14)]
crime_Rural_df <- merge(crime_Rural_main,crime_Rural_population,by.x="state_",by.y="state_")


############### Total#############################

crime02_total$Population[is.na(crime02_total$Population)] <- 100000

crime02_total$Report_Type <- ifelse(crime02_total$Population==100000,'rate','total')
crime02_total$Actual_Rate <- 99



################################Combining Datasets
crime_df <- rbind(crime_Metro_df,crime_Between_df,crime_Rural_df,crime02_total)
crime_dfbystate <- crime_df[order(crime_df$state_),]
Year<-rep(2002,dim(crime_dfbystate)[1])
crime_df2002 <- data.frame(Year,crime_dfbystate)
#Check the results
View(crime_df2002)

# check number of state
length(table(crime_df2002$state_))

# check number of rows, recheck in line 138
dim(crime_df2002)

# remove number in state name
crime_df2002$state_ <- gsub('[[:digit:]]+', '', crime_df2002$state_ )

# give new names for columns
names(crime_df2002) <- c('year','state','actual_rate','crime_index_t','violent_crime',
                         'property_crime','murder','rape','robbery','assault',
                         'burglary','larceny_theft','vehicle_theft','area','report_type',
                         'population'
)
# drop crime_index_t column
d_crime_df2002 <- crime_df2002[,c('year','state','area','population','actual_rate','report_type',
                                  'violent_crime','murder','rape','robbery','assault','property_crime',
                                  'burglary','larceny_theft','vehicle_theft')]


# create a new column called crime_type indicate the type of the crime
u_crime_df2002 <- gather(d_crime_df2002,crime_type,number,violent_crime,property_crime,
                         murder,rape,robbery,assault,
                         burglary,larceny_theft,vehicle_theft)

# check number of state
length(table(u_crime_df2002$state))

# recheck number of rows for each crime type  
table(u_crime_df2002$crime_type)

# output the csv file
write.csv(u_crime_df2002,'../../data/1995-2019_reorganized_data/crime_2002.csv')
