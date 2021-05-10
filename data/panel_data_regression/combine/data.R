# read the file
age <- read.csv('age.csv',header = T)
pop_density <- read.csv('density.csv',header = T)
emp <- read.csv('emp.csv',header = T)
race <- read.csv('race.csv',header = T)
sex <- read.csv('sex.csv',header = T)
pol_emp <- read.csv('pol_emp.csv',header = T)

library(tidyverse)
# remove density of PUERTO RICO
pop_density$state <- str_trim(pop_density$state)
pop_density <- pop_density[pop_density$state!='PUERTO RICO',]

# for combining purpose, transfer state name to lower
age$state <- tolower(age$state)
pop_density$state <- tolower(pop_density$state)
emp$state <- tolower(emp$state)
race$state <- tolower(race$state)
sex$state <- tolower(sex$state)
pol_emp$state_full <- tolower(pol_emp$state_full)

# the state is ordered by alphabetic order
# check it
sum(age$state==emp$state & age$year==emp$year)
sum(age$state==race$state & age$year==race$year)
sum(age$state==sex$state & age$year==sex$year)
sum(age$state==pol_emp$state_full & age$year==pol_emp$data_year)

# pop_density data set
pop_density <- subset(pop_density,select = c('Year','state','Crime',
           'rate','density'))
pop_density_vio <- pop_density[pop_density$Crime=='violent_crime',]
pop_density_prop <- pop_density[pop_density$Crime=='property_crime',]

sum(age$state==pop_density_vio$state & age$year==pop_density_vio$Year)
sum(age$state==pop_density_prop$state & age$year==pop_density_prop$Year)

# combine each type of crime in one data set
combine_vio <- data.frame(age$year,pop_density_vio$state,pop_density_vio$rate,
                          age$youth_rate,emp$emp_rate,pop_density_vio$density,
                          race$white_rate,sex$male_rate,pol_emp$officer_rate_per_1000)
names(combine_vio) <- c('year','region','vio_crime_rate','youth_rate',
                        'emp_rate','pop_density','white_rate',
                        'male_rate','pol_officer_rate')

combine_prop <- data.frame(age$year,pop_density_prop$state,pop_density_prop$rate,
                          age$youth_rate,emp$emp_rate,pop_density_prop$density,
                          race$white_rate,sex$male_rate,pol_emp$officer_rate_per_1000)
names(combine_prop) <- c('year','region','prop_crime_rate','youth_rate',
                        'emp_rate','pop_density','white_rate',
                        'male_rate','pol_officer_rate')
write.csv(combine_vio,'combine_vio.csv')
write.csv(combine_prop,'combine_prop.csv')

