# read the file
pop_density <- read.csv('popu_dens.csv',header = T)
names(pop_density)[3] <- 'region'
pov <- read.csv('pov.csv',header = T)
unemp <- read.csv('unemp.csv',header = T)
pol_emp <- read.csv('pol_emp.csv',header = T)
med_age <- read.csv('med_age.csv',header = T)

# the state is ordered by alphabetic order
# check it
sum(pop_density$region==pov$region & pop_density$year==pov$year)
sum(pop_density$region==unemp$region & pop_density$year==unemp$year)
sum(pop_density$region==pol_emp$state_full & pop_density$year==pol_emp$data_year)
sum(pop_density$region==med_age$region & pop_density$year==med_age$year)

# combine each type of crime in one data set
combine_vio <- data.frame(pop_density$year,pop_density$region,
                          pop_density$violent_crime,
                          pop_density$density,
                          pol_emp$officer_rate_per_1000,
                          pov$pov_rate,
                          unemp$unemp_rate,
                          med_age$med_age)
names(combine_vio) <- c('year','region','crime_rate','pop_density'
                        ,'pol_officer_rate','pov_rate','unemp_rate'
                        ,'med_age')

combine_prop <- data.frame(pop_density$year,pop_density$region,
                          pop_density$property_crime,
                          pop_density$density,
                          pol_emp$officer_rate_per_1000,
                          pov$pov_rate,
                          unemp$unemp_rate,
                          med_age$med_age)
names(combine_prop) <- c('year','region','crime_rate','pop_density'
                        ,'pol_officer_rate','pov_rate','unemp_rate'
                        ,'med_age')

write.csv(combine_vio,'combine_vio.csv')
write.csv(combine_prop,'combine_prop.csv')

