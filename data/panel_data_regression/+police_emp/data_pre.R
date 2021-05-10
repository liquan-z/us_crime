pol_emp <- read.csv("pe_1960_2019.csv",header = T)

# select column of data, keep year, state, officer per 1000
pol_emp <- subset(pol_emp,select = c('state_postal_abbr','data_year','officer_rate_per_1000'))

# some state do NOT have the pol_employment rate data, omit it
pol_emp <- na.omit(pol_emp)     
str(pol_emp)
# only keep year 2010-2018 data
pol_emp <- pol_emp[pol_emp$data_year %in% 1995:2018, ]  

# count number of states
length(table(pol_emp$state_postal_abbr))

# remove P.R, then we have 50 states and DC
pol_emp <- pol_emp[pol_emp$state_postal_abbr != 'PR',]


# transform Abbr to full name
state_full <- state.name[match(pol_emp$state_postal_abbr,state.abb)]
pol_emp <- cbind(pol_emp,state_full)
pol_emp$state_full <- ifelse(pol_emp$state_postal_abbr=='DC','District of Columbia',pol_emp$state_full)

sum(is.na(pol_emp))
pol_emp <- subset(pol_emp,select = -state_postal_abbr)

pol_emp_sort <- pol_emp[with(pol_emp, order(data_year,state_full)), ]
pol_emp_sort$state_full <- tolower(pol_emp_sort$state_full)
write.csv(pol_emp_sort,'pol_emp.csv')

