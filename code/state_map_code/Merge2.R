library(tidyverse)
library(dplyr)
# read the data
crimeDat <- readRDS("../../code/state_map_code/crime1995-2019.rds") 

# make crime types as column
crimeDat1 <- crimeDat %>% spread(Crime,rate)

#Delete district of columbia
crimeDat2 <- crimeDat1 %>%
  filter(state != 'district of columbia')
#Delete puerto rico
crimeDat3 <- crimeDat2 %>%
  filter(state != 'puerto rico')
State <- rep(state.name,25)
# Combine
crimeDat4 <- as.data.frame(cbind(crimeDat3,State))
#str
crimeDat4$Year <- as.factor(crimeDat4$Year)
str(crimeDat4)

# rename columns
names(crimeDat4) <- c("Year","state","population","report type","assault"      
                      ,"burglary","larceny theft","murder","rape","robbery"      
                      ,"vehicle theft","State")

# save the data
saveRDS(crimeDat4,"../../code/state_map_code/usacrimeDat.rds")
