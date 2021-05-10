
func <- function(year_num)
{
  med_age = read.csv(paste0(year_num,".csv"),skip = 2,header = F,
                 nrows = 51)
  return(med_age)
}

df_list <- lapply(1995:2018, func)
med_age_for_regress <- rbind(df_list[[1]],df_list[[2]],df_list[[3]],df_list[[4]]
                            ,df_list[[5]],df_list[[6]],df_list[[7]],df_list[[8]]
                            ,df_list[[9]],df_list[[10]],df_list[[11]],df_list[[12]]
                            ,df_list[[13]],df_list[[14]],df_list[[15]],df_list[[16]]
                            ,df_list[[17]],df_list[[18]],df_list[[19]],df_list[[20]]
                         ,df_list[[21]],df_list[[22]],df_list[[23]],df_list[[24]])
                         
year <- c(rep(1995,51),rep(1996,51),rep(1997,51),rep(1998,51),rep(1999,51),rep(2000,51),rep(2001,51),rep(2002,51),rep(2003,51),rep(2004,51),rep(2005,51),rep(2006,51),rep(2007,51),rep(2008,51),rep(2009,51),rep(2010,51),rep(2011,51),rep(2012,51),rep(2013,51)
          ,rep(2014,51),rep(2015,51),rep(2016,51),rep(2017,51)
          ,rep(2018,51))
str(med_age_for_regress)
df_for_regress_med_age <- cbind(year,med_age_for_regress)

names(df_for_regress_med_age) <- c('year','region','med_age')
df_for_regress_med_age$region <- gsub('[[:digit:]]+', '', df_for_regress_med_age$region)
df_for_regress_med_age$region <- gsub('[[:punct:]]+', '', df_for_regress_med_age$region)

library(tidyverse)
# remove white space of state name  and change name into lower characters
df_for_regress_med_age$region <- str_trim(df_for_regress_med_age$region)
df_for_regress_med_age$region <- tolower(df_for_regress_med_age$region)

df_for_regress_med_age_sort <- df_for_regress_med_age[with(df_for_regress_med_age, order(year,region)), ]
write.csv(df_for_regress_med_age,'med_age.csv')

