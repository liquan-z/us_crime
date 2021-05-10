
func <- function(year_num)
{
  sex = read.csv(paste0(year_num,".csv"),skip = 4,header = F,
                 nrows = 51,col.names = c('state','male_rate'))
  return(sex)
}

df_list <- lapply(2010:2019, func)
sex_for_regress <- rbind(df_list[[1]],df_list[[2]],df_list[[3]],df_list[[4]]
                            ,df_list[[5]],df_list[[6]],df_list[[7]],df_list[[8]]
                            ,df_list[[9]],df_list[[10]])
year <- c(rep(2010,51),rep(2011,51),rep(2012,51),rep(2013,51)
          ,rep(2014,51),rep(2015,51),rep(2016,51),rep(2017,51)
          ,rep(2018,51),rep(2019,51))

df_for_regress_sex <- cbind(year,sex_for_regress)

write.csv(df_for_regress_sex,'sex.csv')

