# do cluster
df_list <- lapply(2005:2019, cluster_model)

sample_for_cluster <- rbind(df_list[[1]],df_list[[2]],df_list[[3]],df_list[[4]]
                            ,df_list[[5]],df_list[[6]],df_list[[7]],df_list[[8]]
                            ,df_list[[9]],df_list[[10]],df_list[[11]],df_list[[12]]
                            ,df_list[[13]],df_list[[14]],df_list[[15]]                            )
  
names(sample_for_cluster) <- c('year','population','region','property_crime'
                               ,'violent_crime','cluster_number')
write.csv(sample_for_cluster,'sample_for_cluster.csv',row.names=FALSE)


# get map data

df_for_map_1 <- cluster_data_52map(df_list[[1]])
df_for_map_2 <- cluster_data_52map(df_list[[2]])
df_for_map_3 <- cluster_data_52map(df_list[[3]])
df_for_map_4 <- cluster_data_52map(df_list[[4]])
df_for_map_5 <- cluster_data_52map(df_list[[5]])
df_for_map_6 <- cluster_data_52map(df_list[[6]])
df_for_map_7 <- cluster_data_52map(df_list[[7]])
df_for_map_8 <- cluster_data_52map(df_list[[8]])
df_for_map_9 <- cluster_data_52map(df_list[[9]])
df_for_map_10 <- cluster_data_52map(df_list[[10]])
df_for_map_11 <- cluster_data_52map(df_list[[11]])
df_for_map_12 <- cluster_data_52map(df_list[[12]])
df_for_map_13 <- cluster_data_52map(df_list[[13]])
df_for_map_14 <- cluster_data_52map(df_list[[14]])
df_for_map_15 <- cluster_data_52map(df_list[[15]])

df_for_map_data <- rbind(df_for_map_1,df_for_map_2,df_for_map_3,
                         df_for_map_4,df_for_map_5,df_for_map_6,
                         df_for_map_7,df_for_map_8,df_for_map_9,
                         df_for_map_10,df_for_map_11,df_for_map_12,
                         df_for_map_13,df_for_map_14,df_for_map_15)

# add year column
year <- c(rep(2005,nrow(cluster_data_52map(df_list[[1]]))),
  rep(2006,nrow(cluster_data_52map(df_list[[2]]))),
  rep(2007,nrow(cluster_data_52map(df_list[[3]]))),
  rep(2008,nrow(cluster_data_52map(df_list[[4]]))),
  rep(2009,nrow(cluster_data_52map(df_list[[5]]))),
  rep(2010,nrow(cluster_data_52map(df_list[[6]]))),
  rep(2011,nrow(cluster_data_52map(df_list[[7]]))),
  rep(2012,nrow(cluster_data_52map(df_list[[8]]))),
  rep(2013,nrow(cluster_data_52map(df_list[[9]]))),
  rep(2014,nrow(cluster_data_52map(df_list[[10]]))),
  rep(2015,nrow(cluster_data_52map(df_list[[11]]))),
  rep(2016,nrow(cluster_data_52map(df_list[[12]]))),
  rep(2017,nrow(cluster_data_52map(df_list[[13]]))),
  rep(2018,nrow(cluster_data_52map(df_list[[14]]))),
  rep(2019,nrow(cluster_data_52map(df_list[[15]]))))

df_for_map_data <- cbind(year,df_for_map_data)

write.csv(df_for_map_data,'df_for_map_data.csv',row.names=FALSE)






# map from 2005 to 2019
cluster_plot_52map(df_list[[1]])
cluster_plot_52map(df_list[[2]])
cluster_plot_52map(df_list[[3]])
cluster_plot_52map(df_list[[4]])
cluster_plot_52map(df_list[[5]])
cluster_plot_52map(df_list[[6]])
cluster_plot_52map(df_list[[7]])
cluster_plot_52map(df_list[[8]])
cluster_plot_52map(df_list[[9]])
cluster_plot_52map(df_list[[10]])
cluster_plot_52map(df_list[[11]])
cluster_plot_52map(df_list[[12]])
cluster_plot_52map(df_list[[13]])
cluster_plot_52map(df_list[[14]])
cluster_plot_52map(df_list[[15]])

