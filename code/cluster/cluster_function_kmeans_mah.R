# this function is to do clustering using violent crime and property crime 

# it will use Mahalanobis Distance

# the first part is do clustering 
# while the second part is for plotting the cluster result

cluster_model <- function(year_num){
  
  # read the data 
  data <- readRDS("crime_cluster.rds")
  
  # get prepared of cluster for each year
  data_cluster <- data[data$year==year_num,]
  
  # remove year and population column
  year_col <- data_cluster$year
  population_col <- data_cluster$population
  data_cluster <- data_cluster[,c('state','property_crime','violent_crime')]
  
  # calculate the distance
  C <- chol(var(data_cluster[-1]))
  y <- as.matrix(data_cluster[-1]) %*% solve(C)
  
  # decide the number of clusters to choose
  wssplot <- function(data,nc=15,seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(data,centers=i)$withinss)
    }
    plot(1:nc,wss,type='b',xlab='# of clusters',ylab='Within SS')
  }

  # plot the number of cluster
  wssplot(y)
  
  # use another method to decide number of clusters
  library(NbClust)
  set.seed(1234)
  nc <- NbClust(y,min.nc=2,max.nc = 10,method='kmeans')
  barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria')
  
  # get the best number of clusters
  centers_num <- length(unique(nc$Best.partition))
  
  # do clustering
  # cluster2 <- kmeans(y,centers = centers_num,nstart = 10)
  cluster2 <- kmeans(y,centers = 3,nstart = 10)
  data_cluster$predict <- factor(cluster2$cluster)
  
  # rename state as region
  names(data_cluster)[1] <- 'region'
  
  data_cluster <- data.frame(year_col,population_col,data_cluster)
  return(data_cluster)
  
}



library(maps)
library(mapproj)

# cluster_plot_map <- function(data_in){
#   # plot the cluster result in map
#   
#   # use map library to get longitude and latitude of US
#   
#   us_states <- map_data("state")
#   map_dat <- merge(us_states,data_in,by='region')
#   
#   p <- ggplot(data = map_dat,
#               mapping = aes(x = long, y = lat,
#                             group = group, fill = predict))
#   
#   p + geom_polygon(color = "gray90", size = 0.1) +
#     coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#     guides(fill = FALSE)
# }

library(usmap)
library(Hmisc)
library(ggplot2)
cluster_plot_52map <- function(data_in){
  # plot the cluster result in map
  
  # capitalize the first letter of regions
  data_in$region <- Hmisc::capitalize(data_in$region)
  # use map library to get longitude and latitude of US
  
  us_states <- data.frame(region=data_in$region,values=data_in$predict)
  
  # rename region as states
  names(us_states)[1] <- 'state'
  names(data_in)[3] <- 'state'
  
  df <- map_with_data(us_states,na=0)
 
  # map_dat <- merge(df,data_in,by='state')
  p <- ggplot(data = df,mapping=aes(x=x,y=y,group=group,fill=values))
  p + geom_polygon(color = "gray90", size = 0.1)
}





# divide function two
cluster_data_52map <- function(data_in){
  # plot the cluster result in map
  
  # capitalize the first letter of regions
  data_in$region <- Hmisc::capitalize(data_in$region)
  # use map library to get longitude and latitude of US
  
  us_states <- data.frame(region=data_in$region,values=data_in$predict)
  
  # rename region as states
  names(us_states)[1] <- 'state'
  names(data_in)[3] <- 'state'
  
  df <- map_with_data(us_states,na=0)
  return(df)
}












cluster_plot_scatter <- function(data_in){
  # plot the scatter plot
  ggplot(data=data_cluster,aes(x=property_crime,y=violent_crime,color=predict,shape=predict))+
    geom_point() + ggtitle('clustering result')
}

