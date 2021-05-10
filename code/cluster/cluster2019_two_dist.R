# clustering for year 2019(2018)

# read the data
crime_df2019 <- read.csv('../../data/1995-2019_reorganized_data/crime_2018.csv',header = T)

# get preparation of the data
# drop the index column (first column)
crime_df2019 <- subset(crime_df2019,select = -X)

# select the state total and total rows
t_crime_df2019 <- crime_df2019[grepl('Total',crime_df2019$area) & crime_df2019$report_type=='rate',
                               c('state','crime_type','number')]

# make crime types as column
library(tidyverse)
t_crime_df2019 <- t_crime_df2019 %>% spread(crime_type,number)

# Color correlation map:
library(corrplot)
corrplot(cor(subset(t_crime_df2019,select=-c(state,property_crime,violent_crime))),method='number')

# partition cluster 2 features
crime_df2019_2 <- t_crime_df2019[,c('state','property_crime','violent_crime')]

# decide the number of cluster to choose
wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type='b',xlab='# of clusters',ylab='Within SS')
}

# remove state name column for clustering
wssplot(scale(crime_df2019_2[-1]))  # choose cluster number as 2

# another method for choosing cluster number
library(NbClust)
set.seed(1234)
nc <- NbClust(crime_df2019_2[-1],min.nc=2,max.nc = 20,method='kmeans')
barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria') # choose cluster number as 2

# k-means cluster
cluster2 <- kmeans(crime_df2019_2[-1],centers=2)
crime_df2019_2$predict <- factor(cluster2$cluster)
crime_df2019_2$state <- tolower(crime_df2019_2$state)
names(crime_df2019_2)[1] <- 'region'

# use map library to get longitude and latitude of US
library(maps)
us_states <- map_data("state")
map_dat <- merge(us_states,crime_df2019_2,by='region')
library(ggplot2)
library(mapproj)
p <- ggplot(data = map_dat,
            mapping = aes(x = long, y = lat,
                          group = group, fill = predict))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

ggplot(data=crime_df2019_2,aes(x=property_crime,y=violent_crime,color=predict,shape=predict))+
  geom_point() + ggtitle('clustering result')


# partition cluster 7 features

crime_df2019_7 <- t_crime_df2019[,c("state","assault","burglary","larceny_theft","murder","rape","robbery","vehicle_theft")]
wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type='b',xlab='# of clusters',ylab='Within SS')
}
wssplot(scale(crime_df2019_7[-1]))
library(NbClust)
nc=NbClust(crime_df2019_7[-1],min.nc=2,max.nc = 20,method='kmeans')
barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria')

cluster7 = kmeans(crime_df2019_7[-1],centers=3)
crime_df2019_7$predict = cluster7$cluster
crime_df2019_7$state = tolower(crime_df2019_7$state)
names(crime_df2019_7)[1] = 'region'
us_states <- map_data("state")
map_dat <- merge(us_states,crime_df2019_7,by='region')
p <- ggplot(data = map_dat,
            mapping = aes(x = long, y = lat,
                          group = group, fill = predict))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)


# partition cluster 2 features with Mahalanobis distance
crime_df2019_2 <- t_crime_df2019[,c('state','property_crime','violent_crime')]
C <- chol( var(crime_df2019_2[-1]) )
y <- as.matrix(crime_df2019_2[-1]) %*% solve(C)
wssplot(y)
set.seed(1234)
nc <- NbClust(y,min.nc=2,max.nc = 20,method='kmeans')
barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria') # choose cluster number as 3
cluster2 <- kmeans(y,centers=3)
crime_df2019_2$predict <- factor(cluster2$cluster)
crime_df2019_2$state <- tolower(crime_df2019_2$state)
names(crime_df2019_2)[1] <- 'region'

# use map library to get longitude and latitude of US
library(maps)
us_states <- map_data("state")
map_dat <- merge(us_states,crime_df2019_2,by='region')
library(ggplot2)
library(mapproj)
p <- ggplot(data = map_dat,
            mapping = aes(x = long, y = lat,
                          group = group, fill = predict))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

ggplot(data=crime_df2019_2,aes(x=property_crime,y=violent_crime,color=predict,shape=predict))+
  geom_point() + ggtitle('clustering result')


# partition cluster 7 features with Mahalanobis distance
crime_df2019_7 <- t_crime_df2019[,c("state","assault","burglary","larceny_theft","murder","rape","robbery","vehicle_theft")]
C <- chol( var(crime_df2019_7[-1]) )
y <- as.matrix(crime_df2019_7[-1]) %*% solve(C)
wssplot(y)
set.seed(1234)
nc <- NbClust(y,min.nc=2,max.nc = 20,method='kmeans')
barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria') # choose cluster number as 2 or 6
cluster7 <- kmeans(y,centers=2)
crime_df2019_7$predict <- factor(cluster7$cluster)
crime_df2019_7$state <- tolower(crime_df2019_7$state)
names(crime_df2019_7)[1] <- 'region'

# use map library to get longitude and latitude of US
library(maps)
us_states <- map_data("state")
map_dat <- merge(us_states,crime_df2019_7,by='region')
library(ggplot2)
library(mapproj)
p <- ggplot(data = map_dat,
            mapping = aes(x = long, y = lat,
                          group = group, fill = predict))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

