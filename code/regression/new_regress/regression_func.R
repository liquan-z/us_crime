# time series data regression
library(car)
# plm inference with newey-west variance estimation

# for every states--------------------------------------------------------------
plm_inference_state_level <- function(x){
  state_frame = data.frame()
  beta_frame = data.frame() # save coefficients in regress
  state_list = sort(unique(x$region))
  for (State in state_list){
    State_data = subset(x,x$region==State)
    State_comp = State_data[rowSums(is.na(State_data)) == 0,]
    if (nrow(State_comp)==0){
      next
    }
    # response: crime
    # Scaling on other variables
    State_comp$unemp_rate = (State_comp$unemp_rate-mean(State_comp$unemp_rate))/sd(State_comp$unemp_rate)
    State_comp$pop_density = (State_comp$pop_density-mean(State_comp$pop_density))/sd(State_comp$pop_density)
    State_comp$pov_rate = (State_comp$pov_rate-mean(State_comp$pov_rate))/sd(State_comp$pov_rate)
    State_comp$pol_officer_rate = (State_comp$pol_officer_rate-mean(State_comp$pol_officer_rate))/sd(State_comp$pol_officer_rate)
    State_comp$med_age = (State_comp$med_age-mean(State_comp$med_age))/sd(State_comp$med_age)
    X = cbind(rep(1,nrow(State_comp)),State_comp[,4:8]) # design matrix
    # Model fit
    model <- lm(log(crime_rate/100000)~pop_density+pol_officer_rate+pov_rate+unemp_rate+med_age,data=State_comp)
    beta = model$coefficient
    beta_frame = rbind(beta_frame,beta)
    names(beta_frame) = c('intercept','pop_density','pol_officer_rate','pov_rate','unemp_rate','med_age')
    # Verify the residual 
    State_residual = model$residuals
    pacf_error <- pacf(State_residual,plot = F) # check auto-correlation
    if (max(abs(pacf_error$acf))>0.4){
      Lag <- max(which(abs(pacf_error$acf) > 0.1))
      sum_newey_b <- 0
      for (l in 1:Lag){
        for (t in ((l+1):length(State_residual))){
          sum_newey_b = sum_newey_b + (1-l/(1+Lag))*State_residual[t]*State_residual[t-l]*(
            t(as.matrix(X[t,])) %*% as.matrix(X[t-l,]) + 
              t(as.matrix(X[t-l,])) %*% as.matrix(X[t,]))
        }
      }
      sum_newey_a <- 0
      for (t in 1:length(State_residual)){
        sum_newey_a = sum_newey_a + State_residual[t]^2* t(as.matrix(X[t,])) %*% as.matrix(X[t,])
      }
      S <- (sum_newey_a + sum_newey_b)
      var_beta = solve(t(as.matrix(X)) %*% as.matrix(X)) %*% S %*%
        solve(t(as.matrix(X)) %*% as.matrix(X))
    } else {
      var_beta = solve(t(as.matrix(X)) %*% as.matrix(X)) * var(State_residual)
    } 
    # Verify the significant:
    summary_tbl = data.frame()
    for (i in 2:ncol(beta_frame)){
      df_add = data.frame(beta[i],sqrt(var_beta[i,i]),beta[i]/sqrt(var_beta[i,i]),
                          (1-pt(abs(beta[i]/sqrt(var_beta[i,i])),df=length(State_residual)-1))*2,
                          (1-pt(abs(beta[i]/sqrt(var_beta[i,i])),df=length(State_residual)-1))*2<=0.05) #/ncol(beta_frame))
      names(df_add) = c('point estimation','standard error','t-statistics','p-value','significant')
      summary_tbl = rbind(summary_tbl,df_add)
    }
    print(State)
    print(summary_tbl)
    state_add = data.frame(State,beta[2]*((1-pt(abs(beta[2]/sqrt(var_beta[2,2])),df=length(State_residual)-1))*2<=0.05),
                           beta[3]*((1-pt(abs(beta[3]/sqrt(var_beta[3,3])),df=length(State_residual)-1))*2<=0.05),
                           beta[4]*((1-pt(abs(beta[4]/sqrt(var_beta[4,4])),df=length(State_residual)-1))*2<=0.05),
                           beta[5]*((1-pt(abs(beta[5]/sqrt(var_beta[5,5])),df=length(State_residual)-1))*2<=0.05),
                           beta[6]*((1-pt(abs(beta[6]/sqrt(var_beta[6,6])),df=length(State_residual)-1))*2<=0.05))
    names(state_add) = c('state','pop_density','pol_officer_rate','pov_rate','unemp_rate','med_age')
    state_frame = rbind(state_frame,state_add)
  }
  return (state_frame)
}

# for Nationwide ---------------------------------------------------------------
# panel data regression
plm_inference_overall <- function(x){
  beta_frame = data.frame()
  time_list = sort(unique(x$year))
  for (Time in time_list){
    Time_data = subset(x,x$year==Time)
    Time_comp = Time_data[rowSums(is.na(Time_data)) == 0,]
    #Time_comp = subset(Time_comp,Time_comp$region!='north dakota')
    if (nrow(Time_comp)==0){
      next
    }
    # response: crime
    # Scaling on other variables
    Time_comp$unemp_rate = (Time_comp$unemp_rate-mean(Time_comp$unemp_rate))/sd(Time_comp$unemp_rate)
    Time_comp$pop_density = (Time_comp$pop_density-mean(Time_comp$pop_density))/sd(Time_comp$pop_density)
    Time_comp$pov_rate = (Time_comp$pov_rate-mean(Time_comp$pov_rate))/sd(Time_comp$pov_rate)
    Time_comp$pol_officer_rate = (Time_comp$pol_officer_rate-mean(Time_comp$pol_officer_rate))/sd(Time_comp$pol_officer_rate)
    # Model fit
    model <- lm(log(crime_rate/100000)~pop_density+pol_officer_rate+pov_rate+unemp_rate+med_age,data=Time_comp)
    beta = model$coefficient
    beta_frame = rbind(beta_frame,beta)
  }
  names(beta_frame) = c('intercept','pop_density','pol_officer_rate','pov_rate','unemp_rate','med_age')
  # Newest Cov for serial correlation
  se_list = c()
  for (i in 1:ncol(beta_frame)){
    beta_k = beta_frame[,i]
    model <- lm(beta_k~1)
    error_t <- model$residuals
    pacf_error <- pacf(error_t,plot = F)
    if (max(abs(pacf_error$acf))>0.4){
      Lag <- max(which(abs(pacf_error$acf) > 0.1))
      sum_newey <- 0
      for (l in 1:Lag){
        for (t in ((l+1):length(error_t))){
          sum_newey = sum_newey + (1-l/(1+Lag))*error_t[t]*error_t[t-l]*2
        }
      }
      S <- (sum(error_t^2) + sum_newey)/(length(error_t)^2)
      se_list = c(se_list,sqrt(S))
    } else {
      se_list = c(se_list,sqrt(sum(error_t^2)/ (length(error_t))^2 ))
    }
  }
  # Significant test and summary:
  summary_tbl = data.frame()
  for (i in 1:ncol(beta_frame)){
    df_add = data.frame(colMeans(beta_frame)[i],se_list[i],colMeans(beta_frame)[i]/se_list[i],
                        (1-pt(abs(colMeans(beta_frame)[i]/se_list[i]),df=nrow(beta_frame)-1))*2,
                        (1-pt(abs(colMeans(beta_frame)[i]/se_list[i]),df=nrow(beta_frame)-1))*2<=0.05) #/ncol(beta_frame))
    names(df_add) = c('point estimation','standard error','t-statistics','p-value','significant')
    summary_tbl = rbind(summary_tbl,df_add)
  }
  return(summary_tbl)
}

# for violent crime-------------------------------------------------------------
data_vio <- read.csv('combine_vio.csv',header = T)
data_vio <- subset(data_vio,select=-X)

# see the structure of data set
str(data_vio)

# show the violent crime and other variables
pairs(data_vio[,3:8],pch = 19, lower.panel = NULL)

# no unemployment rate
# pairs(data_vio[,3:6],pch = 19, lower.panel = NULL)


# use the function to see the result--------------------------------------------
state_frame = plm_inference_state_level(data_vio)
plm_inference_overall(data_vio)

# cluster based on coefficients:
rownames(state_frame) <- state_frame$state
library(factoextra)
distance <- get_dist(state_frame[2:6])
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
library(NbClust)
nc <- NbClust(state_frame[2:5],min.nc=2,max.nc = 10,method='kmeans')
barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria')
centers_num <- length(unique(nc$Best.partition))
cluster2 <- kmeans(state_frame[2:5],centers = centers_num,nstart = 10)
state_frame$predict <- factor(cluster2$cluster)
names(state_frame)[1] <- 'region'
library(usmap)
library(Hmisc)
library(ggplot2)
cluster_plot_52map <- function(data_in){
  # plot the cluster result in map
  
  # capitalize the first letter of regions
  data_in$region <- Hmisc::capitalize(as.character(data_in$region))
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
cluster_plot_52map(state_frame)



# We feel curious about how the causality direction is between police and crime
library(lmtest)
library(tseries)
direction = data.frame()
state_list = unique(data_vio$region)
for (State in state_list){
  State_data = subset(data_vio,data_vio$region==State)
  y_stationary = diff(State_data$crime_rate)
  x_stationary = diff(State_data$pol_officer_rate)
  if ((adf.test(x_stationary,'explosive')$p.value<=0.5)|(adf.test(y_stationary,'explosive')$p.value<=0.5)){
    direction_add = data.frame(State,0) # assumption failed
    names(direction_add) = c('state','direction')
    direction = rbind(direction,direction_add)
    next
  } else {
    x_pval = 1
    y_pval = 1
    for (lag in 1:7){
      x_pval = min(x_pval,grangertest(y_stationary~x_stationary,order=lag)$`Pr(>F)`[2])
      y_pval = min(y_pval,grangertest(x_stationary~y_stationary,order=lag)$`Pr(>F)`[2])
    }
    if ((x_pval<=0.05)&(y_pval<=0.05)){
      direction_add = data.frame(State,'affect each other') # they affect each other
      names(direction_add) = c('state','direction') 
      direction = rbind(direction,direction_add)
    } else if ((x_pval>0.05)&(y_pval<=0.05)){
      direction_add = data.frame(State,'crime affect police') # crime rate affect police officer rate
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    } else if ((x_pval<=0.05)&(y_pval>0.05)){
      direction_add = data.frame(State,'police affect crime') # police officer rate affect crime rate
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    } else {
      direction_add = data.frame(State,0)
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    }
  } 
}
# visualization:
library(usmap)
library(Hmisc)
direction$state = Hmisc::capitalize(as.character(direction$state))
direction$values = as.character(direction$direction)
direction = direction[,c('state','values')]
df <- map_with_data(direction,na=0)
df$values[df$values==0] = NA
p <- ggplot(data = df,mapping=aes(x=x,y=y,group=group,fill=values))
p + geom_polygon(color = "gray90", size = 0.1) + theme(legend.position="top")


# show region's violent crime rate by years
# coplot(vio_crime_rate ~ year|region, type="l", data=data_vio) 
# coplot(vio_crime_rate ~ year|region, type="b", data=data_vio) 

# library(car)
# scatterplot(vio_crime_rate~year|region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data_vio)


# for property crime-------------------------------------------------------------
data_prop <- read.csv('combine_prop.csv',header = T)
data_prop <- subset(data_prop,select=-X)
data_prop[30,3] <- 2540.9
data_prop[262,3] <- 2782.4

# see the structure of data set
str(data_prop)

# show the violent crime and other variables
pairs(data_prop[,3:8],pch = 19, lower.panel = NULL)

# no unemployment rate
# pairs(data_vio[,3:6],pch = 19, lower.panel = NULL)


# use the function to see the result--------------------------------------------
state_frame = plm_inference_state_level(data_prop)
plm_inference_overall(data_prop)

# cluster based on coefficients:
rownames(state_frame) <- state_frame$state
library(factoextra)
distance <- get_dist(state_frame[2:6])
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
library(NbClust)
nc <- NbClust(state_frame[2:5],min.nc=2,max.nc = 10,method='kmeans')
barplot(table(nc$Best.n[1,]),xlab='# of clusters',ylab='# of criteria')
centers_num <- length(unique(nc$Best.partition))
cluster2 <- kmeans(state_frame[2:5],centers = centers_num,nstart = 10)
state_frame$predict <- factor(cluster2$cluster)
names(state_frame)[1] <- 'region'
library(usmap)
library(Hmisc)
library(ggplot2)
cluster_plot_52map <- function(data_in){
  # plot the cluster result in map
  
  # capitalize the first letter of regions
  data_in$region <- Hmisc::capitalize(as.character(data_in$region))
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
cluster_plot_52map(state_frame)



# We feel curious about how the causality direction is between police and crime
library(lmtest)
library(tseries)
direction = data.frame()
state_list = unique(data_prop$region)
for (State in state_list){
  State_data = subset(data_prop,data_prop$region==State)
  y_stationary = diff(State_data$crime_rate)
  x_stationary = diff(State_data$pol_officer_rate)
  if ((adf.test(x_stationary,'explosive')$p.value<=0.5)|(adf.test(y_stationary,'explosive')$p.value<=0.5)){
    direction_add = data.frame(State,0) # assumption failed
    names(direction_add) = c('state','direction')
    direction = rbind(direction,direction_add)
    next
  } else {
    x_pval = 1
    y_pval = 1
    for (lag in 1:7){
      x_pval = min(x_pval,grangertest(y_stationary~x_stationary,order=lag)$`Pr(>F)`[2])
      y_pval = min(y_pval,grangertest(x_stationary~y_stationary,order=lag)$`Pr(>F)`[2])
    }
    if ((x_pval<=0.05)&(y_pval<=0.05)){
      direction_add = data.frame(State,'affect each other') # they affect each other
      names(direction_add) = c('state','direction') 
      direction = rbind(direction,direction_add)
    } else if ((x_pval>0.05)&(y_pval<=0.05)){
      direction_add = data.frame(State,'crime affect police') # crime rate affect police officer rate
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    } else if ((x_pval<=0.05)&(y_pval>0.05)){
      direction_add = data.frame(State,'police affect crime') # police officer rate affect crime rate
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    } else {
      direction_add = data.frame(State,0)
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    }
  } 
}
# visualization:
library(usmap)
library(Hmisc)
direction$state = Hmisc::capitalize(as.character(direction$state))
direction$values = as.character(direction$direction)
direction = direction[,c('state','values')]
df <- map_with_data(direction,na=0)
df$values[df$values==0] = NA
p <- ggplot(data = df,mapping=aes(x=x,y=y,group=group,fill=values))
p + geom_polygon(color = "gray90", size = 0.1) + theme(legend.position="top")





