# time series data regression
library(car)
# plm inference with newey-west covariance estimation

# for every states--------------------------------------------------------------
plm_inference_state_level <- function(x){
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
    State_comp$youth_rate = (State_comp$youth_rate-mean(State_comp$youth_rate))/sd(State_comp$youth_rate)
    State_comp$emp_rate = (State_comp$emp_rate-mean(State_comp$emp_rate))/sd(State_comp$emp_rate)
    State_comp$pop_density = (State_comp$pop_density-mean(State_comp$pop_density))/sd(State_comp$pop_density)
    State_comp$white_rate = (State_comp$white_rate-mean(State_comp$white_rate))/sd(State_comp$white_rate)
    State_comp$male_rate = (State_comp$male_rate-mean(State_comp$male_rate))/sd(State_comp$male_rate)
    State_comp$pol_officer_rate = (State_comp$pol_officer_rate-mean(State_comp$pol_officer_rate))/sd(State_comp$pol_officer_rate)
    X = cbind(rep(1,nrow(State_comp)),State_comp[,4:9]) # design matrix
    # Model fit
    model <- lm(log(crime_rate/100000)~youth_rate+emp_rate+pop_density+white_rate+male_rate+pol_officer_rate,data=State_comp)
    beta = model$coefficient
    beta_frame = rbind(beta_frame,beta)
    names(beta_frame) = c('intercept','youth_rate','emp_rate','pop_density','white_rate','male_rate','pol_officer_rate')
    # Verify the residual 
    State_residual = model$residuals
    pacf_error <- pacf(State_residual,plot = F) # check auto-correlation
    if (max(abs(pacf_error$acf))>0.5){
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
  }
}

# for Nationwide ---------------------------------------------------------------
# panel data regression
plm_inference_overall <- function(x){
  beta_frame = data.frame()
  time_list = sort(unique(x$year))
  for (Time in time_list){
    Time_data = subset(x,x$year==Time)
    Time_comp = Time_data[rowSums(is.na(Time_data)) == 0,]
    if (nrow(Time_comp)==0){
      next
    }
    # response: crime
    # Scaling on other variables
    Time_comp$youth_rate = (Time_comp$youth_rate-mean(Time_comp$youth_rate))/sd(Time_comp$youth_rate)
    Time_comp$emp_rate = (Time_comp$emp_rate-mean(Time_comp$emp_rate))/sd(Time_comp$emp_rate)
    Time_comp$pop_density = (Time_comp$pop_density-mean(Time_comp$pop_density))/sd(Time_comp$pop_density)
    Time_comp$white_rate = (Time_comp$white_rate-mean(Time_comp$white_rate))/sd(Time_comp$white_rate)
    Time_comp$male_rate = (Time_comp$male_rate-mean(Time_comp$male_rate))/sd(Time_comp$male_rate)
    Time_comp$pol_officer_rate = (Time_comp$pol_officer_rate-mean(Time_comp$pol_officer_rate))/sd(Time_comp$pol_officer_rate)
    # Model fit
    model <- lm(log(crime_rate/100000)~youth_rate+emp_rate+pop_density+white_rate+male_rate+pol_officer_rate,data=Time_comp)
    beta = model$coefficient
    beta_frame = rbind(beta_frame,beta)
  }
  names(beta_frame) = c('intercept','youth_rate','emp_rate','pop_density','white_rate','male_rate','pol_officer_rate')
  # Newest Cov for serial correlation
  se_list = c()
  for (i in 1:ncol(beta_frame)){
    beta_k = beta_frame[,i]
    model <- lm(beta_k~1)
    error_t <- model$residuals
    pacf_error <- pacf(error_t,plot = FALSE)
    if (max(abs(pacf_error$acf))>0.1){
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
names(data_vio)[3] <- 'crime_rate'

# see the structure of data set
str(data_vio)
data_vio$pop_density <- gsub(",","",data_vio$pop_density)
data_vio$pop_density <- as.numeric(data_vio$pop_density)

# use the function to see the result--------------------------------------------
plm_inference_state_level(data_vio)
plm_inference_overall(data_vio)



# We feel curious about how the causality direction is between police and crime
library(lmtest)
direction = data.frame()
state_list = unique(data_vio$region)
for (State in state_list){
  State_data = subset(data_vio,data_vio$region==State)
  y_stationary = diff(State_data$crime_rate)
  x_stationary = diff(State_data$pol_officer_rate)
  if ((Box.test(x_stationary,lag=8,type='Ljung-Box')$p.value<=0.05)|(Box.test(y_stationary,lag=8,type='Ljung-Box')$p.value<=0.05)){
    direction_add = data.frame(State,0) # assumption failed
    names(direction_add) = c('state','direction')
    direction = rbind(direction,direction_add)
    next
  } else {
    if ((grangertest(y_stationary~x_stationary,order=2)$`Pr(>F)`[2]<=0.05)&(grangertest(x_stationary~y_stationary,order=2)$`Pr(>F)`[2]<=0.05)){
      direction_add = data.frame(State,'affect each other') # they affect each other
      names(direction_add) = c('state','direction') 
      direction = rbind(direction,direction_add)
    } else if ((grangertest(y_stationary~x_stationary,order=2)$`Pr(>F)`[2]>0.05)&(grangertest(x_stationary~y_stationary,order=2)$`Pr(>F)`[2]<=0.05)){
      direction_add = data.frame(State,'crime affect police') # crime rate affect police officer rate
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    } else if ((grangertest(y_stationary~x_stationary,order=2)$`Pr(>F)`[2]<=0.05)&(grangertest(x_stationary~y_stationary,order=2)$`Pr(>F)`[2]>0.05)){
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

# see the structure of data set
str(data_prop)
data_prop$pop_density <- gsub(",","",data_prop$pop_density)
data_prop$pop_density <- as.numeric(data_prop$pop_density)
names(data_prop)[3] <- 'crime_rate'

# use the function to see the result--------------------------------------------
plm_inference_state_level(data_prop)
plm_inference_overall(data_prop)

# We feel curious about how the causality direction is between police and crime
library(lmtest)
direction = data.frame()
state_list = unique(data_prop$region)
for (State in state_list){
  State_data = subset(data_prop,data_prop$region==State)
  y_stationary = diff(State_data$crime_rate)
  x_stationary = diff(State_data$pol_officer_rate)
  if ((Box.test(x_stationary,lag=8,type='Ljung-Box')$p.value<=0.05)|(Box.test(y_stationary,lag=8,type='Ljung-Box')$p.value<=0.05)){
    direction_add = data.frame(State,0) # assumption failed
    names(direction_add) = c('state','direction')
    direction = rbind(direction,direction_add)
    next
  } else {
    if ((grangertest(y_stationary~x_stationary,order=2)$`Pr(>F)`[2]<=0.05)&(grangertest(x_stationary~y_stationary,order=2)$`Pr(>F)`[2]<=0.05)){
      direction_add = data.frame(State,'affect each other') # they affect each other
      names(direction_add) = c('state','direction') 
      direction = rbind(direction,direction_add)
    } else if ((grangertest(y_stationary~x_stationary,order=2)$`Pr(>F)`[2]>0.05)&(grangertest(x_stationary~y_stationary,order=2)$`Pr(>F)`[2]<=0.05)){
      direction_add = data.frame(State,'crime affect police') # crime rate affect police officer rate
      names(direction_add) = c('state','direction')
      direction = rbind(direction,direction_add)
    } else if ((grangertest(y_stationary~x_stationary,order=2)$`Pr(>F)`[2]<=0.05)&(grangertest(x_stationary~y_stationary,order=2)$`Pr(>F)`[2]>0.05)){
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
coplot(crime_rate ~ year|region, type="b", data=data_vio)

# show the violent crime and other variables
pairs(State_comp[,3:9],pch = 19, lower.panel = NULL)
