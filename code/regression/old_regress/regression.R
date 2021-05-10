# panel data regression
data_vio <- read.csv('combine_vio.csv',header = T)
data_vio <- subset(data_vio,select=-X)

# see the structure of data set
str(data_vio)
data_vio$pop_density <- gsub(",","",data_vio$pop_density)
data_vio$pop_density <- as.numeric(data_vio$pop_density)

# plm inference with newey-west covariance estimation
plm_inference_state_level <- function(x){
  beta_frame = data.frame()
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
    X = cbind(rep(1,nrow(State_comp)),State_comp[,4:9])
    # Model fit
    model <- lm(log(vio_crime_rate/100000)~youth_rate+emp_rate+pop_density+white_rate+male_rate+pol_officer_rate,data=State_comp)
    beta = model$coefficient
    beta_frame = rbind(beta_frame,beta)
    names(beta_frame) = c('intercept','youth_rate','emp_rate','pop_density','white_rate','male_rate','pol_officer_rate')
    # Verify the residual 
    State_residual = model$residuals
    pacf_error <- pacf(State_residual,plot = F)
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
      df_add = data.frame(beta[i],var_beta[i,i],beta[i]/var_beta[i,i],
                          (1-pt(abs(beta[i]/var_beta[i,i]),df=length(State_residual)-1))*2,
                          (1-pt(abs(beta[i]/var_beta[i,i]),df=length(State_residual)-1))*2<=0.05/ncol(beta_frame))
      names(df_add) = c('point estimation','standard error','t statistics','p-value','significant')
      summary_tbl = rbind(summary_tbl,df_add)
    }
    print(State)
    print(summary_tbl)
  }
}


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
    model <- lm(log(vio_crime_rate/100000)~youth_rate+emp_rate+pop_density+white_rate+male_rate+pol_officer_rate,data=Time_comp)
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
          sum_newey = sum_newey + (1-l/(1+Lag))*error_t[t]*error_t[t-l]
        }
      }
      S <- (sum(error_t^2) + sum_newey)/length(error_t)
      se_list = c(se_list,sqrt(S/length(error_t)))
    } else {
      se_list = c(se_list,sqrt(sum(error_t^2)/ (length(error_t))^2 ))
    }
  }
  # Significant test and summary:
  summary_tbl = data.frame()
  for (i in 1:ncol(beta_frame)){
    df_add = data.frame(colMeans(beta_frame)[i],se_list[i],colMeans(beta_frame)[i]/se_list[i],
                        (1-pt(abs(colMeans(beta_frame)[i]/se_list[i]),df=nrow(beta_frame)-1))*2,
                        (1-pt(abs(colMeans(beta_frame)[i]/se_list[i]),df=nrow(beta_frame)-1))*2<=0.05/ncol(beta_frame))
    names(df_add) = c('point estimation','standard error','t statistics','p-value','significant')
    summary_tbl = rbind(summary_tbl,df_add)
  }
  return(summary_tbl)
}


# show region's violent crime rate by years
coplot(vio_crime_rate ~ year|region, type="l", data=data_vio) 
coplot(vio_crime_rate ~ year|region, type="b", data=data_vio) 

# library(car)
# scatterplot(vio_crime_rate~year|region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=data_vio)

library(plm)
fixed <- plm(vio_crime_rate ~ pop_density, data=data_vio, 
             index=c("region", "year"), model="within")
summary(fixed)
plot(fixed$residuals) 

# check assumptions