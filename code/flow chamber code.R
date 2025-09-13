library(ggplot2)
library(lubridate)


#import some data
dat<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/September-LabTesting-data/September12-120-ON_600-OFF_lowest-vel/8393763.CSV')

#function that labels each heating cycle numerically.
add_cycle_column <- function(df, var_name) {
  # Create a lagged version to detect transitions
  df$lag_var <- c(NA, head(df[[var_name]], -1))
  
  # Detect start of new cycles (0 -> 1 transition)
  df$cycle_start <- ifelse(is.na(df$lag_var), 
                          df[[var_name]] == 1,  # First row: start cycle if it's 1
                          df$lag_var == 0 & df[[var_name]] == 1)
  
  # Create cycle numbers using cumulative sum
  df$cycle <- cumsum(df$cycle_start)
  
  # If the first value is 0, it belongs to cycle 0 (pre-first cycle)
  if (!is.na(df[[var_name]][1]) && df[[var_name]][1] == 0) {
    df$cycle <- df$cycle
  }
  
  # Clean up temporary columns
  df$lag_var <- NULL
  df$cycle_start <- NULL
  
  return(df)
}


#converting epoch to posix
dat$dtp<-as_datetime(dat$time.s)

#getting the cycles
dat2<-add_cycle_column(dat,"HEATER_heater")

#subsetting to look at a specific cycle.
dat2_cycle7<-subset(dat2,cycle==59)

#plotting the cycle that has been subset
ggplot(dat2_cycle7,aes(dtp,X000000_TrawB-X000000_TrawE,color=as.factor(HEATER_heater)))+
geom_point()

#plotting the whole dataset
ggplot(dat2,aes(dtp,X000000_TrawB,color=as.factor(HEATER_heater)))+
geom_point()

#plotting the difference between opposite sensors on the whole dataset
ggplot(dat2,aes(dtp,X000000_TrawB-X000000_TrawE,color=as.factor(HEATER_heater)))+
geom_point()
