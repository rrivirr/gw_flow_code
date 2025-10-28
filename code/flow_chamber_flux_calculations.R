library(dplyr)
library(ggplot2)

dat<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/September-LabTesting-data/USGS-chamber/October_27/9995370.CSV')


ggplot(dat,aes(time.s,RING01_Traw0))+
geom_point()




dat<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/September-LabTesting-data/USGS-chamber/OCt_23/9994301.CSV')


ggplot(dat,aes(time.s,RING01_Traw0))+
geom_point()





A-D
B-E
C-F


dat<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/September-LabTesting-data/Sept30-240s-ONtime_1200sOFFtime_4thVel/9999408.CSV')





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





dat$time.s<-as.numeric(dat$time.s)
dat$temp_A<-as.numeric(dat$RING01_TrawA)
dat$temp_B<-as.numeric(dat$RING01_TrawB)
dat$temp_C<-as.numeric(dat$RING01_TrawC)
dat$temp_D<-as.numeric(dat$RING01_TrawD)
dat$temp_E<-as.numeric(dat$RING01_TrawE)
dat$temp_F<-as.numeric(dat$RING01_TrawF)
dat<-subset(dat,time.s>5e8)

dat2<-add_cycle_column(dat,"HEATER_heater")


cycles<-max(dat2$cycle,na.rm=TRUE)


flow_data_compiled<-data.frame()

for(i in 1:cycles){
	cycle0<-subset(dat2,cycle==i)
	heat_cycle<-subset(cycle0,HEATER_heater==1)
	end_heat<-max(heat_cycle$time.s,na.rm=TRUE)
	cold_cycle<-subset(cycle0,HEATER_heater==0)
	end_heat_plus_10<-end_heat+(10*60)
	end_heat_plus_20<-end_heat+(20*60)
	
	temp_A_0<-approx(cycle0$time.s,cycle0$temp_A,xout=end_heat)$y
	temp_A_plus_10<-approx(cycle0$time.s,cycle0$temp_A,xout=end_heat_plus_10)$y
	temp_A_plus_20<-approx(cycle0$time.s,cycle0$temp_A,xout=end_heat_plus_20)$y	

	temp_B_0<-approx(cycle0$time.s,cycle0$temp_B,xout=end_heat)$y
	temp_B_plus_10<-approx(cycle0$time.s,cycle0$temp_B,xout=end_heat_plus_10)$y
	temp_B_plus_20<-approx(cycle0$time.s,cycle0$temp_B,xout=end_heat_plus_20)$y	

#	temp_C_0<-approx(cycle0$time.s,cycle0$temp_C,xout=end_heat)$y
#	temp_C_plus_10<-approx(cycle0$time.s,cycle0$temp_C,xout=end_heat_plus_10)$y
#	temp_C_plus_20<-approx(cycle0$time.s,cycle0$temp_C,xout=end_heat_plus_20)$y	

	temp_D_0<-approx(cycle0$time.s,cycle0$temp_D,xout=end_heat)$y
	temp_D_plus_10<-approx(cycle0$time.s,cycle0$temp_D,xout=end_heat_plus_10)$y
	temp_D_plus_20<-approx(cycle0$time.s,cycle0$temp_D,xout=end_heat_plus_20)$y	

	temp_E_0<-approx(cycle0$time.s,cycle0$temp_E,xout=end_heat)$y
	temp_E_plus_10<-approx(cycle0$time.s,cycle0$temp_E,xout=end_heat_plus_10)$y
	temp_E_plus_20<-approx(cycle0$time.s,cycle0$temp_E,xout=end_heat_plus_20)$y	

	temp_F_0<-approx(cycle0$time.s,cycle0$temp_F,xout=end_heat)$y
	temp_F_plus_10<-approx(cycle0$time.s,cycle0$temp_F,xout=end_heat_plus_10)$y
	temp_F_plus_20<-approx(cycle0$time.s,cycle0$temp_F,xout=end_heat_plus_20)$y
	
	A_D_0<-temp_A_0-temp_D_0
	A_D_10<-temp_A_plus_10-temp_D_plus_10
	A_D_20<-temp_A_plus_20-temp_D_plus_20	

	B_E_0<-temp_B_0-temp_E_0
	B_E_10<-temp_B_plus_10-temp_E_plus_10
	B_E_20<-temp_B_plus_20-temp_E_plus_20	

#	C_F_0<-temp_C_0-temp_F_0
#	C_F_10<-temp_C_plus_10-temp_F_plus_10
#	C_F_20<-temp_C_plus_20-temp_F_plus_20	

	flow_data<-data.frame(cycle=i,end_heat=end_heat,A_D_0=A_D_0,A_D_10=A_D_10,A_D_20=A_D_20,B_E_0=B_E_0,B_E_10=B_E_10,B_E_20=B_E_20)
	flow_data_compiled<-bind_rows(flow_data_compiled,flow_data)

	
}

flow_data_compiled$A_D_Diff_10<-flow_data_compiled$A_D_10-flow_data_compiled$A_D_0
flow_data_compiled$A_D_Diff_20<-flow_data_compiled$A_D_20-flow_data_compiled$A_D_0


flow_data_compiled$B_E_Diff_10<-flow_data_compiled$B_E_10-flow_data_compiled$B_E_0
flow_data_compiled$B_E_Diff_20<-flow_data_compiled$B_E_20-flow_data_compiled$B_E_0

flow_data_compiled$Sum_X_10<-flow_data_compiled$B_E_Diff_10/2
flow_data_compiled$Sum_X_20<-flow_data_compiled$B_E_Diff_20/2

flow_data_compiled$Sum_Y_10<-(flow_data_compiled$B_E_Diff_10/2)*sqrt(3)+flow_data_compiled$A_D_Diff_10
flow_data_compiled$Sum_Y_20<-(flow_data_compiled$B_E_Diff_20/2)*sqrt(3)+flow_data_compiled$A_D_Diff_20

flow_data_compiled$angle_10<-atan2(flow_data_compiled$Sum_Y_10, flow_data_compiled$Sum_X_10)
flow_data_compiled$angle_20<-atan2(flow_data_compiled$Sum_Y_20, flow_data_compiled$Sum_X_20)

flow_data_compiled$flow_mag_10<-sqrt(flow_data_compiled$Sum_X_10^2+flow_data_compiled$Sum_Y_10^2)
flow_data_compiled$flow_mag_20<-sqrt(flow_data_compiled$Sum_X_20^2+flow_data_compiled$Sum_Y_20^2)



ggplot(flow_data_compiled,aes(cycle,A_D_Diff_10))+geom_point()
