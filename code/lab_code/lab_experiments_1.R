library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(stringr)



#function that labels each heating cycle numerically.
add_cycle_column <- function(df, var_name) {
  # Replace NAs in the heater column with 0 (heater off) for cycle detection
  heater_vals <- df[[var_name]]
  heater_vals[is.na(heater_vals)] <- 0
  
  # Create a lagged version to detect transitions
  lag_var <- c(NA, head(heater_vals, -1))
  
  # Detect start of new cycles (0 -> 1 transition)
  df$cycle_start <- ifelse(is.na(lag_var), 
                          heater_vals == 1,
                          lag_var == 0 & heater_vals == 1)
  
  # Create cycle numbers using cumulative sum
  df$cycle <- cumsum(df$cycle_start)
  
  # Clean up
  df$cycle_start <- NULL
  
  return(df)
}
 

mla<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/Lab_analysis/Lab_data/Master_Lab_AllRuns.csv')


mla$HEATER_heater<-as.numeric(mla$HEATER_heater)
dat2<-add_cycle_column(mla,"HEATER_heater")
dat2<-subset(dat2,!is.na(Velocity))



#dat2$time.s<-as.numeric(dat2$time.s)
dat2$temp_A<-as.numeric(dat2$RING01_TrawC)
dat2$temp_B<-as.numeric(dat2$RING01_TrawD)
dat2$temp_C<-as.numeric(dat2$RING01_TrawA)
dat2$temp_D<-as.numeric(dat2$RING01_TrawB)
dat2$temp_E<-as.numeric(dat2$RING01_TrawF)
dat2$temp_F<-as.numeric(dat2$RING01_TrawE)
dat2$time.s<-as.numeric(dat2$time.s)




flow_data_compiled<-data.frame()
cycles<-max(dat2$cycle)

for(i in 1:cycles){
	print(i)
	cycle0<-subset(dat2,cycle==i)
	if(nrow(cycle0)){
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

	temp_C_0<-approx(cycle0$time.s,cycle0$temp_C,xout=end_heat)$y
	temp_C_plus_10<-approx(cycle0$time.s,cycle0$temp_C,xout=end_heat_plus_10)$y
	temp_C_plus_20<-approx(cycle0$time.s,cycle0$temp_C,xout=end_heat_plus_20)$y	

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

	C_F_0<-temp_C_0-temp_F_0
	C_F_10<-temp_C_plus_10-temp_F_plus_10
	C_F_20<-temp_C_plus_20-temp_F_plus_20	

	flow_data<-data.frame(cycle=i,end_heat=end_heat,A_D_0=A_D_0,A_D_10=A_D_10,A_D_20=A_D_20,B_E_0=B_E_0,B_E_10=B_E_10,B_E_20=B_E_20,C_F_0=C_F_0, C_F_10=C_F_10,C_F_20=C_F_20,flow=unique(cycle0$Velocity))
	flow_data_compiled<-bind_rows(flow_data_compiled,flow_data)
}
	
}




flow_data_compiled$A_D_Diff_10<-flow_data_compiled$A_D_10-flow_data_compiled$A_D_0
flow_data_compiled$A_D_Diff_20<-flow_data_compiled$A_D_20-flow_data_compiled$A_D_0


flow_data_compiled$B_E_Diff_10<-flow_data_compiled$B_E_10-flow_data_compiled$B_E_0
flow_data_compiled$B_E_Diff_20<-flow_data_compiled$B_E_20-flow_data_compiled$B_E_0



flow_data_compiled$C_F_Diff_10<-flow_data_compiled$C_F_10-flow_data_compiled$C_F_0
flow_data_compiled$C_F_Diff_20<-flow_data_compiled$C_F_20-flow_data_compiled$C_F_0




flow_data_compiled$A_D_X<-0
flow_data_compiled$A_D_Y<-flow_data_compiled$A_D_Diff_10

B_E_Angle <- 60 * pi / 180
flow_data_compiled$B_E_X<-(flow_data_compiled$B_E_Diff_10)*sin(B_E_Angle)
flow_data_compiled$B_E_Y<-(flow_data_compiled$B_E_Diff_10)*cos(B_E_Angle)

C_F_Angle <- 120 * pi / 180w
flow_data_compiled$C_F_X<-(flow_data_compiled$C_F_Diff_10)*sin(C_F_Angle)
flow_data_compiled$C_F_Y<-(flow_data_compiled$C_F_Diff_10)*cos(C_F_Angle)

flow_data_compiled$Sum_X_10<-flow_data_compiled$A_D_X+flow_data_compiled$B_E_X+flow_data_compiled$C_F_X
flow_data_compiled$Sum_Y_10<-flow_data_compiled$A_D_Y+flow_data_compiled$B_E_Y+flow_data_compiled$C_F_Y

flow_data_compiled$Magnitude_10<-sqrt(flow_data_compiled$Sum_X_10^2*flow_data_compiled$Sum_Y_10^2)

flow_data_compiled$angle_10<-atan2(flow_data_compiled$Sum_Y_10, flow_data_compiled$Sum_X_10)
flow_data_compiled$angle_10_degrees<-flow_data_compiled$angle_10*(180/pi)

mag10_plot<-ggplot(flow_data_compiled,aes(x=flow,y=A_D_Y,color=cycle))+
geom_point()
mag10_plot













flow_160<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/November 2025/20251110_1247/1049094.CSV')

flow_190<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/November 2025/20251110_1543/1050225.CSV')


flow_160$flow<-160
flow_190$flow<-190

flowd<-bind_rows(flow_160,flow_190)

flowds<-flowd[,c("RING01_TrawA","RING01_TrawB","RING01_TrawC","RING01_TrawD","RING01_TrawE","RING01_TrawF","flow","time.s","HEATER_heater")]

flowds$dtp<-as_datetime(flowds$time.s)

flowdsm<-melt(flowds,na.rm=TRUE,id=c("flow","time.s","dtp","HEATER_heater"))

ggplot(flowdsm,aes(dtp,value,color=flow))+
geom_point()+
facet_wrap(.~variable)





dat<-flowds

  0b0011000, 0b0011001, 0b0011110, 0b0011101, 0b0011010, 0b0011100,


A - U7 - 0011011 - Currently C
B - U5 - 0011101 - currently D
C - U4 - 0011000 - currently A
D - U6 - 0011001 - Currently B
E - U3 - 0011100 - currently F
F - U2 - 0011010 - currently E






#dat$time.s<-as.numeric(dat$time.s)
dat$temp_A<-as.numeric(dat$RING01_TrawC)
dat$temp_B<-as.numeric(dat$RING01_TrawD)
dat$temp_C<-as.numeric(dat$RING01_TrawA)
dat$temp_D<-as.numeric(dat$RING01_TrawB)
dat$temp_E<-as.numeric(dat$RING01_TrawF)
dat$temp_F<-as.numeric(dat$RING01_TrawE)
#dat<-subset(dat,time.s>5e8)
dat<-subset(dat,!is.na(HEATER_heater))
dat2<-add_cycle_column(dat,"HEATER_heater")


cycles<-max(dat2$cycle,na.rm=TRUE)

