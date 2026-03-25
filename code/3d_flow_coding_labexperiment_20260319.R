library(dplyr)
library(doBy)
library(ggplot2)
library(lubridate)
library(reshape2)

# =============================================================================
# CORRECTED 3D Flow Sensor Analysis
# 
# Key changes from original:
#   1. Vertical signal: use RING AVERAGES (mean Ring1 - mean Ring3) instead of
#      diagonal pairs (A1-D3, B1-E3, C1-F3). Diagonal pairs conflate horizontal
#      and vertical flow because A and D are on opposite sides of the ring.
#   2. Fixed sin(57.68) radians bug (R expects radians, not degrees).
#   3. Use all 6 same-position upper-lower pairs averaged for vertical signal.
# =============================================================================

average_traw <- function(df, interval = 30) {
  library(tidyverse)
  
  t0 <- min(df$time.s)
  traw_cols <- names(df) %>% str_subset("Traw")
  
  # Force Traw columns to numeric
  df <- df %>%
    mutate(across(all_of(traw_cols), as.numeric))
  
  df %>%
    mutate(bin = floor((time.s - t0) / interval)) %>%
    group_by(bin) %>%
    summarise(
      time.s_start = min(time.s),
      time.s_end   = max(time.s),
      time.s_mean  = mean(time.s),
      TS_switch    = first(TS_switch),
      flow         = first(flow),
      across(all_of(traw_cols), \(x) mean(x, na.rm = TRUE), .names = "{.col}_mean")
    ) %>%
    select(-bin)
}

#function that labels each heating cycle numerically.
add_cycle_column <- function(df, var_name) {
  # Create a lagged version to detect transitions
  df$lag_var <- c(NA, head(df[[var_name]], -1))
  
  # Detect start of new cycles (0 -> 1 transition)
  df$cycle_start <- ifelse(is.na(df$lag_var), 
                          df[[var_name]] == 1,
                          df$lag_var == 0 & df[[var_name]] == 1)
  
  # Create cycle numbers using cumulative sum
  df$cycle <- cumsum(df$cycle_start)
  
  # If the first value is 0, it belongs to cycle 0 (pre-first cycle)
  if (!is.na(df[[var_name]][1]) && df[[var_name]][1] == 0) {
    df$cycle <- df$cycle
  }
  
  # Time since cycle start (seconds)
  df <- df %>%
    group_by(cycle) %>%
    mutate(time_loop = time.s - min(time.s)) %>%
    ungroup()
  
  # Clean up temporary columns
  df$lag_var <- NULL
  df$cycle_start <- NULL
  
  return(df)
}





m90<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/20260313_1206_90ml-min/9438310.CSV')

m90_30<-as.data.frame(average_traw(m90, interval = 10))
m90_30$flow<-90
#m90_30<-m90_30[-c(1:3600),]



m130<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/20260314_1730_130ml-min/9442832.CSV')
m130_30<-as.data.frame(average_traw(m130, interval = 10))
m130_30$flow<-130
#m130_30<-m130_30[-c(1:3600),]




m190<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/20260315_1655_190ml-min/9475176.CSV')
m190_30<-as.data.frame(average_traw(m190, interval = 10))
m190_30$flow<-190
#m190_30<-m190_30[-c(1:3600),]


m240<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/20260316_1030_240ml-min/9438265.CSV')
m240_30<-as.data.frame(average_traw(m240, interval = 10))
m240_30$flow<-240
#m240_30<-m240_30[-c(1:3600),]




m190$RING01_TrawA<-as.numeric(m190$RING01_TrawA)
m190$RING01_TrawB<-as.numeric(m190$RING01_TrawB)
m190$RING01_TrawD<-as.numeric(m190$RING01_TrawD)
m190$RING01_TrawF<-as.numeric(m190$RING01_TrawF)

mla<-bind_rows(m90_30,m130_30,m190_30,m240_30)


dat1<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/20260318_2119_all-velocities/flow_appended_9999899.CSV')

dat1_15<-average_traw(dat1, interval = 15)



dat1_15$HEATER_heater<-as.numeric(dat1_15$TS_switch)
dat1_15$time.s<-dat1_15$time.s_mean
dat1_15_c<-as.data.frame(add_cycle_column(dat1_15,"HEATER_heater"))
dat1_15_c<-subset(dat1_15_c,!is.na(flow))



#dat1_15_c$time.s<-as.numeric(dat1_15_c$time.s)
dat1_15_c$temp_A<-as.numeric(dat1_15_c$RING01_TrawA_mean)
dat1_15_c$temp_B<-as.numeric(dat1_15_c$RING01_TrawB_mean)
dat1_15_c$temp_C<-as.numeric(dat1_15_c$RING01_TrawC_mean)
dat1_15_c$temp_D<-as.numeric(dat1_15_c$RING01_TrawE_mean)
dat1_15_c$temp_E<-as.numeric(dat1_15_c$RING01_TrawD_mean)
dat1_15_c$temp_F<-as.numeric(dat1_15_c$RING01_TrawF_mean)
dat1_15_c$time.s<-as.numeric(dat1_15_c$time.s)



dat1_15_c$time<-as.integer(dat1_15_c$time.s)
dat1_15_c$dtp<-as_datetime(dat1_15_c$time)


cycle_mins<-data.frame()
max_cyc<-max(dat1_15_c$cycle)
for(i in 1:max_cyc){
	datc<-subset(dat1_15_c,cycle==i)
	if(nrow(datc)>20){
	temp_A_min<-min(datc$temp_A)
	temp_B_min<-min(datc$temp_B)
	temp_C_min<-min(datc$temp_C)
	temp_D_min<-min(datc$temp_D)
	temp_E_min<-min(datc$temp_E)
	temp_F_min<-min(datc$temp_F)
	
	temp_A_min_ts<-datc$time.s[which.min(datc$temp_A)]	
	temp_B_min_ts<-datc$time.s[which.min(datc$temp_B)]	
	temp_C_min_ts<-datc$time.s[which.min(datc$temp_C)]	
	temp_D_min_ts<-datc$time.s[which.min(datc$temp_D)]
	temp_E_min_ts<-datc$time.s[which.min(datc$temp_E)]
	temp_F_min_ts<-datc$time.s[which.min(datc$temp_F)]			

	tempc<-data.frame(cycle=i,temp_A_min=temp_A_min,temp_B_min=temp_B_min,temp_C_min=temp_C_min,temp_D_min=temp_D_min,temp_E_min=temp_E_min,temp_F_min=temp_F_min,temp_A_min_ts=temp_A_min_ts,temp_B_min_ts=temp_B_min_ts,temp_C_min_ts=temp_C_min_ts,temp_D_min_ts=temp_D_min_ts,temp_E_min_ts=temp_E_min_ts,temp_F_min_ts=temp_F_min_ts)
	cycle_mins<-bind_rows(cycle_mins,tempc)
}
}



  low_A <- loess(cycle_mins$temp_A_min ~ cycle_mins$temp_A_min_ts, span = 0.75)
  low_B <- loess(cycle_mins$temp_B_min ~ cycle_mins$temp_B_min_ts, span = 0.75)  
  low_C <- loess(cycle_mins$temp_C_min ~ cycle_mins$temp_C_min_ts, span = 0.75)
  low_D <- loess(cycle_mins$temp_D_min ~ cycle_mins$temp_D_min_ts, span = 0.75)
  low_E <- loess(cycle_mins$temp_E_min ~ cycle_mins$temp_E_min_ts, span = 0.75)
  low_F <- loess(cycle_mins$temp_F_min ~ cycle_mins$temp_F_min_ts, span = 0.75)
  
  
  dat1_15_c$low_A_pred<-predict(low_A,newdata=dat1_15_c$time.s)
  dat1_15_c$low_B_pred<-predict(low_B,newdata=dat1_15_c$time.s)
  dat1_15_c$low_C_pred<-predict(low_C,newdata=dat1_15_c$time.s)    
  dat1_15_c$low_D_pred<-predict(low_D,newdata=dat1_15_c$time.s)  
  dat1_15_c$low_E_pred<-predict(low_E,newdata=dat1_15_c$time.s)
  dat1_15_c$low_F_pred<-predict(low_F,newdata=dat1_15_c$time.s)
  
  
  
dat1_15_c$temp_A_off<-dat1_15_c$temp_A-dat1_15_c$low_A_pred
dat1_15_c$temp_B_off<-dat1_15_c$temp_B-dat1_15_c$low_B_pred
dat1_15_c$temp_C_off<-dat1_15_c$temp_C-dat1_15_c$low_C_pred
dat1_15_c$temp_D_off<-dat1_15_c$temp_D-dat1_15_c$low_D_pred
dat1_15_c$temp_E_off<-dat1_15_c$temp_E-dat1_15_c$low_E_pred
dat1_15_c$temp_F_off<-dat1_15_c$temp_F-dat1_15_c$low_F_pred







raw_compiled<-data.frame()
flow_data_compiled<-data.frame()
cycles<-max(dat1_15_c$cycle)

for(i in 1:cycles){
	print(i)
	cycle0<-subset(dat1_15_c,cycle==i & !is.na(temp_A_off))
	if(nrow(cycle0)>10){	

	
	heat_cycle<-subset(cycle0,HEATER_heater==1)
	cold_cycle<-subset(cycle0,HEATER_heater==0)




	time0<-heat_cycle$time.s[1]
	time480<-cold_cycle$time.s[1]
	
	temp_A_0<-heat_cycle$temp_A_off[1]
	temp_A_480<-cold_cycle$temp_A_off[1]
		
	temp_B_0<-heat_cycle$temp_B_off[1]
	temp_B_480<-cold_cycle$temp_B_off[1]

	temp_C_0<-heat_cycle$temp_C_off[1]
	temp_C_480<-cold_cycle$temp_C_off[1]
	
	temp_D_0<-heat_cycle$temp_D_off[1]
	temp_D_480<-cold_cycle$temp_D_off[1]
	
	temp_E_0<-heat_cycle$temp_E_off[1]
	temp_E_480<-cold_cycle$temp_E_off[1]
	
	temp_F_0<-heat_cycle$temp_F_off[1]
	temp_F_480<-cold_cycle$temp_F_off[1]
	
	temp_A_600<-approx(cycle0$time_loop,cycle0$temp_A_off,xout=data.frame(time_loop=600))$y
	temp_B_600<-approx(cycle0$time_loop,cycle0$temp_B_off,xout=data.frame(time_loop=600))$y	
	temp_C_600<-approx(cycle0$time_loop,cycle0$temp_C_off,xout=data.frame(time_loop=600))$y
	temp_D_600<-approx(cycle0$time_loop,cycle0$temp_D_off,xout=data.frame(time_loop=600))$y		
	temp_E_600<-approx(cycle0$time_loop,cycle0$temp_E_off,xout=data.frame(time_loop=600))$y
	temp_F_600<-approx(cycle0$time_loop,cycle0$temp_F_off,xout=data.frame(time_loop=600))$y
	
	temp_A_min<-min(cycle0$temp_A_off)
	temp_B_min<-min(cycle0$temp_B_off)
	temp_C_min<-min(cycle0$temp_C_off)
	temp_D_min<-min(cycle0$temp_D_off)
	temp_E_min<-min(cycle0$temp_E_off)
	temp_F_min<-min(cycle0$temp_F_off)					
	
	
	temp_A_max<-max(cycle0$temp_A_off)
	temp_B_max<-max(cycle0$temp_B_off)
	temp_C_max<-max(cycle0$temp_C_off)
	temp_D_max<-max(cycle0$temp_D_off)
	temp_E_max<-max(cycle0$temp_E_off)
	temp_F_max<-max(cycle0$temp_F_off)					


	raw_compiled<-bind_rows(raw_compiled,cycle0)
	
	flow_data<-data.frame(cycle=i,time0=time0,time480=time480,temp_A_0=temp_A_0,temp_A_480=temp_A_480,temp_B_0=temp_B_0,temp_B_480=temp_B_480,temp_C_0=temp_C_0,temp_C_480=temp_C_480,temp_D_0=temp_D_0,temp_D_480=temp_D_480,temp_E_0=temp_E_0,temp_E_480=temp_E_480,temp_F_0=temp_F_0,temp_F_480=temp_F_480,temp_A_min,temp_B_min,temp_C_min,temp_D_min,temp_E_min,temp_F_min,temp_A_max,temp_B_max,temp_C_max,temp_D_max,temp_E_max,temp_F_max,flow=unique(cycle0$flow),mean_trend=mean_trend,temp_A_600=temp_A_600,temp_B_600=temp_B_600,temp_C_600=temp_C_600,temp_D_600=temp_D_600,temp_E_600=temp_E_600,temp_F_600=temp_F_600)
	flow_data_compiled<-bind_rows(flow_data_compiled,flow_data)
}
	
}






flow_data_compiled$A_range<-flow_data_compiled$temp_A_600-flow_data_compiled$temp_A_0
flow_data_compiled$B_range<-flow_data_compiled$temp_B_600-flow_data_compiled$temp_B_0
flow_data_compiled$C_range<-flow_data_compiled$temp_C_600-flow_data_compiled$temp_C_0
flow_data_compiled$D_range<-flow_data_compiled$temp_D_600-flow_data_compiled$temp_D_0
flow_data_compiled$E_range<-flow_data_compiled$temp_E_600-flow_data_compiled$temp_E_0
flow_data_compiled$F_range<-flow_data_compiled$temp_F_600-flow_data_compiled$temp_F_0


flow_data_compiled$A_range<-flow_data_compiled$temp_A_600
flow_data_compiled$B_range<-flow_data_compiled$temp_B_600
flow_data_compiled$C_range<-flow_data_compiled$temp_C_600
flow_data_compiled$D_range<-flow_data_compiled$temp_D_600
flow_data_compiled$E_range<-flow_data_compiled$temp_E_600
flow_data_compiled$F_range<-flow_data_compiled$temp_F_600


flow_data_compiled$AD_range<-flow_data_compiled$A_range-flow_data_compiled$D_range
flow_data_compiled$BE_range<-flow_data_compiled$B_range-flow_data_compiled$E_range
flow_data_compiled$CF_range<-flow_data_compiled$C_range-flow_data_compiled$F_range

ggplot(flow_data_compiled,aes(cycle,D_range,color=as.factor(flow)))+geom_point()


A_D_Angle <- 30 * pi / 180
B_E_Angle <- 90 * pi / 180
F_C_Angle <- 150 * pi / 180

flow_data_compiled$A_D_X <- flow_data_compiled$AD_range * sin(A_D_Angle)
flow_data_compiled$A_D_Z <- flow_data_compiled$AD_range * cos(A_D_Angle)
flow_data_compiled$B_E_X <- flow_data_compiled$BE_range * sin(B_E_Angle)
flow_data_compiled$B_E_Z <- flow_data_compiled$BE_range * cos(B_E_Angle)
flow_data_compiled$C_F_X <- flow_data_compiled$CF_range * sin(F_C_Angle)
flow_data_compiled$C_F_Z <- flow_data_compiled$CF_range * cos(F_C_Angle)

flow_data_compiled$total_x <- flow_data_compiled$A_D_X + flow_data_compiled$B_E_X + flow_data_compiled$C_F_X
flow_data_compiled$total_z <- flow_data_compiled$A_D_Z + flow_data_compiled$B_E_Z + flow_data_compiled$C_F_Z
flow_data_compiled$horiz_mag <- sqrt(flow_data_compiled$total_x^2 + flow_data_compiled$total_z^2)





ggplot(flow_data_compiled,aes(flow,horiz_mag))+geom_point()




write.csv(dat1_15_c,"dat1_15_c.csv",row.names=FALSE)



cycle_dat<-subset(dat1_15_c)
ggplot(cycle_dat,aes(time_loop,temp_C_off,color=as.factor(flow)))+geom_point()

























    
library(tidyverse)

# --- 1. Fit LOESS to cycle mins and predict baseline for full dataset ---

thermistors <- c("A", "B", "C", "D", "E", "F")

# Start with your full dataset
dat_corrected <- dat1_15_c

for(th in thermistors) {
  min_col <- paste0("temp_", th, "_min")
  ts_col  <- paste0("temp_", th, "_min_ts")
  raw_col <- paste0("temp_", th)
  
  # Fit LOESS on cycle minimums
  lo <- loess(cycle_mins[[min_col]] ~ cycle_mins[[ts_col]], span = 0.75)
  
  # Predict baseline at every timestamp in the full dataset
  baseline <- predict(lo, newdata = dat_corrected$time.s_mean)
  
  # Subtract baseline
  dat_corrected[[paste0(raw_col, "_corr")]] <- dat_corrected[[raw_col]] - baseline
  dat_corrected[[paste0(raw_col, "_baseline")]] <- baseline
}


ggplot(dat1_15_c,aes(dtp,temp_A,color=as.factor(flow)))+
geom_point(aes(color=TS_switch))



cyc_melt<-melt(dat1_15_c[,c("cycle","flow","time_loop","temp_A","temp_B","temp_C","temp_D","temp_E","temp_F")],id=c("cycle","flow","time_loop"))




cycles_dat_10<-subset(dat1_15_c,cycle==15)
cycles_dat_10$temp_A_norm<-cycles_dat_10$temp_A-min(cycles_dat_10$temp_A)
cycles_dat_10$temp_B_norm<-cycles_dat_10$temp_B-min(cycles_dat_10$temp_B)
cycles_dat_10$temp_C_norm<-cycles_dat_10$temp_C-min(cycles_dat_10$temp_C)
cycles_dat_10$temp_D_norm<-cycles_dat_10$temp_D-min(cycles_dat_10$temp_D)
cycles_dat_10$temp_E_norm<-cycles_dat_10$temp_E-min(cycles_dat_10$temp_E)
cycles_dat_10$temp_F_norm<-cycles_dat_10$temp_F-min(cycles_dat_10$temp_F)

cycles_dat_77<-subset(dat1_15_c,cycle==80)
cycles_dat_77$temp_A_norm<-cycles_dat_77$temp_A-min(cycles_dat_77$temp_A)
cycles_dat_77$temp_B_norm<-cycles_dat_77$temp_B-min(cycles_dat_77$temp_B)
cycles_dat_77$temp_C_norm<-cycles_dat_77$temp_C-min(cycles_dat_77$temp_C)
cycles_dat_77$temp_D_norm<-cycles_dat_77$temp_D-min(cycles_dat_77$temp_D)
cycles_dat_77$temp_E_norm<-cycles_dat_77$temp_E-min(cycles_dat_77$temp_E)
cycles_dat_77$temp_F_norm<-cycles_dat_77$temp_F-min(cycles_dat_77$temp_F)

cycles_dat_136<-subset(dat1_15_c,cycle==136)
cycles_dat_136$temp_A_norm<-cycles_dat_136$temp_A-min(cycles_dat_136$temp_A)
cycles_dat_136$temp_B_norm<-cycles_dat_136$temp_B-min(cycles_dat_136$temp_B)
cycles_dat_136$temp_C_norm<-cycles_dat_136$temp_C-min(cycles_dat_136$temp_C)
cycles_dat_136$temp_D_norm<-cycles_dat_136$temp_D-min(cycles_dat_136$temp_D)
cycles_dat_136$temp_E_norm<-cycles_dat_136$temp_E-min(cycles_dat_136$temp_E)
cycles_dat_136$temp_F_norm<-cycles_dat_136$temp_F-min(cycles_dat_136$temp_F)

cycles_dat_191<-subset(dat1_15_c,cycle==188)
cycles_dat_191$temp_A_norm<-cycles_dat_191$temp_A-min(cycles_dat_191$temp_A)
cycles_dat_191$temp_B_norm<-cycles_dat_191$temp_B-min(cycles_dat_191$temp_B)
cycles_dat_191$temp_C_norm<-cycles_dat_191$temp_C-min(cycles_dat_191$temp_C)
cycles_dat_191$temp_D_norm<-cycles_dat_191$temp_D-min(cycles_dat_191$temp_D)
cycles_dat_191$temp_E_norm<-cycles_dat_191$temp_E-min(cycles_dat_191$temp_E)
cycles_dat_191$temp_F_norm<-cycles_dat_191$temp_F-min(cycles_dat_191$temp_F)

cycles_dat<-bind_rows(cycles_dat_10,cycles_dat_77,cycles_dat_136,cycles_dat_191)


cyc_melt<-melt(cycles_dat[,c("flow","cycle","time_loop","temp_A_norm","temp_B_norm","temp_C_norm","temp_D_norm","temp_E_norm","temp_F_norm")],id=c("cycle","flow","time_loop"))

ggplot(cyc_melt,aes(time_loop,value,color=as.factor(cycle)))+
facet_wrap(~variable)+
geom_point()

#cycles_dat<-subset(dat1_15_c,cycle==10|cycle==77|cycle==136|cycle==191)


all_130<-subset(dat1_15_c,flow==130&time_loop<2000)

ggplot(all_130,aes(time_loop,temp_C,color=cycle))+geom_point()




ggplot(cycles_dat,aes(time_loop,temp_E_norm,color=as.factor(flow)))+
facet_wrap(~)
geom_point(aes())



#names(simout)[1:6] <- c("Time","Type","Velocity_ft","XZ_Angle","YZ_Angle","Cycle")
#simout<-subset(simout,!(YZ_Angle==30&Velocity_ft==15))

simout<-dat2


write.csv(dat2[1:10000,],"flow_data_comp.csv",row.names=FALSE)


raw_compiled<-data.frame()
flow_data_compiled<-data.frame()
cycles<-max(dat1_15_c$cycle)

for(i in 1:cycles){
	print(i)
	cycle0<-subset(dat1_15_c,cycle==i)
	if(nrow(cycle0)){	

	

	rows<-nrow(cycle0)
	rows5<-rows-5

	A_trend<-mean(cycle0$temp_A[1:5])-mean(cycle0$temp_A[rows5:rows])
	B_trend<-mean(cycle0$temp_B[1:5])-mean(cycle0$temp_B[rows5:rows])
	C_trend<-mean(cycle0$temp_C[1:5])-mean(cycle0$temp_C[rows5:rows])		
	D_trend<-mean(cycle0$temp_D[1:5])-mean(cycle0$temp_D[rows5:rows])	
	E_trend<-mean(cycle0$temp_E[1:5])-mean(cycle0$temp_E[rows5:rows])	
	F_trend<-mean(cycle0$temp_F[1:5])-mean(cycle0$temp_F[rows5:rows])
	
	mean_trend<-mean(c(A_trend,B_trend,C_trend,D_trend,E_trend,F_trend))
	trend_slope<-mean_trend/max(cycle0$time_loop)
	
	cycle0$temp_A_corr<-(cycle0$temp_A+(trend_slope*cycle0$time_loop))-min(cycle0$temp_A)
	cycle0$temp_B_corr<-(cycle0$temp_B+(trend_slope*cycle0$time_loop))-min(cycle0$temp_B)	
	cycle0$temp_C_corr<-(cycle0$temp_C+(trend_slope*cycle0$time_loop))-min(cycle0$temp_C)	
	cycle0$temp_D_corr<-(cycle0$temp_D+(trend_slope*cycle0$time_loop))-min(cycle0$temp_D)	
	cycle0$temp_E_corr<-(cycle0$temp_E+(trend_slope*cycle0$time_loop))-min(cycle0$temp_E)	
	cycle0$temp_F_corr<-(cycle0$temp_F+(trend_slope*cycle0$time_loop))-min(cycle0$temp_F)	

	heat_cycle<-subset(cycle0,HEATER_heater==1)
	cold_cycle<-subset(cycle0,HEATER_heater==0)




	time0<-heat_cycle$time.s[1]
	time480<-cold_cycle$time.s[1]
	
	temp_A_0<-heat_cycle$temp_A_corr[1]
	temp_A_480<-cold_cycle$temp_A_corr[1]
		
	temp_B_0<-heat_cycle$temp_B_corr[1]
	temp_B_480<-cold_cycle$temp_B_corr[1]

	temp_C_0<-heat_cycle$temp_C_corr[1]
	temp_C_480<-cold_cycle$temp_C_corr[1]
	
	temp_D_0<-heat_cycle$temp_D_corr[1]
	temp_D_480<-cold_cycle$temp_D_corr[1]
	
	temp_E_0<-heat_cycle$temp_E_corr[1]
	temp_E_480<-cold_cycle$temp_E_corr[1]
	
	temp_F_0<-heat_cycle$temp_F_corr[1]
	temp_F_480<-cold_cycle$temp_F_corr[1]
	
	temp_A_min<-min(cycle0$temp_A_corr)
	temp_B_min<-min(cycle0$temp_B_corr)
	temp_C_min<-min(cycle0$temp_C_corr)
	temp_D_min<-min(cycle0$temp_D_corr)
	temp_E_min<-min(cycle0$temp_E_corr)
	temp_F_min<-min(cycle0$temp_F_corr)					
	
	
	temp_A_max<-max(cycle0$temp_A_corr)
	temp_B_max<-max(cycle0$temp_B_corr)
	temp_C_max<-max(cycle0$temp_C_corr)
	temp_D_max<-max(cycle0$temp_D_corr)
	temp_E_max<-max(cycle0$temp_E_corr)
	temp_F_max<-max(cycle0$temp_F_corr)					


	raw_compiled<-bind_rows(raw_compiled,cycle0)
	
	flow_data<-data.frame(cycle=i,time0=time0,time480=time480,temp_A_0=temp_A_0,temp_A_480=temp_A_480,temp_B_0=temp_B_0,temp_B_480=temp_B_480,temp_C_0=temp_C_0,temp_C_480=temp_C_480,temp_D_0=temp_D_0,temp_D_480=temp_D_480,temp_E_0=temp_E_0,temp_E_480=temp_E_480,temp_F_0=temp_F_0,temp_F_480=temp_F_480,temp_A_min,temp_B_min,temp_C_min,temp_D_min,temp_E_min,temp_F_min,temp_A_max,temp_B_max,temp_C_max,temp_D_max,temp_E_max,temp_F_max,flow=unique(cycle0$flow),mean_trend=mean_trend)
	flow_data_compiled<-bind_rows(flow_data_compiled,flow_data)
}
	
}


ggplot(flow_data_compiled,aes(flow,))

all_130<-subset(raw_compiled,flow==130&time_loop<2000)

ggplot(all_130,aes(time_loop,temp_C_corr,color=cycle))+geom_point()





flow_data_compiled$A_range<-flow_data_compiled$temp_A_max-flow_data_compiled$temp_A_0
flow_data_compiled$B_range<-flow_data_compiled$temp_B_max-flow_data_compiled$temp_B_0
flow_data_compiled$C_range<-flow_data_compiled$temp_C_max-flow_data_compiled$temp_C_0
flow_data_compiled$D_range<-flow_data_compiled$temp_D_max-flow_data_compiled$temp_D_0
flow_data_compiled$E_range<-flow_data_compiled$temp_E_max-flow_data_compiled$temp_E_0
flow_data_compiled$F_range<-flow_data_compiled$temp_F_max-flow_data_compiled$temp_F_0

flow_data_compiled$AD_range<-flow_data_compiled$A_range-flow_data_compiled$D_range
flow_data_compiled$BE_range<-flow_data_compiled$B_range-flow_data_compiled$E_range
flow_data_compiled$CF_range<-flow_data_compiled$C_range-flow_data_compiled$F_range

ggplot(flow_data_compiled,aes(cycle,D_range,color=as.factor(flow)))+geom_point()


A_D_Angle <- 30 * pi / 180
B_E_Angle <- 90 * pi / 180
F_C_Angle <- 150 * pi / 180

flow_data_compiled$A_D_X <- flow_data_compiled$AD_range * sin(A_D_Angle)
flow_data_compiled$A_D_Z <- flow_data_compiled$AD_range * cos(A_D_Angle)
flow_data_compiled$B_E_X <- flow_data_compiled$BE_range * sin(B_E_Angle)
flow_data_compiled$B_E_Z <- flow_data_compiled$BE_range * cos(B_E_Angle)
flow_data_compiled$C_F_X <- flow_data_compiled$CF_range * sin(F_C_Angle)
flow_data_compiled$C_F_Z <- flow_data_compiled$CF_range * cos(F_C_Angle)

flow_data_compiled$total_x <- flow_data_compiled$A_D_X + flow_data_compiled$B_E_X + flow_data_compiled$C_F_X
flow_data_compiled$total_z <- flow_data_compiled$A_D_Z + flow_data_compiled$B_E_Z + flow_data_compiled$C_F_Z
flow_data_compiled$horiz_mag <- sqrt(flow_data_compiled$total_x^2 + flow_data_compiled$total_z^2)

flow_data_compiled<-subset(flow_data_compiled,horiz_mag<1.35)

flow_data_compiled0<-flow_data_compiled[-c(42,43,112,113,114,172,173,174),]

ggplot(flow_data_compiled0,aes(flow,horiz_mag,color=cycle))+geom_point()+geom_smooth(method="lm")

summary(lm(horiz_mag~flow,flow_data_compiled0))

flow_data_compiled0$pred<-predict(lm(flow~horiz_mag,flow_data_compiled0))

rmse <- sqrt(mean((flow_data_compiled0$flow - flow_data_compiled0$pred)^2, na.rm = TRUE))


summary(lm(horiz_mag.mean~flow,summaryBy(horiz_mag~flow,data=flow_data_compiled,FUN=c(mean))))

flow_data_compiled$calc_XZ_angle <- atan2(flow_data_compiled$total_x, flow_data_compiled$total_z) * 180 / pi



result <- flow_data_compiled %>%
  group_by(flow) %>%
  mutate(bin = (row_number() - 1) %/% 3) %>%
  group_by(flow, bin) %>%
  summarise(horiz_mag = mean(horiz_mag), .groups = "drop") %>%
  select(-bin)

  result<-as.data.frame(result)
  
  ggplot(result,aes(flow,horiz_mag))+geom_point()
  
  summary(lm(horiz_mag~flow,result))

cyc_melt<-melt(flow_data_compiled[,c("flow","cycle","AD_range","BE_range","CF_range")],id=c("cycle","flow"))

ggplot(cyc_melt,aes(flow,value,color=variable))+
geom_point()


flow_data_compiled$A2D2_difference_0<-flow_data_compiled$temp_A_max-flow_data_compiled$temp_A_min
flow_data_compiled$B2E2_difference_0<-flow_data_compiled$temp_B_0-flow_data_compiled$temp_E_0
flow_data_compiled$C2F2_difference_0<-flow_data_compiled$temp_C_0-flow_data_compiled$temp_F_0



flow_data_compiled$A2D2_difference_0<-flow_data_compiled$temp_A_0-flow_data_compiled$temp_D_0
flow_data_compiled$B2E2_difference_0<-flow_data_compiled$temp_B_0-flow_data_compiled$temp_E_0
flow_data_compiled$C2F2_difference_0<-flow_data_compiled$temp_C_0-flow_data_compiled$temp_F_0

flow_data_compiled$A2D2_difference_480<-flow_data_compiled$temp_A_480-flow_data_compiled$temp_D_480
flow_data_compiled$B2E2_difference_480<-flow_data_compiled$temp_B_480-flow_data_compiled$temp_E_480
flow_data_compiled$C2F2_difference_480<-flow_data_compiled$temp_C_480-flow_data_compiled$temp_F_480



# Horizontal
flow_data_compiled$A2D2_timediff <- flow_data_compiled$A2D2_difference_480 - flow_data_compiled$A2D2_difference_0
flow_data_compiled$B2E2_timediff <- flow_data_compiled$B2E2_difference_480 - flow_data_compiled$B2E2_difference_0
flow_data_compiled$C2F2_timediff <- flow_data_compiled$C2F2_difference_480 - flow_data_compiled$C2F2_difference_0

# =============================================================================
# XZ (horizontal) angle calculation — YOUR WORKING METHOD, unchanged
# =============================================================================

A_D_Angle <- 30 * pi / 180
B_E_Angle <- 90 * pi / 180
F_C_Angle <- 150 * pi / 180

flow_data_compiled$A_D_X <- flow_data_compiled$A2D2_timediff * sin(A_D_Angle)
flow_data_compiled$A_D_Z <- flow_data_compiled$A2D2_timediff * cos(A_D_Angle)
flow_data_compiled$B_E_X <- flow_data_compiled$B2E2_timediff * sin(B_E_Angle)
flow_data_compiled$B_E_Z <- flow_data_compiled$B2E2_timediff * cos(B_E_Angle)
flow_data_compiled$C_F_X <- flow_data_compiled$C2F2_timediff * sin(F_C_Angle)
flow_data_compiled$C_F_Z <- flow_data_compiled$C2F2_timediff * cos(F_C_Angle)

flow_data_compiled$total_x <- flow_data_compiled$A_D_X + flow_data_compiled$B_E_X + flow_data_compiled$C_F_X
flow_data_compiled$total_z <- flow_data_compiled$A_D_Z + flow_data_compiled$B_E_Z + flow_data_compiled$C_F_Z
flow_data_compiled$horiz_mag <- sqrt(flow_data_compiled$total_x^2 + flow_data_compiled$total_z^2)

flow_data_compiled$calc_XZ_angle <- atan2(flow_data_compiled$total_x, flow_data_compiled$total_z) * 180 / pi


ggplot(flow_data_compiled,aes(flow,horiz_mag))+
geom_point()+
geom_smooth(method="lm")


fd_sum<-summaryBy(horiz_mag~flow,flow_data_compiled,FUN=c(mean))


ggplot(fd_sum,aes(flow,horiz_mag.mean))+
geom_point()+
geom_smooth(method="lm")


summary(lm(horiz_mag.mean~flow,fd_sum))
summary(lm(horiz_mag~flow,flow_data_compiled))


# =============================================================================
# YZ (vertical) angle calculation — CORRECTED
# =============================================================================

# The vertical signal is simply the ring-average timediff.
# No sin(angle) projection is needed here because the rings are directly
# above/below the heater — the measurement axis IS the vertical axis.
timings_compiled$total_y <- timings_compiled$ring_vert_timediff

# For 3D magnitude, you need a calibration factor to put horizontal and
# vertical signals on the same scale. The horizontal signal comes from 
# sensors ~3.3 cm from the heater (in the same plane), while the vertical 
# signal comes from sensors ~4 cm above/below. The heat transport geometry 
# differs, so a scaling factor is needed.
#
# Approach: at a known velocity, the ratio of horiz_mag (at YZ=0) to 
# vert_signal (at YZ=90, XZ=0) gives the relative sensitivity.
# Then: total_mag = sqrt(horiz_mag^2 + (vert * scale_factor)^2)

# Estimate calibration from the simulation data:
# At V=5, YZ=0:  horiz_mag should represent full velocity
# At V=5, YZ=90: vert_signal should represent full velocity
horiz_ref <- timings_compiled$horiz_mag[timings_compiled$YZ_angles == 0 & 
                                         timings_compiled$XZ_angles == 0 & 
                                         timings_compiled$Velocity_ft == 5]
vert_ref  <- timings_compiled$total_y[timings_compiled$YZ_angles == 90 & 
                                       timings_compiled$XZ_angles == 0 & 
                                       timings_compiled$Velocity_ft == 5]

if (length(horiz_ref) > 0 & length(vert_ref) > 0) {
  vert_scale <- horiz_ref[1] / vert_ref[1]
  cat("Vertical calibration scale factor:", vert_scale, "\n")
  cat("  (horizontal signal at V=5, XZ=0, YZ=0):", horiz_ref[1], "\n")
  cat("  (vertical signal at V=5, XZ=0, YZ=90):", vert_ref[1], "\n")
} else {
  vert_scale <- 1  # fallback
}

timings_compiled$total_y_scaled <- timings_compiled$total_y * vert_scale
timings_compiled$total_3d_mag <- sqrt(timings_compiled$horiz_mag^2 + 
                                       timings_compiled$total_y_scaled^2)

# Recover YZ angle
timings_compiled$calc_YZ_angle <- atan2(timings_compiled$total_y_scaled, 
                                         timings_compiled$horiz_mag) * 180 / pi


# =============================================================================
# Diagnostic plots
# =============================================================================

# 1. Horizontal magnitude vs velocity (your working method)
p1 <- ggplot(subset(timings_compiled, YZ_angles == 0), 
             aes(as.numeric(Velocity_ft), horiz_mag, color = XZ_angles)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "XZ (Horizontal) Magnitude vs Velocity",
       subtitle = "Your existing method — works correctly",
       x = "Velocity (ft/day)", y = "Horizontal Magnitude")
print(p1)

# 2. Vertical signal vs YZ angle (CORRECTED ring average)
p2 <- ggplot(subset(timings_compiled, XZ_angles == 0), 
             aes(as.numeric(YZ_angles), ring_vert_timediff, 
                 color = Velocity_ft, group = Velocity_ft)) +
  geom_point(size = 4) +
  geom_line() +
  labs(title = "Vertical Signal vs YZ Angle (CORRECTED)",
       subtitle = "Ring-average method: mean(Ring1) - mean(Ring3)",
       x = "YZ Angle (degrees)", y = "Vertical Signal (ring avg timediff)")
print(p2)

# 3. Vertical signal is near-zero for purely horizontal flow
p3 <- ggplot(subset(timings_compiled, YZ_angles == 0), 
             aes(as.numeric(XZ_angles), ring_vert_timediff, 
                 color = Velocity_ft, group = Velocity_ft)) +
  geom_point(size = 4) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Vertical Signal for Horizontal-Only Flow (YZ=0°)",
       subtitle = "Should be ~0 regardless of XZ angle — confirms no contamination",
       x = "XZ Angle (degrees)", y = "Vertical Signal (ring avg timediff)")
print(p3)

# 4. 3D magnitude vs velocity
p4 <- ggplot(timings_compiled, 
             aes(as.numeric(Velocity_ft), total_3d_mag, color = YZ_angles)) +
  geom_point(size = 4) +
  labs(title = "Total 3D Magnitude vs Velocity",
       subtitle = "Combined horizontal + corrected vertical",
       x = "Velocity (ft/day)", y = "3D Magnitude")
print(p4)

# 5. Recovered YZ angle vs actual
p5 <- ggplot(subset(timings_compiled, XZ_angles == 0 & as.numeric(Velocity_ft) >= 1),
             aes(as.numeric(YZ_angles), calc_YZ_angle, 
                 color = Velocity_ft, group = Velocity_ft)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Recovered vs Actual YZ Angle",
       subtitle = "Points should fall on the 1:1 dashed line",
       x = "Actual YZ Angle (degrees)", y = "Recovered YZ Angle (degrees)") +
  coord_equal(xlim = c(-5, 95), ylim = c(-5, 95))
print(p5)
