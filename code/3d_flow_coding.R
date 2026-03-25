library(dplyr)
library(doBy)
library(ggplot2)

simout<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Ansys_data&Analysis/ALL_angles_ALL_velocities_simulation_data_restructured_New.csv')

names(simout)[1:6]<-c("Time","Type","Velocity_ft","XZ_Angle","YZ_Angle","Cycle")

simout$A1A3<-simout$A1-simout$A3
simout$A1D3<-simout$A1-simout$D3
simout$B1E3<-simout$B1-simout$E3
simout$C1F3<-simout$C1-simout$F3
simout$A2D2<-simout$A2-simout$D2
simout$B2E2<-simout$B2-simout$E2
simout$C2F2<-simout$C2-simout$F2
simout$r1_mean<-simout$A1+simout$B1+simout$C1+simout$D1+simout$E1+simout$F1
simout$r2_mean<-simout$A2+simout$B2+simout$C2+simout$D2+simout$E2+simout$F2
simout$r3_mean<-simout$A3+simout$B3+simout$C3+simout$D3+simout$E3+simout$F3

simout$r1_r3_diff<-simout$r1_mean-simout$r3_mean

simout$XZ_Angle_fac<-as.factor(simout$XZ_Angle)
simout$YZ_Angle_fac<-as.factor(simout$YZ_Angle)
simout$Velocity_ft_fac<-as.factor(simout$Velocity_ft)

#Compare time 1 to time 480 (8 minutes)


#simout_max<-summaryBy(A2D2_Difference+A1D3_Difference+B2E2_Difference+C2F2_Difference~XZ_Angle_fac+simout$Velocity_ft_fac,data=simout,fun=c("which.min"))


simout_v15_yz30<-subset(simout,Velocity_ft==15 & XZ_Angle==0)

ggplot(simout_v15_yz30,aes(Time,A1A3,color=as.factor(YZ_Angle)))+
geom_point(size=2)

ggplot()+
	geom_line(data=simout_v15_yz30,aes(Time,A1,color=as.factor(YZ_Angle)),linetype="dashed",size=2)+
	geom_line(data=simout_v15_yz30,aes(Time,A3,color=as.factor(YZ_Angle)),size=2)




timings_compiled<-data.frame()
XZ_angles<-unique(simout$XZ_Angle_fac)
XZ_angles<-XZ_angles[!is.na(XZ_angles)]
YZ_angles<-unique(simout$YZ_Angle_fac)
YZ_angles<-YZ_angles[!is.na(YZ_angles)]
Velocity_ft<-unique(simout$Velocity_ft_fac)

for(i in XZ_angles){

	for(j in Velocity_ft){

			for(k in YZ_angles){
		simout_sub<-subset(simout,XZ_Angle_fac==i&Velocity_ft_fac==j&YZ_Angle_fac==k)
		if(nrow(simout_sub)>20){
		A2D2_max_difference<-simout_sub$Time[which.max(abs(simout_sub$A2D2))]
		B2E2_max_difference<-simout_sub$Time[which.max(abs(simout_sub$B2E2))]		
		C2F2_max_difference<-simout_sub$Time[which.max(abs(simout_sub$C2F2))]		
		A2D2_difference_1<-simout_sub$A2D2[simout_sub$Time==1]
		A2D2_difference_480<-simout_sub$A2D2[simout_sub$Time==480]		
		
		B2E2_difference_1<-simout_sub$B2E2[simout_sub$Time==1]
		B2E2_difference_480<-simout_sub$B2E2[simout_sub$Time==480]		
		
		C2F2_difference_1<-simout_sub$C2F2[simout_sub$Time==1]
		C2F2_difference_480<-simout_sub$C2F2[simout_sub$Time==480]		

		A1D3_difference_1<-simout_sub$A1D3[simout_sub$Time==1]
		A1D3_difference_480<-simout_sub$A1D3[simout_sub$Time==480]		

		B1E3_difference_1<-simout_sub$B1E3[simout_sub$Time==1]
		B1E3_difference_480<-simout_sub$B1E3[simout_sub$Time==480]		

		C1F3_difference_1<-simout_sub$C1F3[simout_sub$Time==1]
		C1F3_difference_480<-simout_sub$C1F3[simout_sub$Time==480]		


		
		timings_compiled<-bind_rows(timings_compiled,data.frame(XZ_angles=i,YZ_angles=k,Velocity_ft=j,A2D2_max_difference,B2E2_max_difference,C2F2_max_difference,A1D3_difference_1,A1D3_difference_480,A2D2_difference_1,A2D2_difference_480,B2E2_difference_1,B2E2_difference_480,C2F2_difference_1,C2F2_difference_480,B1E3_difference_1,B1E3_difference_480,C1F3_difference_1,C1F3_difference_480))
	}
	}
}
}






timings_compiled$B2E2_timediff <- timings_compiled$B2E2_difference_480-timings_compiled$B2E2_difference_1
timings_compiled$C2F2_timediff <- timings_compiled$C2F2_difference_480-timings_compiled$C2F2_difference_1
timings_compiled$A2D2_timediff <- timings_compiled$A2D2_difference_480-timings_compiled$A2D2_difference_1
#timings_compiled$A1A3_timediff <- timings_compiled$A1A3_difference_480-timings_compiled$A1A3_difference_1
timings_compiled$A1D3_timediff <- timings_compiled$A1D3_difference_480-timings_compiled$A1D3_difference_1

timings_compiled$B1E3_timediff <- timings_compiled$B1E3_difference_480-timings_compiled$B1E3_difference_1

timings_compiled$C1F3_timediff <- timings_compiled$C1F3_difference_480-timings_compiled$C1F3_difference_1




A_D_Angle <- 30* pi / 180
timings_compiled$A_D_X<-(timings_compiled$A2D2_timediff)*sin(A_D_Angle)
timings_compiled$A_D_Z<-(timings_compiled$A2D2_timediff)*cos(A_D_Angle)


B_E_Angle <- 90 * pi / 180
timings_compiled$B_E_X<-(timings_compiled$B2E2_timediff)*sin(B_E_Angle)
timings_compiled$B_E_Z<-(timings_compiled$B2E2_timediff)*cos(B_E_Angle)


F_C_Angle <- 150 * pi / 180
timings_compiled$C_F_X<-(timings_compiled$C2F2_timediff)*sin(F_C_Angle)
timings_compiled$C_F_Z<-(timings_compiled$C2F2_timediff)*cos(F_C_Angle)


timings_compiled$total_x<-timings_compiled$C_F_X+timings_compiled$B_E_X+timings_compiled$A_D_X
timings_compiled$total_z<-timings_compiled$C_F_Z+timings_compiled$B_E_Z+timings_compiled$A_D_Z

timings_compiled$total_mag<-sqrt((timings_compiled$total_x^2)+(timings_compiled$total_z^2))

tc_limit<-subset(timings_compiled,YZ_angles==0)
ggplot(tc_limit,aes(as.numeric(Velocity_ft),total_mag,group=XZ_angles,color=XZ_angles))+
geom_point(aes())+
geom_smooth(method="lm",se=FALSE)


#Y Angle Calcs

timings_compiled$A1_D3_Y<-timings_compiled$A1D3_timediff*sin(57.68)
timings_compiled$B1_E3_Y<-timings_compiled$B1E3_timediff*sin(57.68)
timings_compiled$C1_F3_Y<-timings_compiled$C1F3_timediff*sin(57.68)

timings_compiled$Y_sum<-timings_compiled$A1_D3_Y + timings_compiled$B1_E3_Y + timings_compiled$C1_F3_Y

ggplot(timings_compiled,aes(YZ_angles,Y_sum,color=Velocity_ft,group=Velocity_ft))+
geom_smooth(method="lm")+
geom_point(size=10)


ggplot(timings_compiled,aes(YZ_angles,B1_E3_Y,color=Velocity_ft,group=Velocity_ft))+
geom_smooth(method="lm")+
geom_point(size=10)



ggplot(timings_compiled,aes(YZ_angles,C1_F3_Y,color=Velocity_ft,group=Velocity_ft))+
geom_smooth(method="lm")+
geom_point(size=10)






timings_compiled$A1_A3_Y<-timings_compiled$A1A3_timediff

#7.102


#Degrees = cos(6/7.102) * (180/pi)
#cos(3.8/7.102) * (180/pi)

timings_compiled$total_z_mag<-timings_compiled$C_F_Z+timings_compiled$B_E_Z+timings_compiled$A_D_Z
timings_compiled$total_x_mag<-timings_compiled$C_F_X+timings_compiled$B_E_X+timings_compiled$A_D_X
timings_compiled$total_y_mag<-timings_compiled$A1_A3_Y
timings_compiled$total_mag<-sqrt((timings_compiled$total_z_mag/3.312)^2+(timings_compiled$total_x_mag/3.312)^2+((timings_compiled$total_y_mag/6)*3)^2)

ggplot(timings_compiled,aes(as.numeric(Velocity_ft),total_mag))+
geom_point(size=10,aes(color=YZ_angles))


ggplot(subset(timings_compiled,Velocity_ft==5),aes(as.numeric(YZ_angles),A1_A3_Z))+
geom_point(size=10,aes(color=XZ_angles))

ggplot(subset(timings_compiled,XZ_angles==0),aes(as.numeric(Velocity_ft),B2E2_timediff))+
geom_point(size=10,aes(color=XZ_angles))

timings_compiled[order(timings_compiled$XZ_angles),]


simout_0<-subset(simout,XZ_Angle==0&Velocity_ft==5)


ggplot()+
geom_point(data=simout_0,aes(Time,A2),color="green")+
geom_point(data=simout_0,aes(Time,A1),color="brown")



geom_vline(xintercept=240,color="red")+
geom_vline(xintercept=840,color="green")+
geom_vline(xintercept=1440,color="blue")
	
#	, linetype, color, linewidth) 


