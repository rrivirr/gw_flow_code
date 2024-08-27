
library(stringr)
library(readr)
library(reshape2)
#library(tidyverse)
library(dplyr)
library(data.table)

f0.5md_directory<-"~/Desktop/ansys_simu/0.5m-day(5.79e-6)"
f1md_directory<- "/home/hosen-lab/Desktop/ansys_simu/1m-day(1.16e-5)"
f2md_directory<- "/home/hosen-lab/Desktop/ansys_simu/2m-day(2.31e-5)"


flowVector <- function(directory){
setwd(directory)
f0.5md_files<-list.files(directory)

f0.5md<-data.frame()

for(i in 1:length(f0.5md_files)){
	f0.5md_temp<-as.data.frame(read_delim(f0.5md_files[i], id = "name",skip=3,col_names=FALSE,delim=" "))
	f0.5md_temp$sensor_location <- str_extract(f0.5md_temp[4,1], "(?<=-).+?(?=-)")
	names(f0.5md_temp)<-c("filename","timestep","temp_k","timestep_2","sensor_location")	
	f0.5md<-bind_rows(f0.5md,f0.5md_temp)
}

f0.5md_cast<-dcast(f0.5md[,c("timestep","temp_k","sensor_location")],timestep~sensor_location,value.var="temp_k", fun.aggregate=mean)


# heater is on for 30 seconds and then heater is off for 4 minutes (240 seconds)
#f minus a
#e minus b
#c minus d

f0.5md_cast$f_minus_a<-f0.5md_cast$f-f0.5md_cast$a
f0.5md_cast$e_minus_b<-f0.5md_cast$e-f0.5md_cast$b
f0.5md_cast$c_minus_d<-f0.5md_cast$c-f0.5md_cast$d

initial_temperature<-f0.5md_cast[f0.5md_cast$timestep==30,]
final_temperature<-f0.5md_cast[f0.5md_cast$timestep==270,]

temp_difference<-final_temperature-initial_temperature
temp_difference$f_minus_a_x_dimension<-(temp_difference$f_minus_a/2)*sqrt(3)
temp_difference$f_minus_a_y_dimension<--temp_difference$f_minus_a/2

temp_difference$e_minus_b_x_dimension<-(temp_difference$e_minus_b/2)*sqrt(3)
temp_difference$e_minus_b_y_dimension<-temp_difference$e_minus_b/2

temp_difference$c_minus_d_x_dimension<-(temp_difference$c_minus_d/2)*sqrt(3)
temp_difference$c_minus_d_y_dimension<-temp_difference$c_minus_d/2

x_vector_sum<-temp_difference$c_minus_d_x_dimension+temp_difference$e_minus_b_x_dimension+temp_difference$f_minus_a_x_dimension

y_vector_sum<-temp_difference$c_minus_d_y_dimension+temp_difference$e_minus_b_y_dimension+temp_difference$f_minus_a_y_dimension
flow_output <- list(x_vector_sum = x_vector_sum, y_vector_sum = y_vector_sum)
return(flow_output)
}

flowVector(f0.5md_directory)
flow0.5 <- flowVector(f0.5md_directory)
flow1.0 <- flowVector(f1md_directory)
flow2.0 <- flowVector(f2md_directory)



flow0.5_df<-as.data.frame(flow0.5)
flow0.5_df$flow_md<-0.5

flow1.0_df<-as.data.frame(flow1.0)
flow1.0_df$flow_md<-1.0

flow2.0_df<-as.data.frame(flow2.0)
flow2.0_df$flow_md<-2.0

flowCompiled<-bind_rows(flow0.5_df, flow1.0_df, flow2.0_df)

flowCompiled$Magnitude <- sqrt(flowCompiled$x_vector_sum^2 + flowCompiled$y_vector_sum^2)

plot(flowCompiled$flow_md, flowCompiled$x_vector_sum)
