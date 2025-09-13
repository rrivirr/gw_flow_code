library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)
library(stringr)

t60s_180ml<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/rriv-data_latest_lab_data/180ml/60s-20m/9442372.CSV')
t60s_180ml_2<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/rriv-data_latest_lab_data/180ml/60s-20m/9442822.CSV')



t60s_180ml_c<-bind_rows(t60s_180ml,t60s_180ml_2)


t60s_180ml_c$RING_1_5<-t60s_180ml_c$RING01_Tra.1-t60s_180ml_c$RING01_Tra.5
t60s_180ml_c$RING_2_4<-t60s_180ml_c$RING01_Tra.2-t60s_180ml_c$RING01_Tra.4

ggplot(t60s_180ml_c,aes(as_datetime(timestamp),RING_2_4))+
geom_point()



t60s_180ml_m<-melt(t60s_180ml_c[,c(1,2,4,6,10,12)],id=c("timestamp"))


ggplot(t60s_180ml_m,aes(timestamp,value,color=variable))+
geom_point()+
facet_wrap(.~variable)



t60s_265ml<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/rriv-data_latest_lab_data/265ml/60sec/9438225.CSV')

t30s_265ml<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/rriv-data_latest_lab_data/265ml/30sec/9442373.CSV')

t265ml<-bind_rows(t60s_265ml,t30s_265ml)

t265ml$RING_1_5<-t265ml$RING01_Tra.1-t265ml$RING01_Tra.5

ggplot(t265ml,aes(timestamp,RING01_Tra.1))+
geom_point()


t60s_265ml$RING_1_5<-t60s_265ml$RING01_Tra.1-t60s_265ml$RING01_Tra.5
t60s_265ml$RING_2_4<-t60s_265ml$RING01_Tra.2-t60s_265ml$RING01_Tra.4

ggplot(t60s_265ml,aes(timestamp,RING_2_4))+
geom_point()










library(dplyr)
library(purrr)
library(readr)

combine_csvs <- function(directory_path) {
  csv_files <- list.files(path = directory_path, 
                         pattern = "\\.csv$", 
                         recursive = TRUE, 
                         full.names = TRUE,
                         ignore.case = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified directory")
  }
  
  # Set names to be the basenames so .id uses actual filenames
  names(csv_files) <- basename(csv_files)
  
  # Read and combine all CSV files
  combined_df <- map_dfr(csv_files, read_csv, .id = "source_file")
  
  # Add source folder column
  file_paths <- csv_files[combined_df$source_file]
  combined_df$source_folder <- dirname(file_paths)
  
  return(combined_df)
}


cf<-as.data.frame(combine_csvs('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/rriv-data_latest_lab_data/First-test-data_july25th'))




cf$SA<-cf$RING01_Tra...2
cf$SD<-cf$RING01_Tra...4
cf$SE<-cf$RING01_Tra...6
cf$SF<-cf$RING01_Tra...8
cf$SB<-cf$RING01_Tra...10
cf$SC<-cf$RING01_Tra...12


cf2<-cf[,c("timestamp","SA","SD","SE","SF","SB","SC")]


cf2$SF<-cf2$SC


A_D<-cf2$SA-cf2$SD
E_B<-cf2$SB-cf2$SE
C_F<-cf2$SC-cf2$SF

tdif<-data.frame(timestamp=cf2$timestamp,A_D=A_D,E_B=E_B,C_F=C_F)

tdif_s<-subset(tdif,timestamp>1753473650&timestamp<1753475000)

tdifsm<-melt(tdif_s,id=c("timestamp"))

ggplot(tdifsm,aes(timestamp,value,color=variable))+
geom_point()

cf_t1<-subset(cf2,timestamp>1753473650&timestamp<1753475000)

cf_t1_m<-melt(cf_t1,id=c("timestamp"))

cf_t1_m$datetime<-as_datetime(cf_t1_m$timestamp)

ggplot(cf_t1,aes(timestamp,SA))+
geom_point()

cf_t1_m$value<-as.numeric(cf_t1_m$value)
ggplot(cf_t1_m,aes(datetime,value,color=variable))+
geom_point()+
xlab("")+
ylab("Temperature (°C)")




pattern <- "\\d{3}ml"
cf$flow_rate <- str_extract(cf$source_folder, pattern)

pattern <- "\\d{2}s"
cf$time <- str_extract(cf$source_folder, pattern)



ggplot(cf,aes(timestamp,RING01_Tra...2))+
geom_point()+
facet_wrap(.~flow_rate,scales="free_x")


t180_30<-subset(cf,flow_rate=="180ml" & time=="30s")
t180_60<-subset(cf,flow_rate=="180ml" & time=="60s")


ggplot(cf,aes(timestamp,RING01_Tra...2))+
geom_point()

cf$SA<-cf$RING01_Tra...2
cf$SD<-cf$RING01_Tra...4
cf$SE<-cf$RING01_Tra...6
cf$SF<-cf$RING01_Tra...8
cf$SB<-cf$RING01_Tra...10
cf$SC<-cf$RING01_Tra...12


cf2<-cf[,c("timestamp","SA","SD","SE","SF","SB","SC","time","flow_rate")]


cf2$Diff_AD<-cf2$SA-cf2$SD
cf2$Diff_BE<-cf2$SB-cf2$SE
#cf2$Diff_CF<-cf2$SC-cf2$SF


t180_30<-subset(cf2,flow_rate=="180ml" & time=="30s")
t180_60<-subset(cf2,flow_rate=="180ml" & time=="60s")



ggplot(t180_30,aes(timestamp,Diff_AD))+
geom_point()

ggplot(t180_30,aes(timestamp,Diff_BE))+
geom_point()

ggplot(t180_60,aes(timestamp,Diff_AD))+
geom_point()

ggplot(t180_60,aes(timestamp,Diff_BE))+
geom_point()







t265_30<-subset(cf2,flow_rate=="265ml" & time=="30s")
t265_60<-subset(cf2,flow_rate=="265ml" & time=="60s")



ggplot(t265_30,aes(timestamp,SB))+
geom_point()

ggplot(t265_30,aes(timestamp,Diff_BE))+
geom_point()

ggplot(t265_60,aes(timestamp,Diff_AD))+
geom_point()

ggplot(t265_60,aes(timestamp,Diff_BE))+
geom_point()






t350_30<-subset(cf2,flow_rate=="350ml" & time=="30s")
t350_60<-subset(cf2,flow_rate=="350ml" & time=="60s")



ggplot(t350_30,aes(timestamp,Diff_AD))+
geom_point()

ggplot(t350_30,aes(timestamp,Diff_BE))+
geom_point()

ggplot(t350_60,aes(timestamp,Diff_AD))+
geom_point()

ggplot(t350_60,aes(timestamp,Diff_BE))+
geom_point()


t350_60_m<-melt(t350_60[,c(1,2,3,4,6,7)],id=c("timestamp"))

ggplot(t350_60_m,aes(timestamp,value,color=variable))+
geom_point()

start<-range(t350_60$timestamp)[1]+120
end<-range(t350_60$timestamp)[1]+1320




directory_path<-'/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/rriv-data_latest_lab_data/First-test-data_july25th'

  csv_files <- list.files(path = directory_path, 
                         pattern = "\\.csv$", 
                         recursive = TRUE, 
                         full.names = TRUE)
  