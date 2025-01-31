tr1<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Atmosphere Tests/tra-001_Jan14-Jan16th2025/0524824.CSV')
tr1$lag_stamp<-lag(tr1$timestamp,n=1)
tr1$interval<-tr1$timestamp-tr1$lag_stamp

ggplot(tr1,aes(timestamp,interval))+
geom_point()

