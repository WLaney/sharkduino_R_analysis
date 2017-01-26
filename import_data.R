#import data file and creat accel, gyro, date_time, temp, and pressure arrays
raw_data<-read.csv("test-data.csv", header=TRUE, sep=",") #import data

#creat seprate data frames for accel, gyro, data/time, pressure, and temp
#Is this nessary/do I realy want to do this?
accel<-raw_data[,c("ax","ay","az")]
gyro<-raw_data[,c("gx","gy","gz")]
date_time<-as.vector(raw_data$date_time, )
temp<-raw_data["temp"]
pressure<-raw_data["pressure"]
rm(raw_data) #remove raw data to save memory

#Datetime interpolation
#remove \t from date time data and convert to times
date_time<-gsub("\t"," ", date_time, fixed=T)
date_time<-strptime(date_time, format="%F %T") #times exptected in ISO 8691 fromate

#interpolate the data time between RTC writes
inds<-which(!is.na(date_time)) #find index of date/time values

#creat data frame with just enivormental data and time stamps (no NAs)
date_time_env<-date_time[inds[-1]] #the first reading is just the RTC, no real data
pressure<-pressure[inds[-1],]
temp<-temp[inds[-1],]
enivroment_data<-data.frame(pressure, temp, date_time_env)

#calulate times between recorded times, assumes that times are linarly spaced
for (n in 1:(length(inds)-1)){
	i_old<-inds[n]
	i_new<-inds[n+1]
	t_old<-date_time[i_old]
	t_new<-date_time[i_new]
	date_time[i_old:(i_new-1)]<-seq(t_old, t_new, i_new-i_old)
}


data_end<-!is.na(date_time) #find the data without times
#remove data without times
date_time<-date_time[data_end]
accel<-accel[data_end,]
gyro<-gyro[data_end,]

#remove rows without data
not_data<-!is.na(accel$ax)
accel<-accel[not_data,]
gyro<-gyro[not_data,]
date_time<-date_time[not_data]

movement_data<-data.frame(accel, gyro, date_time)
save(movement_data, enivroment_data, file="importated.RData")
