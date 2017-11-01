source("packages/import_data.R")
#source("packages/subsample.R")
data = import_data("/Users/penghanqiu/Desktop/sharkduino_R_analysis/data/2017-8-22 7_51_51_21a _data.csv", legacy=F)
length_window = 5
n_windows= floor(nrow(data[,1])/length_window)
#ax=aggregate(data[1:(length_window*n_windows),1], data.table(rep(1:n_windows, each=length_window)), mean)[,2]
#ay=aggregate(data[1:(length_window*n_windows),2], data.table(rep(1:n_windows, each=length_window)), mean)[,2]
#az=aggregate(data[1:(length_window*n_windows),3], data.table(rep(1:n_windows, each=length_window)), mean)[,2]
#gx=aggregate(data[1:(length_window*n_windows),4], data.table(rep(1:n_windows, each=length_window)), mean)[,2]
#gy=aggregate(data[1:(length_window*n_windows),5], data.table(rep(1:n_windows, each=length_window)), mean)[,2]
#gz=aggregate(data[1:(length_window*n_windows),6], data.table(rep(1:n_windows, each=length_window)), mean)[,2]
#output=cbind(ax,ay,az,gx,gy,gz)
mean_data=aggregate(data[1:(length_window*n_windows),1:6], data.table(rep(1:n_windows, each=length_window)), mean)[,2:7]
