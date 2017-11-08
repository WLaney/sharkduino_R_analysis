source("packages/import_data.R")
library(ggplot2)
require("cowplot")

data = import_data("/Users/penghanqiu/Desktop/sharkduino_R_analysis/data/2017-8-22 7_51_51_21a _data.csv", legacy=F)
length_window = 10
n_windows= floor(nrow(data[,1])/length_window)

separator=rep(1:n_windows, each=length_window)
rest_rows=rep(n_windows+1,times=nrow(data)-length_window*n_windows)
window_length=c(rep(length_window,times=n_windows),nrow(data)-length_window*n_windows)

mean_data=aggregate(data[,1:6], data.table(c(separator,rest_rows)), mean)[,2:7]
mean_data_with_window=cbind(mean_data,window_length)

min_data=aggregate(data[,1:6], data.table(c(separator,rest_rows)), min)[,2:7]
min_data_with_window=cbind(min_data,window_length)

max_data=aggregate(data[,1:6], data.table(c(separator,rest_rows)), max)[,2:7]
max_data_with_window=cbind(max_data,window_length)

var_data=aggregate(data[,1:6], data.table(c(separator,rest_rows)), var)[,2:7]
var_data_with_window=cbind(var_data,window_length)

range_data=max_data-min_data
range_data_with_window=cbind(range_data,window_length)

root_means_quare <- function(num) sqrt(sum(num^2)/length(num)); 
rms_data=aggregate(data[,1:6], data.table(c(separator,rest_rows)), root_means_quare)[,2:7]
rms_data_with_window=cbind(rms_data,window_length)

png('/Users/penghanqiu/Desktop/Rplot_mean//rplot_correlation.png', width = 2000, height = 2000, res = 200)
plot(mean_data)
dev.off()

# expression("Gyro X-Axis ("*degree*"/s)")
#"acceleration X (Gs)"
rplot_ax=qplot(1:nrow(mean_data_with_window), mean_data_with_window[,1], geom="line") + ylab("acceleration X (Gs)") + xlab("row number") 
ggsave(
  "/Users/penghanqiu/Desktop/Rplot_mean//rplot_ax.png",
  rplot_ax
)

rplot_ay=qplot(1:nrow(mean_data_with_window), mean_data_with_window[,2], geom="line") + ylab("acceleration Y (Gs)") + xlab("row number") 
ggsave(
  "/Users/penghanqiu/Desktop/Rplot_mean//rplot_ay.png",
  rplot_ay
)

rplot_az=qplot(1:nrow(mean_data_with_window), mean_data_with_window[,3], geom="line") + ylab("acceleration Z (Gs)") + xlab("row number") 
ggsave(
  "/Users/penghanqiu/Desktop/Rplot_mean//rplot_az.png",
  rplot_az
)

rplot_gx=qplot(1:nrow(mean_data_with_window), mean_data_with_window[,4], geom="line") + ylab(expression("Gyro X-Axis ("*degree*"/s)")) + xlab("row number") 
ggsave(
  "/Users/penghanqiu/Desktop/Rplot_mean//rplot_gx.png",
  rplot_gx
)

rplot_gy=qplot(1:nrow(mean_data_with_window), mean_data_with_window[,5], geom="line") + ylab(expression("Gyro Y-Axis ("*degree*"/s)")) + xlab("row number") 
ggsave(
  "/Users/penghanqiu/Desktop/Rplot_mean//rplot_gy.png",
  rplot_gy
)

rplot_gz=qplot(1:nrow(mean_data_with_window), mean_data_with_window[,6], geom="line") + ylab(expression("Gyro Z-Axis ("*degree*"/s)")) + xlab("row number") 
ggsave(
  "/Users/penghanqiu/Desktop/Rplot_mean//rplot_gz.png",
  rplot_gz
)