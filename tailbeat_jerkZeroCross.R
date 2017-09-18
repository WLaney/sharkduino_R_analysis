# Tailbeat frequency: 3 ways
# By Dara Kharabi, for Sharkduino

# imports
require("ggplot2") # for pretty plots
require("scales") # for date formatting
require("cowplot") # for prettier plots
require("zoo") #for rolling sums
require("signal") # signal processing toolkit w/filters, etc
require("RcppRoll") # for faster rolling sums


source("packages/import_data.R")
source("packages/subsample.R")

# Import data
data = import_data("data/tmp-data.csv", legacy=F)

# 1: Remove DC from Accel Z and count zero crossings
windowSize = 25*180
azAvg = mean(data[[3]])
az.ctr = diff(data[[3]] - azAvg)
az.ctr[abs(az.ctr) < .025] = 0

# 3rd order Butterworth LPF
myLPF = butter(type="low", 3, 4/12.5)
# Filter data
az.flt = filter(myLPF, az.ctr)



az.sign = c(0, diff(sign(az.flt)))/2
az.beats = roll_sum(abs(az.sign), windowSize)/2/windowSize*25
rollSize = floor(windowSize/2)

x = ss.simple(data[[7]][rollSize:(nrow(data)-rollSize)][1:370000], 10)
y = ss(az.beats[1:370000],10)
qplot(x, y, geom="line") + scale_x_datetime(breaks = date_breaks("30 mins"), labels = date_format("%H:%M"))