# Tailbeat frequency method #1: 
# Remove DC from Accel Z, take the jerk, and count zero crossings
# By Dara Kharabi, for Sharkduino

# imports
require("ggplot2") # for pretty plots
require("scales") # for date formatting
require("cowplot") # for prettier plots
require("signal") # signal processing toolkit w/filters, etc
require("RcppRoll") # for faster rolling sums

source("packages/import_data.R")
source("packages/subsample.R")

# Import data
data = import_data("data/tmp-data.csv", legacy=F)

windowSize = 25*600
azAvg = mean(data[[3]])
az.ctr = diff(data[[3]] - azAvg)
az.ctr[abs(az.ctr) < .025] = 0

# 3rd order Butterworth LPF
myLPF = butter(type="low", 3, 4/12.5)
# Filter data
az.flt = filter(myLPF, az.ctr)

# Count zero crossings
az.sign = c(0, diff(sign(az.flt)))/2
# Get average count/2 per second for window (hopefully tailbeat freq.)
az.beats = roll_sum(abs(az.sign), windowSize)/2/windowSize*25

roll.range = 1:(nrow(data) - windowSize)

# Plot
x = ss.simple(data[[7]][roll.range], 10)
y = ss(az.beats[roll.range],10)
qplot(x, y, geom="line") + scale_x_datetime(breaks = date_breaks("1 hours"), date_labels = "%H")