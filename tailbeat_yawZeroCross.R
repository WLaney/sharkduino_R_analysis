# Tailbeat frequency method #3: 
# Take the first derivative of the yaw, then count zero-crossings
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
eas = fread("data/tmp-eas.csv", sep=",", header=TRUE)

windowSize = 600*25

yaw = eas[[3]]
dyaw=diff(yaw)

# Make the yaw continuous, instead of looping over [-pi, pi]
cyaw = yaw
pi.offset = 0
for (i in 2:length(yaw)) {
  if (dyaw[i-1] <= -pi/2) {
    # overflow = real big negative -> add pi rad
    pi.offset = pi.offset + 1
  } else if (dyaw[i-1] >= pi/2) {
    # underflow = real big positive -> subtract pi rad
    pi.offset = pi.offset - 1
  }
  cyaw[i] = yaw[i] + (pi.offset * 2*pi)
}

# 3rd order Butterworth filters
myLPF = butter(type="low", 3, 4/12.5)
myHPF = butter(type="high", 3, 0.1/12.5)
# Filter data
fcyaw = filter(myLPF, cyaw)
fcyaw = filter(myHPF, fcyaw)

# Count zero crossings
yaw.sign = c(0, diff(sign(diff(fcyaw))))/2
# Get average count/2 per second for window (hopefully tailbeat freq.)
yaw.beats = roll_mean(abs(yaw.sign)/2, windowSize)*25
yaw.beats = yaw.beats[2:length(yaw.beats)]

roll.range = 1:(nrow(data) - windowSize)

# Plot
x = ss.simple(data[[7]][roll.range], 10)
y = ss(yaw.beats[roll.range],10)
qplot(x, y, geom="line") + scale_x_datetime(breaks = date_breaks("1 hours"), date_labels = "%H")