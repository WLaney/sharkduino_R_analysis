# Tailbeat frequency: 3 ways
# By Abby Bilenkin, for Sharkduino

# imports
require("ggplot2") # for pretty plots
require("scales") # for date formatting
require("cowplot") # for prettier plots
require("zoo") #for rolling sums
require("signal") # signal processing toolkit w/filters, etc
require("RcppRoll") # for faster rolling sums
require("pracma") #for finding peaks

source("packages/import_data.R")
source("packages/subsample.R")

# Import data
myData = import_data("data.csv", legacy=F)

# Fourier transform of z-axis accelerometer data
fourier = abs(fft(myData[[3]][1000:2000]))^2

# Testing this with a sine wave
# values of two different sines at two different freqencies
# fourier = abs(fft(sin(10 * (2 * pi) * x)))^2

# Identify peaks above 0.25 Hz
peaks = findpeaks(fourier, minpeakheight = 0.25, threshold = 0, npeaks = 0, sortstr = FALSE)

# Graph
qplot(0:499, (fourier)[1:500], geom="line", log = "y")
#spectrum(fourier)
