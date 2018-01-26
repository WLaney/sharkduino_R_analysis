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
require("viridis") # colors

source("packages/import_data.R")
source("packages/subsample.R")

# Import data
data = import_data("/Users/dara/GDrive - WM/Animal Tag Lab Book/Data/20170828-v2.1a-(Other shark)/csvs/data//2017-8-28 14/47/55_21a _data.csv", legacy=F)

ds = 6
sample.rate = 25
window.length = 30 * sample.rate

for (window.n in 100:120) {
  myWindow = 1:window.length + (window.n * window.length)
  # Fourier spectrum of z-axis accelerometer data
  myspec = spectrum(data[[ds]][myWindow], plot=F, method="pgram")


  x = as.numeric(myspec$freq * sample.rate)
  y = as.numeric(myspec$spec+.0001)
  
  # Identify peaks
  peaks = findpeaks(y, npeaks=20, sortstr=T)
  
  if (is.null(peaks)){
    print(paste("window", window.n,"is null"))
    specPlot = NULL
  } else {
    # Graph
    specPlot = qplot(x,y, geom="line", log="y") + 
      geom_point(aes(x=x[peaks[,2]], y = peaks[,1], col=log(peaks[,1])) ) +
      scale_x_continuous(breaks = 0:12) +
      background_grid(major = "xy", minor = "y") +
      scale_color_viridis()
  }
  
  basePlot = qplot(data[[7]][myWindow], data[[ds]][myWindow], geom="line") +
    scale_x_datetime(breaks = date_breaks("5 secs"), date_labels = "%H:%M:%S")
  windowPlot = plot_grid(specPlot, basePlot, ncol=1)
  ggsave(paste("plots/peaks_axis_", ds, "_window_", window.n, ".png", sep=""), windowPlot)
}