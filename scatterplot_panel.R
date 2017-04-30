# imports
require("data.table") # Faster data import and manipulation
require("fasttime") # Faster data import and manipulation
require("ggplot2") # for pretty plots
require("scales") # for better scales
require("cowplot") # for arranging plots in grids

source(".Rprofile")
source("packages/import_data.R")
source("packages/subsample.R")

# Read in interpolated CSV. Place your interpolated CSV file in the data subdirectory,
# then edit the following line to reference it.
data = import_data("data/tmp-data.csv", legacy=F)


# this function lets you sample a vector obj at every nth point, 
# so long graphs don't take forever to render. 
# Make sure to update your sampling rate if you're using subsampling!
subsample = ss

# Function for making our plots (with lapply)
makeScatterPane = function(ds, data, datasetName = "NO NAME", dataRange = 1:nrow(data), ssres = 1) {
  # Titles/labels/limits for the various plots
  titles = c(
    paste("Accelerometer X Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Accelerometer Y Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Accelerometer Z Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Gyro X Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Gyro Y Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Gyro Z Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep="")
  )
  
  ylabs = c(
    "Accelerometer X-Axis (Gs)",
    "Accelerometer Y-Axis (Gs)",
    "Accelerometer Z-Axis (Gs)",
    expression("Gyro X-Axis ("*degree*"/s)"),
    expression("Gyro Y-Axis ("*degree*"/s)"),
    expression("Gyro Z-Axis ("*degree*"/s)")
  )
  
  myLims = list(
    c(-1.2, -0.8),
    c(-0.1,  0.3),
    c(-0.4, -0.0),
    c(-100,  100),
    c(-100,  100),
    c(-100,  100)
  )
  
  # Make plot with GGPlot2
  myPlot <<- ggplot(
    data.frame(
      dates = subsample(data[dataRange][[7]], ssres),
      series = subsample(data[dataRange][[ds]], ssres)
    )) +
    geom_line(aes(x=dates, y=series)) +
    labs(
      x="Time (hh:mm)", 
      y=ylabs[ds], 
      title=titles[ds] 
    ) + 
    background_grid(major = 'xy', minor = "none") + 
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    ylim(myLims[[ds]])
  
  return(myPlot)
}

## Array of scatterplots for a given dataset
# Name of dataset (for plot titles)
head.datasetName = "07/29 Sandbar (Scratch)"
# Which points to plot?
head.dataRange = 810000:811500
# subsampling resolution?
head.ssres = 1

# Base list for plots
plots = as.list(1:6)

plots = lapply(
  1:6, 
  makeScatterPane, 
  data = head.data, 
  datasetName = head.datasetName, 
  dataRange = head.dataRange, 
  ssres = head.ssres
)


filename = gsub("/", "", gsub(" ", "_", paste(head.datasetName, " ScatterPanel_window_cow.png", sep="")))

# output to PDF
#png(filename, height=1920, width=1080)
plotPanel = plot_grid(
  plots[[1]],
  plots[[2]],
  plots[[3]], 
  plots[[4]],
  plots[[5]],
  plots[[6]],labels = "AUTO", ncol = 1, align = "v")

#dev.off()

save_plot(paste("plots/", filename, sep = ""), plotPanel, ncol=1, nrow=6, base_aspect_ratio = 2, base_height = 3)

