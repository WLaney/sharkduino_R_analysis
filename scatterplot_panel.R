# imports
require("ggplot2") # for pretty plots
require("scales") # for better scales
require("cowplot") # for arranging plots in grids

source("packages/import_data.R")
source("packages/subsample.R")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
# Path to CSV file (change "data/myData.csv" to point to where your data is)
data = import_data("data/myData.csv", legacy=F)

# Name of dataset (for plot titles)
head.datasetName = "Enter dataset name here"

# Range of points to plot
head.dataRange = 1:nrow(data)

# subsampling resolution
head.ssres = 10
# ------------------------------------------------------------------------------


# this function lets you sample a vector obj at every nth point, 
# so long graphs don't take forever to render. 
subsample = ss.simple

# Function for making our plots (with lapply)
makeScatterPane = function(ds, data, datasetName = "NO NAME", dataRange = 1:nrow(data), ssres = 1) {
  # Titles/labels/limits for the various plots
  titles = c(
    paste("Accelerometer X Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Accelerometer Y Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Accelerometer Z Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Gyro X Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Gyro Y Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("Gyro Z Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep=""),
    paste("ODBA Data for ", datasetName, " Dataset (SS: 1/", ssres, ")", sep="")
  )
  
  ylabs = c(
    "Accel. X-Axis (Gs)",
    "Accel. Y-Axis (Gs)",
    "Accel. Z-Axis (Gs)",
    expression("Gyro X-Axis ("*degree*"/s)"),
    expression("Gyro Y-Axis ("*degree*"/s)"),
    expression("Gyro Z-Axis ("*degree*"/s)"),
    "ODBA (Gs)"
  )
  
  myLims = list(
    c(NA, NA),
    c(NA, NA),
    c(NA, NA),
    c(NA, NA),
    c(NA, NA),
    c(NA, NA),
    c(NA, NA)
  )
  
  if (ds <= 6) {
     ypts = data[dataRange][[ds]]
  } else if (ds == 7) {
    ypts = data[dataRange][[1]] + data[dataRange][[2]] + data[dataRange][[3]]
  } else {
    stop(paste("Bad Data Series value:", ds, "- needs a value 1-7."))
  }
  
  # Make plot with GGPlot2
  myPlot <<- ggplot(
    data.frame(
      dates = subsample(data[dataRange][[7]], ssres),
      series = ss(ypts, ssres)
    )) +
    geom_line(aes(x=dates, y=series), size=.2) +
    labs(
      x="Time (hh:mm)", 
      y=ylabs[ds], 
      title=titles[ds] 
    ) + 
    background_grid(major = 'xy', minor = "none") + 
    scale_x_datetime(breaks = date_breaks("5 mins"), labels = date_format("%H:%M"), expand=c(0,0)) + 
    scale_y_continuous(limits = c(mean(ypts)-10*sd(ypts), mean(ypts)+10*sd(ypts)), expand=c(-0.1,0)) + 
    theme(axis.text=element_text(size=9), axis.title=element_text(size=12,face="bold"))
  
  return(myPlot)
}



# make list of plots
plots = lapply(
  1:7, 
  makeScatterPane, 
  data = data, 
  datasetName = head.datasetName, 
  dataRange = head.dataRange, 
  ssres = head.ssres
)

filename = gsub("/", "", gsub(" ", "_", paste(head.datasetName, "_summary", sep="")))

axes = c(
  "ax",
  "ay",
  "az",
  "gx",
  "gy",
  "gz",
  "ODBA"
)

for (i in 1:7) {
  ggsave(
    paste("plots/", filename, "_", axes[i], ".png", sep = ""),
    plots[[i]], 
    dpi= 240,
    width=nrow(data)/25/300,
    height=2.5,
    limitsize = FALSE
  )
}
