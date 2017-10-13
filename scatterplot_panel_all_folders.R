# imports
require("ggplot2") # for pretty plots
require("scales") # for better scales
require("cowplot") # for arranging plots in grids
require("RcppRoll")

source("packages/import_data.R")
source("packages/combine_csvs.R")
source("packages/subsample.R")

# I've created a folder "my_files" that is...
base.dir = "/Users/penghanqiu/Google Drive/Data"

# populated by three subfolders
dataset.dirs <- list.dirs(path=base.dir, full.names = TRUE)

sapply(dataset.dirs, make.summary.plots)


make.summary.plots(data.dir) {
  csv.name =
  combine.csvs(
    path = paste(data.dir, "/csvs/data", sep=""),
    out.path = paste(data.dir, "/", csv.name, sep="")
  
}
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
    plot.ylim = c(mean(ypts)-10*sd(ypts), mean(ypts)+10*sd(ypts))
  } else if (ds == 7) {
    win.length = 50 # rolling mean window length
    rollx = roll_mean(data[dataRange][[1]], n = win.length, fill = c(0))
    rolly = roll_mean(data[dataRange][[2]], n = win.length, fill = c(0))
    rollz = roll_mean(data[dataRange][[3]], n = win.length, fill = c(0))
    abs.dyn.a = abs(data[,1:3] - data.table(rollx, rolly, rollz))
    
    ypts = abs.dyn.a[dataRange][[1]] + abs.dyn.a[dataRange][[2]] + abs.dyn.a[dataRange][[3]]
    plot.ylim = c(-0.1, mean(ypts)+12*sd(ypts))
  } else {
    stop(paste("Bad Data Series value:", ds, "- needs a value 1-7."))
  }
  
  # Make plot with GGPlot2
  myPlot <<- ggplot(
    data.frame(
      dates = ss.simple(data[dataRange][[7]], ssres),
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
    scale_y_continuous(limits = plot.ylim, expand=c(-0.1,0)) + 
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
  print(paste("Saving plot for", axes[i], "axis..."))
  ggsave(
    paste("plots/", filename, "_", axes[i], ".png", sep = ""),
    plots[[i]], 
    dpi= 240,
    width=nrow(data)/25/300,
    height=2.5,
    limitsize = FALSE
  )
}

print("All plots saved.")
