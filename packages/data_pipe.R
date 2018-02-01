#=============================================================================================
# data_pipe.R
#=============================================================================================
# Batch processing of Sharkduino data directories
# Includes functions for various operations, as well as a way to create data pipelines
# Written for Sharkduino by Dara Kharabi
#=============================================================================================

# imports
require("ggplot2") # for pretty plots
require("scales") # for better scales
require("cowplot") 
require("RcppRoll")

source("packages/import_data.R")
source("packages/combine_csvs.R")
source("packages/subsample.R")

#-------------------------------------------------------------------------------------------
# Helper function to make running series of operations easier
# Applies all functions in func.list to every data directory in base.dir

apply.data.pipeline = function(base.dir, func.list) {
  # get directories in base dir
  dataset.dirs <- dir(path=base.dir, full.names = TRUE)
  # only directories, please
  dataset.dirs = dataset.dirs[file.info(dataset.dirs)$isdir]
  
  continue.flags = rep(TRUE, length(dataset.dirs))
  
  for (ifunc in func.list) {
    continue.flags = sapply(dataset.dirs[continue.flags == TRUE], ifunc)
  }
  
  return(continue.flags)
}


#-------------------------------------------------------------------------------------------
# Combines raw data csvs in order of time - also cleans them

combine.data.csvs = function(data.dir) {
  ds.name = tail(strsplit(data.dir,"/")[[1]], n=1)
  csv.name = paste(ds.name, "_combined.csv", sep="")
  csv.path = paste(data.dir, "/", csv.name, sep="")
  
  print(paste("Now combining CSV for dataset:", ds.name))
  combine.success = tryCatch(
    combine.csvs(
      path = paste(data.dir, "/csvs/data", sep=""),
      out.path = csv.path
    ),
    warning = function(w) {
      # if combining csvs fails
      # warning(w)
      return(FALSE)
    }
  )
  
  if (!is.null(combine.success)){
    print("....Failed to combine csvs, skipping this folder.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#-------------------------------------------------------------------------------------------
# Makes summary plots in the dataset/plots folder
# Needs a cleaned, combined CSV file to work

make.summary.plots = function(data.dir, dataRange = NA, ssres = 1) {
  ds.name = tail(strsplit(data.dir,"/")[[1]], n=1)
  csv.name = paste(ds.name, "_combined.csv", sep="")
  csv.path = paste(data.dir, "/", csv.name, sep="")
  
  print(paste("Now generating plots for dataset:", ds.name))
  data = import_data(csv.path, clean=TRUE)
  
  if (is.na(dataRange)) {
    dataRange = 1:nrow(data)
  }
    
  # make list of plots
  plots = lapply(
    1:7, 
    makeScatterPane, 
    data = data, 
    datasetName = ds.name, 
    dataRange = dataRange, 
    ssres = ssres
  )
    
  filename = gsub("/", "", gsub(" ", "_", paste(ds.name, "_summary", sep="")))
    
  axes = c(
    "ax",
    "ay",
    "az",
    "gx",
    "gy",
    "gz",
    "ODBA"
  )
  
  # create plots dir if needed
  plots.dir = paste(data.dir, "/plots", sep="")
  if (!dir.exists(plots.dir)) {
    print("..../plots/ directory doesn't exist. Creating one now.")
    dir.create(plots.dir)
  }
    
  for (i in 1:7) {
    print(paste("....Saving plot for", axes[i], "axis."))
    ggsave(
      paste(data.dir, "/plots/", filename, "_", axes[i], ".png", sep = ""),
      plots[[i]], 
      dpi= 240,
      width=nrow(data)/25/300,
      height=2.5,
      limitsize = FALSE
    )
  }
    
  print("....All plots saved.")
  
}

#-------------------------------------------------------------------------------------------
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
    scale_x_datetime(breaks = date_breaks("5 mins"), date_labels = "%H:%M", expand=c(0,0)) + 
    scale_y_continuous(limits = plot.ylim, expand=c(-0.1,0)) + 
    theme(axis.text=element_text(size=9), axis.title=element_text(size=12,face="bold"))
  
  return(myPlot)
}


