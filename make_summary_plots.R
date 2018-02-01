# combines csvs and generates plots for all dataset directories in base.dir

source("packages/import_data.R")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
# Path to base folder (the folder containing the dataset folders you want to process)
#base.dir = "/Users/dara/GDrive - WM/Animal Tag Lab Book/Data"
base.dir = "/Users/dara/Projects/Sharkduino/testDatasets"


# Range of points to plot (set to NA to plot the whole dataset)
head.dataRange = NA
# ------------------------------------------------------------------------------

apply.data.pipeline(base.dir, list(
  combine.data.csvs, 
  make.summary.plots
))
