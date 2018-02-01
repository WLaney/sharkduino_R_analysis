# combines csvs and generates plots for all dataset directories in base.dir

source("packages/data_pipe.R")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
# Path to base folder (the folder containing the dataset folders you want to process)
#base.dir = "/Users/dara/GDrive - WM/Animal Tag Lab Book/Data"
base.dir = "/Users/dara/Projects/Sharkduino/testDatasets"

# ------------------------------------------------------------------------------

# If you set parallel to TRUE, run from command line, not RStudio
apply.data.pipeline.mc(base.dir, "list(combine.data.csvs, make.summary.plots)")
