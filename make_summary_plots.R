# combines csvs and generates plots for all dataset directories in base.dir

source("packages/data_pipe.R")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
# Path to base folder (the folder containing the dataset folders you want to process)
#base.dir = "/Users/dara/GDrive - WM/Animal Tag Lab Book/Data"
base.dir = "/Users/dara/Projects/Sharkduino/testDatasets"

# Run in parallelized mode? (faster)
multicore = FALSE
# ------------------------------------------------------------------------------

if (multicore == TRUE) {
  apply.data.pipeline.mc(base.dir, "list(combine.data.csvs, make.summary.plots)")
} else {
  apply.data.pipeline(base.dir, list(combine.data.csvs, make.summary.plots))
}

