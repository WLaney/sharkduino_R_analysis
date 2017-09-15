# imports
require("ggplot2") # for pretty plots
require("scales") # for date formatting
require("cowplot") # for prettier plots

source("packages/import_data.R")
source("packages/subsample.R")

# Read in raw CSV. Place your raw CSV file in the data subdirectory,
# then edit the following line to reference it.
data = import_data("../test.csv", legacy=F)

## Simple scatterplot of one axis
# Which points to plot?
dataRange = 1:nrow(data)
# subsampling resolution?
ssres = 1000
# dataset?
ds = 3
# plot with ggplot2
qplot(
  x = ss(data[[7]][dataRange],ssres), 
  y = ss(data[[ds]][dataRange],ssres), 
  geom = "line") + #scale_y_continuous(breaks = -12:12/10) + 
  scale_x_datetime(breaks = date_breaks("10 sec"), labels = date_format("%S")) +
  labs(
    x="Time", 
    y="Sensor Data", 
    title="Sensor Data over Time") 


