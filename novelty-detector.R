# This script is an offline version of my naive novelty detection algorithm.
# While I don't expect it to be too accurate, I do think it's a good thing to show the Doctor at
# the end of the semester, and serves as a good introduction to R.
source(".Rprofile")
source("packages/import_data.R")

# Get magnitude of a vector
normalize <- function(x) sqrt(sum(x^2))

# Discard all but every nth row of xs
subsample <- function(xs, factor) {
  if (factor == 1) {
    xs
  } else {
    xs[seq(from=1,to=length(xs),by=factor)]
  }
}

# Degree to which data is subsampled during plotting.
subsample_factor = 2

# Import data
data = import_data("data/ben-walking-to-get-laundry1.csv")
timestamps = read.csv("data/ben-walking-to-get-laundry1-timestamp.csv")

# Convert times into POSIX times
timestamps[,"time"] = as.POSIXct(timestamps[,1], format="%Y-%m-%d %H:%M:%OS")

t1 = timestamps[[1,"time"]]
t2 = data$date_time[1]

# Match up timestamps to nearest index in data
timestamps[,"index"] = apply(timestamps, 1, function(row) {
  which.max(data$date_time > row[["time"]])
})

# Find the minimum time for which timestamp > time
# Set index to the index of this time

# Convert accelerometer data into total acceleration and plot, using
# timestamps as labels (?)
total_accel = apply(data[,c("ax","ay","az")], 1, normalize)

# Plot data

plot(subsample(total_accel, subsample_factor), cex=0.2)
title("Summed Accelerometer")

real_indices = timestamps[["index"]] / subsample_factor
abline(v=real_indices)
text(real_indices, c(2.3,2.2), timestamps[["activity"]], cex=0.8)
