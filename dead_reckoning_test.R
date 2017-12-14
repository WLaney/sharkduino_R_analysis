# Rudimentary dead-reckoning script.
# Takes in accelerometer and orientation data and outputs position.
# Initial position and velocity is also required.
# This works best on small segments of data.

source('packages/madgwick.R')
source('packages/import_data.R')
library('signal')

# Read in data
if (!(exists("raw_data") && exists("orientation"))) {
  raw_data <- import_data("data/tmp-data.csv")
  orientation <- fread("data/tmp-eas.csv", sep=",", header=TRUE)
  
  raw_data <- raw_data[100000:130000,]
  orientation <- orientation[30000:40000,]
  
  print("Orientation loaded.")
  # orientation <- read.csv("data/tmp-eas.csv", header=TRUE)
}

# Slice out the stuff we need
accel <- raw_data[,c("ax", "ay", "az")]
orient_clipped <- orientation[2:(dim(orientation)[1]),]

# Rotate accelerometer-space acceleration into world-space normal vector.
# There is probably a better way to use this with rotate.vec, but I don't
# know R very well...
world_normal <- rotate.df(accel$ax, accel$ay, accel$az, orient_clipped$yaw, orient_clipped$pitch, orient_clipped$roll)

# Highpass to remove gravity vector, hopefully
filtFreq <- 1.0;
myHPF <- butter(type="high", 3, filtFreq/25.0)
world_acceleration <- data.table(filter(myHPF, world_normal$x),
                                 filter(myHPF, world_normal$y),
                                 filter(myHPF, world_normal$z))
# filtdat = smooth(filtdat)
double_integrate = function(x){cumsum(cumsum(x))}
world_position <- data.table(double_integrate(world_acceleration[[1]]),
                             double_integrate(world_acceleration[[2]]),
                             double_integrate(world_acceleration[[3]]))

fwrite(world_position, file="data/world-pos.csv")