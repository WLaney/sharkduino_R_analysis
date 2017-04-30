# Initial tests for madgwick.R - superceded by madgwick_test.R

library("data.table")
library("ggplot2")
library("cowplot")

source(".Rprofile")
source("packages/import_data.R")
source("packages/madgwick.R")
source("packages/subsample.R")

extract.qs = function(data, beta = 01.1, frequency = 25) {
  qs = data.frame(q0=1,q1=0,q2=0,q3=0)
  
  for (s in 1:nrow(data)) {
    qs[s+1,] = madgwick.update.vec(q = as.numeric(qs[s,]), 
                                   g = as.numeric(data[s, 4:6]), 
                                   a = as.numeric(data[s, 1:3]),
                                   m = c(0, 0, 0),
                                   beta, frequency)
  }
  return(qs)
}



turn.data = import_data("data/orient/gyro_data.csv")
camera1.data = import_data("data/camera/camera1_data.csv")
camera2.data = import_data("data/camera/camera2_data.csv")

turn.qs = extract.qs(turn.data)
camera1.qs = extract.qs(camera1.data)
camera2.qs = extract.qs(camera2.data)


turn.eas = toEuler.df(turn.qs)
camera1.eas = toEuler.df(camera1.qs)
camera2.eas = toEuler.df(camera2.qs)


turn.qx = 1:nrow(turn.qs)
camera1.qx = 1:nrow(camera1.qs)
camera2.qx = 1:nrow(camera2.qs)

qplot(qx, turn.eas[[2]], geom = "line")

getEAs = function(datafile) {
  myData = import_data(datafile)
  myQs = extract.qs(myData)
  myEAs = toEuler.df(myQs)
  return(myEAs)
}


fwrite(getEAs("data/camera/nod_data.csv"), "nodeuler.csv")

##qplot demos
qplot(((1:nrow(camera1.data))/25)[200:5250], camera1.data[[6]][200:5250], geom="line")+scale_x_continuous(breaks = seq(0, 300, 10))

qplot(((camera1.qx)/25)[200:5250], camera1.eas[[3]][200:5250], geom="line")+scale_x_continuous(breaks = seq(0, 300, 10))


nod.eas = getEAs("data/camera/nod_data.csv")
