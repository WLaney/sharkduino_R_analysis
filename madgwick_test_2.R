setwd("~/sharkduino/sharkduino_R_analysis")

library("data.table")
library("ggplot2")
library("cowplot")

source("import_data.R")
source("madgwick.R")

extract.qs = function(data, beta = 0.7, frequency = 25) {
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

getEAs = function(datafile) {
  myData = import_data(datafile)
  myQs = extract.qs(myData)
  myEAs = toEuler.df(myQs)
  return(myEAs)
}

calib.eas.alt = getEAs("data/camera/3dcalib_data.csv")

qplot(1:nrow(calib.eas.alt), calib.eas.alt[[1]])

fwrite(getEAs("data/camera/3dcalib_data.csv"), "data/3dcalibeuler.csv")
