setwd("~/sharkduino/sharkduino_R_analysis")

library("data.table")
library("ggplot2")
library("cowplot")

source("import_data.R")
source("madgwick.R")

gyro.data = import_data("data/orient/gyro_data.csv")
orient5.data = import_data("data/orient/orient5_data_interp_pos.csv")

extract.qs = function(data, beta = 0.1, frequency = 25) {
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


qx = 1:nrow(qs)

euler.data = toEuler.df(qs)

qplot(qx, euler.data[[2]], geom = "line")
