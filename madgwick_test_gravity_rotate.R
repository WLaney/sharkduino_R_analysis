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


calib.data = import_data("data/tmp-data.csv")
calib.qs = extract.qs(calib.data)
calib.eas = toEuler.df(calib.qs)

new.as = rotate.df(calib.data[[1]], calib.data[[2]], calib.data[[3]],
          -calib.eas[[2]], -calib.eas[[1]], -calib.eas[[3]])
new.as.q = rotateQ.df.inv(calib.data[[1]], calib.data[[2]], calib.data[[3]],
                         calib.qs[[1]], calib.qs[[2]], calib.qs[[3]], calib.qs[[4]])

mag.new = sqrt(new.as[[1]]^2 + new.as[[2]]^2 + new.as[[3]]^2)
mag.base = sqrt(calib.data[[1]]^2 + calib.data[[2]]^2 + calib.data[[3]]^2)
qplot(1:nrow(calib.data), mag.new, geom="line") +
  geom_line(aes(y=mag.base), col="red", alpha = "0.5")

ds = 3
qplot(1:nrow(calib.data), new.as[[ds]], geom="line") +
  geom_line(aes(y=calib.data[[ds]]), col="red", alpha = "0.5") + 
  geom_line(aes(y=new.as.q[[ds]]), col="blue", alpha = "0.5")
