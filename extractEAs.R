setwd("~/sharkduino/sharkduino_R_analysis")

library.path <- cat(.libPaths())
#.libPaths("/Library/Frameworks/R.framework/Versions/3.3/Resources/library") 
library("data.table", lib.loc = library.path)
library("fasttime", lib.loc = library.path)


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


fwrite(getEAs("data/tmp-data.csv"), "data/tmp-eas.csv")
