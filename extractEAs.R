source("packages/import_data.R")
source("packages/madgwick.R")

extract.qs = function(data, beta = 0.7, frequency = 25, init.q = c(1, 0, 0, 0)) {
  data.length = nrow(data)
  
  ql = vector("list", data.length + 1)
  ql[[1]] = as.list(init.q)
  
  for (s in 1:data.length) {
    # Print progress
    if (s %% 10000 == 0){ print(paste("Determined orientation for", s, "/", data.length, "rows"))}
    # Run madgwick to determine new row quaternion
    new.row.vec = madgwick.update.vec(q = as.numeric(ql[[s]]), 
                                      g = as.numeric(data[s, 4:6]), 
                                      a = as.numeric(data[s, 1:3]),
                                      m = c(0, 0, 0),
                                      beta, frequency)
    # Add new row to ql
    ql[[s+1]] =  as.list(new.row.vec)
  }
  
  # Turn ql into a data table
  qs = rbindlist(ql)
  names(qs) = c("q0", "q1", "q2", "q3")
  
  return(qs)
}

getEAs = function(datafile) {
  myData = import_data(datafile)
  print("Imported data. Beginning orientation determination...")
  myQs = extract.qs(myData)
  print("Determined orientation Qs for all rows.")
  myEAs = toEuler.df(myQs)
  return(myEAs)
}


fwrite(getEAs("data/tmp-data.csv"), "data/tmp-eas.csv")
