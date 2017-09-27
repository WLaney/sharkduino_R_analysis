#=============================================================================================
# subsamble
#=============================================================================================
# functions for subsampling large datasets
# Written for Sharkduino by Dara Kharabi
#=============================================================================================

# subsamples obj to 1/ssres points, taking the average of the source samples
ss = function(obj, ssres=1) {
  if (ssres == 1) return(obj)
  nCols = floor(length(obj)/ssres)
  buckets = matrix(obj[1:(nCols*ssres)], ssres, nCols)
  means = colMeans(buckets)
  # if there's a short column, take its mean and append that
  if (nCols != length(obj)/ssres) {
    means = c(means, mean(obj[((nCols*ssres) + 1):length(obj)]))
  }
  return(means)
}

# subsamples obj to 1/ssres points by returning every nth point
ss.simple = function(obj, ssres=1) {
  if (ssres == 1) return(obj)
  return(obj[seq(1, length(obj), ssres)])
}