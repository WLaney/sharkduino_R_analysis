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
  return(colMeans(buckets))
}

# subsamples obj to 1/ssres points by returning every nth point
ss.simple = function(obj, ssres=1) {
  if (ssres == 1) return(obj)
  return(obj[seq(1, length(obj), ssres)])
}