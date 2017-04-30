require("data.table")
require("fasttime")

import_data <- function(data_file, save_csv = FALSE, save_rdata = FALSE, legacy = FALSE) {
  # Read in datafile (an uninterpolated CSV). fread for speed/data.table flexibility.
  raw.data = fread(data_file, sep=",", header=TRUE)
  # dates as POSIXct date objects (format = "%Y-%m-%d %H:%M:%OS")
  raw.data[, date_time := fastPOSIXct(raw.data[, date_time])] 
  
  # interpolate dates
  raw.data[, date_time := as.POSIXct(approx(raw.data[, date_time], xout=1:nrow(raw.data))$y, origin = "1970-01-01")] 
  # delete time-only rows
  interp.data = raw.data[!is.na(ax)]
  # Throw warning if more than 10 seconds of uninterpolated data.
  if (nrow(raw.data[is.na(date_time)]) > 250) print(paste("WARNING: Unexpected number of rows with missing dates - (", nrow(raw.data[is.na(date_time)]), ").", sep = ""))
  # delete rows with no valid date interp.
  interp.data = interp.data[!is.na(date_time)]
  
  # Throw out empty temp/pressure rows. Later, when we get these sensors, we'll output 
  # separate files for them.
  pos.data = interp.data[,1:7]
  
  # Process gyro data according to tag series
  if (legacy == FALSE) {
    
    # flip X and Y gyro axes for v2.x tags
    pos.data = pos.data[, c("gx", "gy") := .(gy, gx)]
    # invert X axis to preserve right handedness
    pos.data[,gx := -gx]
    
    # check if gyro data has missing rows
    if (anyNA(interp.data[,4:6])) {
      print("WARNING: Sparse gyro data detected. Linearly interpolating gyro reads (this may affect data quality).")
      
      # Interpolate missing gyro rows
      pos.data[, gx := approx(pos.data[, gx], xout=1:nrow(pos.data))$y] 
      pos.data[, gy := approx(pos.data[, gy], xout=1:nrow(pos.data))$y] 
      pos.data[, gz := approx(pos.data[, gz], xout=1:nrow(pos.data))$y] 
      # Delete rows with no valid gyro interp.
      print(paste("WARNING: Culling", nrow(pos.data[is.na(gx) | is.na(gy) | is.na(gz)]), "rows with no valid gyro interpolation."))
      pos.data = pos.data[!is.na(gx) & !is.na(gy) & !is.na(gz)]
    }
    
  } else {
    # Throw away bad gyro data from v1.x tags
    pos.data = pos.data[,c(1,2,3,7)]
  }
  
  # path splitting helper function
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  
  filename = paste(strsplit(split_path(data_file)[1], "[.]")[[1]][1], "_processed", sep = "")
  
  if (save_csv == TRUE) {
    fwrite(pos.data, file = paste(filename, ".csv", sep = ""))
  }
  if (save_rdata == TRUE) {
    save(pos.data, file = paste(filename, ".RData", sep = ""))
  }
  
  return(pos.data)
}
