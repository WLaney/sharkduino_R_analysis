import_data <- function(data_file, save_csv = FALSE, save_rdata = FALSE) {
  require("data.table")
  require("fasttime")
  
  # Read in datafile (an uninterpolated CSV). fread for speed/data.table flexibility.
  raw.data = fread(data_file, sep=",", header=TRUE)
  # dates as POSIXct date objects (format = "%Y-%m-%d %H:%M:%OS")
  raw.data[, date_time := fastPOSIXct(raw.data[, date_time])] 
  
  # interpolate dates
  raw.data[, date_time := as.POSIXct(approx(raw.data[, date_time], xout=1:nrow(raw.data))$y, origin = "1970-01-01")] 
  # delete time-only rows
  interp.data = raw.data[!is.na(ax)]
  if (nrow(raw.data[is.na(date_time)]) > 50) print("WARNING: Unexpected number of rows with missing dates")
  # delete rows with no valid date interp.
  interp.data = interp.data[!is.na(date_time)]
  
  # Throw out empty temp/pressure rows. Later, when we get these sensors, we'll output 
  # separate files for them.
  pos.data = interp.data[,1:7]
  
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  
  filename = paste(strsplit(split_path(data_file)[1], "[.]")[[1]][1], "_interp_pos", sep = "")
  
  if (save_csv == TRUE) {
    fwrite(pos.data, file = paste(filename, ".csv", sep = ""))
  }
  if (save_rdata == TRUE) {
    save(pos.data, file = paste(filename, ".RData", sep = ""))
  }
  
  return(pos.data)
}
