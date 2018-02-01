#=============================================================================================
# import_data.R
#=============================================================================================
# Importers for Sharkduino .CSV files
# Written for Sharkduino by William Laney, Dara Kharabi, and Hanqiu Peng
#=============================================================================================

require("data.table")
require("fasttime")

#-------------------------------------------------------------------------------------------
# Path splitting helper function

split_path <- function(x) {
  if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
}

#-------------------------------------------------------------------------------------------
# Importer for raw CSV files

import_data <- function(data_file, save_csv = FALSE, save_rdata = FALSE, legacy = FALSE, legacy_interp = FALSE, clean = FALSE) {
  # Read in datafile (an uninterpolated CSV). fread for speed/data.table flexibility.
  raw.data = fread(data_file, sep=",", header=TRUE)
  # Dates as POSIXct date objects (format = "%Y-%m-%d %H:%M:%OS")
  raw.data[, date_time := fastPOSIXct(raw.data[, date_time])] 
  
  # These lines check that the raw.data table has at least 2 values in the datetime column,
  # with rows between them that aren't NA in columns 1-6.
  count=0
  for (i in 1:nrow(raw.data)){
    if (!is.na(raw.data[i,7])){
      count = count + 1
    }
    if (count==2){
      break
    }
  }
  
  if (count <2){
    print("WARNING: Fewer than 2 values in the datetime column.")
    return(NA)
  }
  
  if(all(is.na(raw.data[,1:6]))){
    print("WARNING: No data rows in input file")
    return(NA)
  }
  
  # Interpolate dates if needed
  if (clean == FALSE) {
    if (legacy_interp == TRUE) {
      # Interpolate dates
      raw.data[, date_time := as.POSIXct(approx(raw.data[, date_time], xout=1:nrow(raw.data))$y, origin = "1970-01-01")] 
      # Delete time-only rows
      interp.data = raw.data[!is.na(ax)]
    } else {
      # Assign date rows to the previous data read (since those readings happen virtually simultaneously)
      raw.data[2:(nrow(raw.data)-1), date_time := raw.data[3:nrow(raw.data),7]]
      # Remove all time rows except the first
      interp.data = raw.data[c(TRUE, !is.na(ax[2:length(ax)]))]
      # Interpolate dates
      interp.data[, date_time := as.POSIXct(approx(
        interp.data[, date_time], 
        xout=1:nrow(interp.data))$y, 
        origin = "1970-01-01")] 
      # Remove any remaining time rows
      interp.data = interp.data[!is.na(ax)]
    }
  }

  # Throw warning if more than 10 seconds of uninterpolated data.
  if (nrow(interp.data[is.na(date_time)]) > 250) {
    print(paste("WARNING: Unexpected number of rows with missing dates - (", nrow(interp.data[is.na(date_time)]), ").", sep = ""))
  }
  # Delete rows with no valid date interp.
  interp.data = interp.data[!is.na(date_time)]
  
  # Throw out empty temp/pressure rows. Later, when we get these sensors, we'll output 
  # separate files for them.
  pos.data = interp.data[,1:7]
  
  # Process gyro data by tag series (if necessary)
  if (clean == FALSE) {
    if (legacy == TRUE) {
      # Throw away bad gyro data from v1.x tags
      pos.data = pos.data[,c(1,2,3,7)]
    } else {
        # These are fixes for v2.x tags due to the physical alignment of their sensors:
        #     flip X and Y gyro axes for v2.x tags
        pos.data = pos.data[, c("gx", "gy") := .(gy, gx)]
        #     invert X axis to preserve right handedness
        pos.data[,gx := -gx]
  
      
      # check if gyro data has missing rows
      # this code is experimental - TODO: test more thoroughly
      if (anyNA(interp.data[,4:6])) {
        print("WARNING: Sparse gyro data detected. Linearly interpolating gyro reads (this may affect data quality).")
        
        # remove first gyro read in each block (there is a bug in the sparse gyro code that makes this sample bad)
        pos.data[2:nrow(pos.data), gx := ifelse(is.na(pos.data[[4]][1:(nrow(pos.data)-1)]), NA, pos.data[[4]][2:nrow(pos.data)])]
        pos.data[2:nrow(pos.data), gy := ifelse(is.na(pos.data[[5]][1:(nrow(pos.data)-1)]), NA, pos.data[[5]][2:nrow(pos.data)])]
        pos.data[2:nrow(pos.data), gy := ifelse(is.na(pos.data[[6]][1:(nrow(pos.data)-1)]), NA, pos.data[[6]][2:nrow(pos.data)])]
        
        # Interpolate missing gyro rows
        pos.data[, gx := approx(pos.data[, gx], xout=1:nrow(pos.data))$y] 
        pos.data[, gy := approx(pos.data[, gy], xout=1:nrow(pos.data))$y] 
        pos.data[, gz := approx(pos.data[, gz], xout=1:nrow(pos.data))$y] 
        # Delete rows with no valid gyro interp.
        nBadRows = nrow(pos.data[is.na(gx) | is.na(gy) | is.na(gz)])
        if (nBadRows > 0) {
          print(paste("WARNING: Culling", nBadRows, "rows with no valid gyro interpolation."))
        }
        pos.data = pos.data[!is.na(gx) & !is.na(gy) & !is.na(gz)]
      }
    }
  }
  
  # Warn if processed data.table is empty
  if (all(is.na(pos.data))) {
    print("WARNING: Processed dataset appears to be blank. Check if your input file is correctly formatted.")
  }
  
  filename = paste(strsplit(split_path(data_file)[1], "[.]")[[1]][1], "_processed", sep = "")
  
  if (save_csv == TRUE) {
    print("INFO: Remember to use the [clean = TRUE] flag when importing a .csv file saved by this function.")
    fwrite(pos.data, file = paste(filename, ".csv", sep = ""))
  }
  if (save_rdata == TRUE) {
    save(pos.data, file = paste(filename, ".RData", sep = ""))
  }
  
  return(pos.data)
}

#-------------------------------------------------------------------------------------------
# Importer for CSV files already processed and written by import_data

import.data.cleaned <- function(data_file, save_csv = FALSE, save_rdata = FALSE, legacy = FALSE) {
  import_data(data_file, save_csv = FALSE, save_rdata = FALSE, legacy = FALSE, clean = TRUE)
}
