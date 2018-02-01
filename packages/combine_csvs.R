#=============================================================================================
# combine_csvs.R
#=============================================================================================
# Just a function to combine Sharkduino CSVs into one long file
# Written for Sharkduino by Dara Kharabi and Hanqiu Peng
#=============================================================================================

source("packages/import_data.R")

#-------------------------------------------------------------------------------------------
# Combines multiple csvs and either writes or returns the resulting file/data.frame

combine.csvs = function(path="", out.path = "", write = TRUE, return.df = FALSE, combine.cleaned = FALSE) {

  file.names <- list.files(path, pattern ="data.csv", full.names = TRUE)
  
  # warn if the folder givenis empty
  if (length(file.names) == 0) {
    warning(paste("No csvs detected in", path, "-- returning NA."))
    return(NA)
  }

  out.df = NA

  for(i in 1:length(file.names)){
    # import the next piece of csv data
    if (combine.cleaned == TRUE) {
      dataset = import.data.cleaned(file.names[i], legacy = F)
    } else {
      dataset = import_data(file.names[i], legacy = F)
    }
    
    if (class(out.df)[1] == "logical") {
      # if this is the first valid csv, make it the output
      out.df = dataset
    } else if (class(dataset)[1] != "logical" ) {
      # if the new csv part isn't garbage, append it
      if ((dataset[[7]][1] - out.df[[7]][1]) > 0) {
        # append if csv is at a later time
        out.df <- rbind(out.df, dataset)
      } else {
        # prepend if csv is at an earlier time
        out.df <- rbind(dataset, out.df)
      }
    }
  }
  if (write == TRUE) { 
    fwrite(out.df, file = out.path)
  } 
  if (return.df == TRUE) {
    return(out.df)
  }
}




