source("packages/import_data.R")

combine.csvs = function(path="", write = TRUE, return.df = FALSE, out.path = "") {

  file.names <- list.files(path, pattern ="data.csv", full.names = TRUE)
  if (length(file.names) == 0) {
    print(paste("WARNING: No csvs detected in", path, "-- returning NA."))
    return(NA)
  }

  out.df=NA
  for(i in 1:length(file.names)){
    dataset <- import_data(file.names[i], legacy = F)
    if (class(out.df)[1]=="logical"){
      out.df=dataset
    }
    else if(class(dataset)[1]=="logical"){}
    else{
    out.df <- rbind(out.df, dataset)
    }
  }
  if (write == TRUE) { 
    fwrite(out.df, file = out.path)
  } 
  if (return.df == TRUE) {
    return(out.df)
  }
}


