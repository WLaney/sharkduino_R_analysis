source("packages/import_data.R")

combine.csvs = function(path="", write = TRUE, out.path = "") {

file.names <- paste(path, dir(path, pattern ="data.csv"), sep="")

  out.file=NA
  for(i in 1:length(file.names)){
    dataset <- import_data(file.names[i], legacy = F)
    if (class(out.file)[1]=="logical"){
      out.file=dataset
    }
    else if(class(dataset)[1]=="logical"){}
    else{
    out.file <- rbind(out.file, dataset)
    }
  }
  if (write == TRUE) fwrite(out.file, file = out.path)
}


