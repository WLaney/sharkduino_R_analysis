source("packages/import_data.R")

combine.csvs = function(path="", write = TRUE, out.path = "") {

file.names <- paste(path, dir(path, pattern ="data.csv"), sep="")

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
  } else {
    return(out.df)
  }
}


