path = "~/Documents/My Data/BRAZIL/Elections/"
filename = "dataset name goes here"

file.names <- dir(path, pattern ="data.csv")

for(i in 1:length(file.names)){
  dataset <- import_data(file.names[i], legacy = F)
  out.file <- rbind(out.file, file)
}
fwrite(out.file, file = paste(filename, ".csv", sep = ""))

