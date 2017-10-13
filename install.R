# Install dependent packages

# I don't like the way R handles selcting mirrors on the mac so I am hard coding one in
duke_repo <- "http://archive.linux.duke.edu/cran/" 

# Install packages
install.packages("data.table", repos=duke_repo)
install.packages("fasttime", repos=duke_repo)
install.packages("ggplot2", repos=duke_repo)
install.packages("cowplot", repos=duke_repo)
install.packages("psd", repos=duke_repo)
install.packages("signal", repos=duke_repo)
install.packages("seewave", repos=duke_repo)
install.packages("shiny", repos=duke_repo)
install.packages("RcppRoll", repos=duke_repo)

# Deprecated packages (listed here for historical record)
#install.packages("viridis", repos=duke_repo)
