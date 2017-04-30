library("data.table")
library("ggplot2")
library("cowplot")

source(".Rprofile")
source("packages/import_data.R")
source("packages/madgwick.R")
source("packages/subsample.R")


if (!exists("data.imported")) {
  bp0 = import_data("data/backpack/backpack-0-data.csv")
  bp1 = import_data("data/backpack/backpack-1-data.csv")
  bp2 = import_data("data/backpack/backpack-2-data.csv")
  #bp3 = import_data("data/backpack/backpack-3-data.csv")
  bp4 = import_data("data/backpack/backpack-4-data.csv")
  bp5 = import_data("data/backpack/backpack-5-data.csv")
  
  bp = rbind(bp0, bp1, bp2, bp4, bp5)
  data.imported = T
}


ds = 1
ssres = 50

sbp0.dates = ss(bp0[[7]],  ssres)
sbp0.data  = ss(bp0[[ds]], ssres)
sbp1.dates = ss(bp1[[7]],  ssres) 
sbp1.data  = ss(bp1[[ds]], ssres) 
sbp2.dates = ss(bp2[[7]],  ssres)
sbp2.data  = ss(bp2[[ds]], ssres)
sbp4.dates = ss(bp4[[7]],  ssres)
sbp4.data  = ss(bp4[[ds]], ssres)
sbp5.dates = ss(bp5[[7]],  ssres)
sbp5.data  = ss(bp5[[ds]], ssres)

sbp.dates = ss(bp[[7]],  ssres)
sbp.data  = ss(bp[[ds]], ssres)
sbp = data.table(date = sbp.dates, data = sbp.data)

ggplot(data = sbp, aes(date, data)) + geom_line() + 
  labs(
    title = "Backpack Dataset",
    x = "Date + Time",
    y = "Accel/Gyro Data"
  )

bp.plot = ggplot() + 
  labs(
    title = "Backpack Dataset",
    x = "Date + Time",
    y = "Accel/Gyro Data"
  ) +
  geom_line(aes(x=sbp0.dates, y=sbp0.data), col="purple") +
  geom_line(aes(x=sbp1.dates, y=sbp1.data), col="red") +
  geom_line(aes(x=sbp2.dates, y=sbp2.data), col="blue") +
  geom_line(aes(x=sbp4.dates, y=sbp4.data), col="green") +
  geom_line(aes(x=sbp5.dates, y=sbp5.data), col="orange") 

print(bp.plot)