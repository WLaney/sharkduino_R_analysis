require("data.table")
require("RAHRS")

load("data/orient5_data_interp_pos.RData")

pos.data[, gx := gx*pi/180]
pos.data[, gy := gy*pi/180]
pos.data[, gz := gz*pi/180]

MadgwickIMU(25, .04, c(0,0,0,1), as.numeric(pos.data[1, 4:6]), as.numeric(pos.data[1, 1:3]))

qs = data.frame(q1=1,q2=0,q3=0,q4=0)

for (s in 1:nrow(pos.data)) {
  qs[s+1,] = MadgwickIMU(1/25, .04, as.numeric(qs[s,]), as.numeric(pos.data[s, 4:6]), as.numeric(pos.data[s, 1:3]))
}