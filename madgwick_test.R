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

toEuler = function(qs) {
  qw = as.numeric(qs[,1])
  qx = as.numeric(qs[,2])
  qy = as.numeric(qs[,3])
  qz = as.numeric(qs[,4])
  
  # roll (x-axis rotation)
  t0 = 2.0 * (qw * qx + qy * qz)
  t1 = 1.0 - 2.0 * (qx^2 + qy^2)
  roll = atan2(t0, t1)
  
  # pitch (y-axis rotation)
  t2 = 2.0 * (qw * qy - qz * qx)
  t2 = min(1.0, t2)
  t2 = max(-1.0, t2)
  pitch = asin(t2)
  
  # yaw (z-axis rotation)
  t3 = 2.0 * (qw * qz + qx * qy)
  t4 = 1.0 - 2.0 * (qy^2 + qz^2)  
  yaw = atan2(t3, t4)
  
  return(data.frame(roll=roll, pitch=pitch, yaw=yaw))
}