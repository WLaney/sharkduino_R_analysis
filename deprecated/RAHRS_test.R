# Test for RAHRS, an AHRS library that was ultimately not used by the project
# By Dara Kharabi

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
  t2 = pmin(1.0, t2)
  t2 = pmax(-1.0, t2)
  pitch = asin(t2)
  
  # yaw (z-axis rotation)
  t3 = 2.0 * (qw * qz + qx * qy)
  t4 = 1.0 - 2.0 * (qy^2 + qz^2)  
  yaw = atan2(t3, t4)
  
  return(data.frame(roll=roll, pitch=pitch, yaw=yaw))
}

qsEuler = toEuler(qs)
qsEulerAlt = data.frame(roll=0, pitch=0, yaw=0)
for (i in 1: nrow(qs)) {qsEulerAlt[i,] = Q2EA.Xiao(as.numeric(qs[i,]), EulerOrder='xyz')}
qsE2 = data.frame(roll=0, pitch=0, yaw=0)
for (i in 1: nrow(qs)) {qsE2[i,] = Q2EA(as.numeric(qs[i,]), EulerOrder='xyz')}


#presentation stuff
# Data scatter
qplot((1:nrow(pos.data)), pos.data[,ax]*180/pi, geom="line")
#q->e discrepancy
qplot((1:nrow(qsE2)), qsE2[,3], geom="line")+geom_line(aes(y=qsEuler[,3]), col="red")
#