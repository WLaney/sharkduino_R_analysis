#=============================================================================================
# Madgwick.R
#=============================================================================================
# R language port of https://github.com/PaulStoffregen/MadgwickAHRS
# Written for Sharkduino by Dara Kharabi
#=============================================================================================

#-------------------------------------------------------------------------------------------
# 3D Cartesian and quaternion rotation matrices

Rx = function(th) {
  return(rbind(c(       1,       0,        0),
               c(       0, cos(th), -sin(th)),
               c(       0, sin(th),  cos(th))))
}

Ry = function(th) {
  return(rbind(c( cos(th),       0,  sin(th)),
               c(       0,       1,        0),
               c(-sin(th),       0,   cos(th))))
}

Rz = function(th) {
  return(rbind(c( cos(th), -sin(th),       0),
               c( sin(th),  cos(th),       0),
               c(       0,        0,       1)))
}

Rq = function(q0, q1, q2, q3) {
  return(rbind(c(q0^2 + q1^2 - (q2^2 +q3^2),         2*q1*q2 - 2*q0*q3,         2*q1*q3 + 2*q0*q2),
               c(         2*q1*q2 + 2*q0*q3, q0^2 - q1^2 + q2^2 - q3^2,         2*q2*q3 - 2*q0*q1),
               c(         2*q1*q3 - 2*q0*q2,         2*q2*q3 + 2*q0*q1, q0^2 - q1^2 - q2^2 + q3^2)))
}

#-------------------------------------------------------------------------------------------
# 3D Cartesian rotation function (returns data frame)
# Takes 3 vectors for initial xyz values and 3 vectors for yaw/pitch/roll rotation amounts
# (in radians). Returns a data frame with rotated xys values. Rotation order is Y/P/R.

rotate.df = function(vx, vy, vz, ey, ep, er) {
  newRot = data.frame(x = 0, y = 0, z = 0)
  
  for (n in 1:length(vx)) {
    R3d = Rx(er[n]) %*% Ry(ep[n]) %*% Rz(ey[n])
    newRot[n,] = c(vx[n], vy[n], vz[n]) %*% R3d
  }
  
  return(newRot)
}

#-------------------------------------------------------------------------------------------
# 3D Cartesian rotation function (returns vector)
# Takes a vector c(x,y,z) for initial xyz values and a vector c(r,p,y) for yaw/pitch/roll
# EAs (in radians). Returns a vector with rotated xys values. Rotation order is Y/P/R.

rotate.vec = function(val, eas) {
  R3d = Rx(eas[1]) %*% Ry(eas[2]) %*% Rz(eas[3])
  return(val %*% R3d)
}

#-------------------------------------------------------------------------------------------
# Quaternion rotation function (returns data frame)

rotateQ.df = function(vx, vy, vz, q0, q1, q2, q3) {
  newRot = data.frame(x = 0, y = 0, z = 0)
  
  for (n in 1:length(vx)) {
    newRot[n,] = c(vx[n], vy[n], vz[n]) %*% Rq(q0[n], q1[n], q2[n], q3[n])
  }
  
  return(newRot)
}

# Inverted quaternion rotation. 
# Computationally less expensive than inverting the quaternion and rotating
rotateQ.df.inv = function(vx, vy, vz, q0, q1, q2, q3) {
  newRot = data.frame(x = 0, y = 0, z = 0)
  
  for (n in 1:length(vx)) {
    newRot[n,] = c(vx[n], vy[n], vz[n]) %*% t(Rq(q0[n], q1[n], q2[n], q3[n]))
  }
  
  return(newRot)
}


#-------------------------------------------------------------------------------------------
# Initial quaternion for an object at rest (z is up)

initial.q = c(1,0,0,0)

#-------------------------------------------------------------------------------------------
# Inverse square root
# Since real-time performance isn't a goal for us, I'm using a regular 1/sqrt(x) instead of
# the fast inverse sqrt trick Stoffregen's library uses.
# But it's just soooooo cool, y'know? --Ben

invSqrt = function(x) {
  return (1/sqrt(x))
}

#-------------------------------------------------------------------------------------------
# Compute roll, pitch, and yaw Euler Angles, given components of a quaternion q

#YPR order
toEuler.df = function(qs) {
  yaw.l = c()
  pitch.l = c()
  roll.l = c()
  
  for (i in 1:nrow(qs)) {
    q0 = qs[[1]][i]
    q1 = qs[[2]][i]
    q2 = qs[[3]][i]
    q3 = qs[[4]][i]
    
    test = q1*q2 + q3*q0;
    if (test > 0.499) { # singularity at north pole
      yaw = 2 * atan2(q1,q0)
      pitch = pi/2
      roll = 0
    } else if (test < -0.499) { # singularity at south pole
      yaw = -2 * atan2(q1,q0)
      pitch = -pi/2
      roll = 0
    } else {
      yaw = atan2(q2*q0-q1*q3 , 0.5 - q2*q2 - q3*q3)
      pitch = asin(2*test)
      roll = atan2(q1*q0-q2*q3 , 0.5 - q1*q1 - q3*q3)
    }
    yaw.l[i] = yaw
    pitch.l[i] = pitch
    roll.l[i] = roll
  }
  yaw = yaw.l
  pitch = pitch.l
  roll = roll.l
  
  
  return(data.table(yaw, pitch, roll))
}


toEuler = function(q0, q1, q2, q3) {
  test = q1*q2 + q3*q0;
  
  if (test > 0.499) { # singularity at north pole
    yaw = 2 * atan2(q1,q0)
    pitch = pi/2
    roll = 0
  } else if (test < -0.499) { # singularity at south pole
    yaw = -2 * atan2(q1,q0)
    pitch = -pi/2
    roll = 0
  } else {
    yaw = atan2(q2*q0-q1*q3 , 0.5 - q2*q2 - q3*q3)
    pitch = asin(2*test)
    roll = atan2(q1*q0-q2*q3 , 0.5 - q1*q1 - q3*q3)
  }
  
  return(c(yaw, pitch, roll))
}


#RPY order
toEuler.df.RPY = function(qs) {
  q0 = qs[[1]]
  q1 = qs[[2]]
  q2 = qs[[3]]
  q3 = qs[[4]]
  roll = atan2(q0*q1 + q2*q3, 0.5 - q1*q1 - q2*q2)
  pitch = asin(pmax(pmin(-2.0 * (q1*q3 - q0*q2), 1), -1))
  yaw = atan2(q1*q2 + q0*q3, 0.5 - q2*q2 - q3*q3)
  return(data.table(roll, pitch, yaw))
}

toEuler.RPY = function(q0, q1, q2, q3) {
  roll = atan2(q0*q1 + q2*q3, 0.5 - q1*q1 - q2*q2)
  pitch = asin(pmax(pmin(-2.0 * (q1*q3 - q0*q2), 1), -1))
  yaw = atan2(q1*q2 + q0*q3, 0.5 - q2*q2 - q3*q3)
  return(c(roll, pitch, yaw))
}

#-------------------------------------------------------------------------------------------
# IMU algorithm update (actually an AHRS algorithm for 6 DoF (no magnetometer) systems)
# Takes an initial quaternion (4 components), accelerometer x/y/z, and gyro x/y/z (in dps).
# Returns an updated quaternion. Frequency is sample freq. in Hz. Beta is proportional gain.

madgwick.update.6dof = function(q0 = 1, q1 = 0, q2 = 0, q3 = 0,
                                gx, gy, gz, ax, ay, az, 
                                beta = 0.1, frequency = 25)  {
  
  invSampleFreq = 1.0 / frequency
  # Convert gyroscope degrees/sec to radians/sec
  gx = gx * 0.01745329
  gy = gy * 0.01745329
  gz = gz * 0.01745329
  
  # Rate of change of quaternion from gyroscope
  qDot1 = 0.5 * (-q1 * gx - q2 * gy - q3 * gz)
  qDot2 = 0.5 * ( q0 * gx + q2 * gz - q3 * gy)
  qDot3 = 0.5 * ( q0 * gy - q1 * gz + q3 * gx)
  qDot4 = 0.5 * ( q0 * gz + q1 * gy - q2 * gx)
  
  # Compute feedback only if accelerometer measurement valid (avoids NaN in accelerometer normalisation)
  if(!((ax == 0.0) && (ay == 0.0) && (az == 0.0))) {
    
    # Normalise accelerometer measurement
    recipNorm = invSqrt(ax * ax + ay * ay + az * az)
    ax = ax * recipNorm
    ay = ay * recipNorm
    az = az * recipNorm
    

    # Gradient descent algorithm corrective step
    Fmat = rbind(2*(q1*q3 - q0*q2) - ax,
                 2*(q0*q1 + q2*q3) - ay,
                 2*(0.5 - q1^2 - q2^2) - az)
    Jmat = rbind(c(-2*q2,  2*q3, -2*q0,	2*q1),
                 c( 2*q1,  2*q0,  2*q3,	2*q2), 
                 c(    0, -4*q1, -4*q2,	0))
    step = (t(Jmat) %*% Fmat);
    
    step = step / norm(step, "2");	# normalise step magnitude

    s0 = step[1]
    s1 = step[2]
    s2 = step[3]
    s3 = step[4]
    
    
    # Apply feedback step
    qDot1 = qDot1 - (beta * s0)
    qDot2 = qDot2 - (beta * s1)
    qDot3 = qDot3 - (beta * s2)
    qDot4 = qDot4 - (beta * s3)
  } else {
    print("Warning: Zero accelerometer measurement detected. Skipping feedback step.")
  }
  
  # Integrate rate of change of quaternion to yield updated quaternion
  q0 = q0 + (qDot1 * invSampleFreq)
  q1 = q1 + (qDot2 * invSampleFreq)
  q2 = q2 + (qDot3 * invSampleFreq)
  q3 = q3 + (qDot4 * invSampleFreq)
  
  # Normalise quaternion
  recipNorm = invSqrt(q0 * q0 + q1 * q1 + q2 * q2 + q3 * q3)
  q0 = q0 * recipNorm
  q1 = q1 * recipNorm
  q2 = q2 * recipNorm
  q3 = q3 * recipNorm
  
  # Return updated quaternion
  return(c(q0, q1, q2, q3))
}

#-------------------------------------------------------------------------------------------
# IMU algorithm update - older version that eschews matrix math.
# This version may be slightly faster than the other method, but the math is harder to 
# understand.

madgwick.update.6dof.nomatrix = function(q0 = 1, q1 = 0, q2 = 0, q3 = 0,
                                         gx, gy, gz, ax, ay, az, 
                                         beta = 0.1, frequency = 25)  {
  
  invSampleFreq = 1.0 / frequency
  # Convert gyroscope degrees/sec to radians/sec
  gx = gx * 0.01745329
  gy = gy * 0.01745329
  gz = gz * 0.01745329
  
  # Rate of change of quaternion from gyroscope
  qDot1 = 0.5 * (-q1 * gx - q2 * gy - q3 * gz)
  qDot2 = 0.5 * ( q0 * gx + q2 * gz - q3 * gy)
  qDot3 = 0.5 * ( q0 * gy - q1 * gz + q3 * gx)
  qDot4 = 0.5 * ( q0 * gz + q1 * gy - q2 * gx)
  
  # Compute feedback only if accelerometer measurement valid (avoids NaN in accelerometer normalisation)
  if(!((ax == 0.0) && (ay == 0.0) && (az == 0.0))) {
    
    # Normalise accelerometer measurement
    recipNorm = invSqrt(ax * ax + ay * ay + az * az)
    ax = ax * recipNorm
    ay = ay * recipNorm
    az = az * recipNorm
    
    # Auxiliary variables to avoid repeated arithmetic
    # [Porting note: I could remove this without adding much (if any) overhead, 
    # but I want to stay as close to the original code as possible for now.
    # Consider simplifying later.]
    i.2q0  =  2 * q0
    i.2q1  =  2 * q1
    i.2q2  =  2 * q2
    i.2q3  =  2 * q3
    i.4q0  =  4 * q0
    i.4q1  =  4 * q1
    i.4q2  =  4 * q2
    i.8q1  =  8 * q1
    i.8q2  =  8 * q2
    q0q0 = q0 * q0
    q1q1 = q1 * q1
    q2q2 = q2 * q2
    q3q3 = q3 * q3
    
    # Gradient decent algorithm corrective step
    s0 = i.4q0 * q2q2 + i.2q2 * ax + i.4q0 * q1q1 - i.2q1 * ay
    s1 = i.4q1 * q3q3 - i.2q3 * ax + 4.0 * q0q0 * q1 - i.2q0 * ay - i.4q1 + i.8q1 * q1q1 + i.8q1 * q2q2 + i.4q1 * az
    s2 = 4.0 * q0q0 * q2 + i.2q0 * ax + i.4q2 * q3q3 - i.2q3 * ay - i.4q2 + i.8q2 * q1q1 + i.8q2 * q2q2 + i.4q2 * az
    s3 = 4.0 * q1q1 * q3 - i.2q1 * ax + 4.0 * q2q2 * q3 - i.2q2 * ay
    
    
    
    # Normalize step magnitude
    recipNorm = invSqrt(s0 * s0 + s1 * s1 + s2 * s2 + s3 * s3) 
    s0 = s0 * recipNorm
    s1 = s1 * recipNorm
    s2 = s2 * recipNorm
    s3 = s3 * recipNorm
    
    # Apply feedback step
    qDot1 = qDot1 - (beta * s0)
    qDot2 = qDot2 - (beta * s1)
    qDot3 = qDot3 - (beta * s2)
    qDot4 = qDot4 - (beta * s3)
  } else {
    print("Warning: Zero accelerometer measurement detected. Skipping feedback step.")
  }
  
  # Integrate rate of change of quaternion to yield updated quaternion
  q0 = q0 + (qDot1 * invSampleFreq)
  q1 = q1 + (qDot2 * invSampleFreq)
  q2 = q2 + (qDot3 * invSampleFreq)
  q3 = q3 + (qDot4 * invSampleFreq)
  
  # Normalise quaternion
  recipNorm = invSqrt(q0 * q0 + q1 * q1 + q2 * q2 + q3 * q3)
  q0 = q0 * recipNorm
  q1 = q1 * recipNorm
  q2 = q2 * recipNorm
  q3 = q3 * recipNorm
  
  # Return updated quaternion
  return(c(q0, q1, q2, q3))
}

#-------------------------------------------------------------------------------------------
# AHRS algorithm update

madgwick.update = function(q0 = 1, q1 = 0, q2 = 0, q3 = 0,
                           gx, gy, gz, 
                           ax, ay, az,
                           mx = 0, my = 0, mz = 0, 
                           beta = 0.1, frequency = 25) {
  
  # Use IMU algorithm if magnetometer measurement invalid (avoids NaN in magnetometer normalisation)
  if((mx == 0) && (my == 0) && (mz == 0)) {

    result.6dof = madgwick.update.6dof(q0, q1, q2, q3, 
                                       gx, gy, gz, 
                                       ax, ay, az, 
                                       beta = beta, frequency = frequency)
    return(result.6dof)

  } else {

    print("Sharkduino with a magnetometer?")
    return(NA)

  }

  # I'll finish this later.
  # The following algorithm is currently unnecessary for Sharkduino, 
  # since we don't have a magnetometer.

  #invSampleFreq = 1.0 / sampleFreqDef
  # # Convert gyroscope degrees/sec to radians/sec
  # gx *= 0.0174533;
  # gy *= 0.0174533;
  # gz *= 0.0174533;
  
  # # Rate of change of quaternion from gyroscope
  # qDot1 = 0.5 * (-q1 * gx - q2 * gy - q3 * gz);
  # qDot2 = 0.5 * (q0 * gx + q2 * gz - q3 * gy);
  # qDot3 = 0.5 * (q0 * gy - q1 * gz + q3 * gx);
  # qDot4 = 0.5 * (q0 * gz + q1 * gy - q2 * gx);
  
  # # Compute feedback only if accelerometer measurement valid (avoids NaN in accelerometer normalisation)
  # if(!((ax == 0.0) && (ay == 0.0) && (az == 0.0))) {
  
  # # Normalise accelerometer measurement
  # recipNorm = invSqrt(ax * ax + ay * ay + az * az);
  # ax *= recipNorm;
  # ay *= recipNorm;
  # az *= recipNorm;
  
  # # Normalise magnetometer measurement
  # recipNorm = invSqrt(mx * mx + my * my + mz * mz);
  # mx *= recipNorm;
  # my *= recipNorm;
  # mz *= recipNorm;
  
  # # Auxiliary variables to avoid repeated arithmetic
  # _2q0mx = 2.0 * q0 * mx;
  # _2q0my = 2.0 * q0 * my;
  # _2q0mz = 2.0 * q0 * mz;
  # _2q1mx = 2.0 * q1 * mx;
  # _2q0 = 2.0 * q0;
  # _2q1 = 2.0 * q1;
  # _2q2 = 2.0 * q2;
  # _2q3 = 2.0 * q3;
  # _2q0q2 = 2.0 * q0 * q2;
  # _2q2q3 = 2.0 * q2 * q3;
  # q0q0 = q0 * q0;
  # q0q1 = q0 * q1;
  # q0q2 = q0 * q2;
  # q0q3 = q0 * q3;
  # q1q1 = q1 * q1;
  # q1q2 = q1 * q2;
  # q1q3 = q1 * q3;
  # q2q2 = q2 * q2;
  # q2q3 = q2 * q3;
  # q3q3 = q3 * q3;
  
  # # Reference direction of Earth's magnetic field
  # hx = mx * q0q0 - _2q0my * q3 + _2q0mz * q2 + mx * q1q1 + _2q1 * my * q2 + _2q1 * mz * q3 - mx * q2q2 - mx * q3q3;
  # hy = _2q0mx * q3 + my * q0q0 - _2q0mz * q1 + _2q1mx * q2 - my * q1q1 + my * q2q2 + _2q2 * mz * q3 - my * q3q3;
  # _2bx = sqrt(hx * hx + hy * hy);
  # _2bz = -_2q0mx * q2 + _2q0my * q1 + mz * q0q0 + _2q1mx * q3 - mz * q1q1 + _2q2 * my * q3 - mz * q2q2 + mz * q3q3;
  # _4bx = 2.0 * _2bx;
  # _4bz = 2.0 * _2bz;
  
  # # Gradient decent algorithm corrective step
  # s0 = -_2q2 * (2.0 * q1q3 - _2q0q2 - ax) + _2q1 * (2.0 * q0q1 + _2q2q3 - ay) - _2bz * q2 * (_2bx * (0.5 - q2q2 - q3q3) + _2bz * (q1q3 - q0q2) - mx) + (-_2bx * q3 + _2bz * q1) * (_2bx * (q1q2 - q0q3) + _2bz * (q0q1 + q2q3) - my) + _2bx * q2 * (_2bx * (q0q2 + q1q3) + _2bz * (0.5 - q1q1 - q2q2) - mz);
  # s1 = _2q3 * (2.0 * q1q3 - _2q0q2 - ax) + _2q0 * (2.0 * q0q1 + _2q2q3 - ay) - 4.0 * q1 * (1 - 2.0 * q1q1 - 2.0 * q2q2 - az) + _2bz * q3 * (_2bx * (0.5 - q2q2 - q3q3) + _2bz * (q1q3 - q0q2) - mx) + (_2bx * q2 + _2bz * q0) * (_2bx * (q1q2 - q0q3) + _2bz * (q0q1 + q2q3) - my) + (_2bx * q3 - _4bz * q1) * (_2bx * (q0q2 + q1q3) + _2bz * (0.5 - q1q1 - q2q2) - mz);
  # s2 = -_2q0 * (2.0 * q1q3 - _2q0q2 - ax) + _2q3 * (2.0 * q0q1 + _2q2q3 - ay) - 4.0 * q2 * (1 - 2.0 * q1q1 - 2.0 * q2q2 - az) + (-_4bx * q2 - _2bz * q0) * (_2bx * (0.5 - q2q2 - q3q3) + _2bz * (q1q3 - q0q2) - mx) + (_2bx * q1 + _2bz * q3) * (_2bx * (q1q2 - q0q3) + _2bz * (q0q1 + q2q3) - my) + (_2bx * q0 - _4bz * q2) * (_2bx * (q0q2 + q1q3) + _2bz * (0.5 - q1q1 - q2q2) - mz);
  # s3 = _2q1 * (2.0 * q1q3 - _2q0q2 - ax) + _2q2 * (2.0 * q0q1 + _2q2q3 - ay) + (-_4bx * q3 + _2bz * q1) * (_2bx * (0.5 - q2q2 - q3q3) + _2bz * (q1q3 - q0q2) - mx) + (-_2bx * q0 + _2bz * q2) * (_2bx * (q1q2 - q0q3) + _2bz * (q0q1 + q2q3) - my) + _2bx * q1 * (_2bx * (q0q2 + q1q3) + _2bz * (0.5 - q1q1 - q2q2) - mz);

  # recipNorm = invSqrt(s0 * s0 + s1 * s1 + s2 * s2 + s3 * s3); # normalise step magnitude
  # s0 *= recipNorm;
  # s1 *= recipNorm;
  # s2 *= recipNorm;
  # s3 *= recipNorm;
  
  # # Apply feedback step
  # qDot1 -= beta * s0;
  # qDot2 -= beta * s1;
  # qDot3 -= beta * s2;
  # qDot4 -= beta * s3;
  # }
  
  # # Integrate rate of change of quaternion to yield quaternion
  # q0 += qDot1 * invSampleFreq;
  # q1 += qDot2 * invSampleFreq;
  # q2 += qDot3 * invSampleFreq;
  # q3 += qDot4 * invSampleFreq;
  
  # # Normalise quaternion
  # recipNorm = invSqrt(q0 * q0 + q1 * q1 + q2 * q2 + q3 * q3);
  # q0 *= recipNorm;
  # q1 *= recipNorm;
  # q2 *= recipNorm;
  # q3 *= recipNorm;
  # anglesComputed = 0;
  # }

}

#-------------------------------------------------------------------------------------------
# Convenience function for calling AHRS algorithm update with vector arguments

madgwick.update.vec = function(q = c(1, 0, 0, 0), 
                               g, a, m, 
                               beta = 0.1, frequency = 25) {
  # Break vectors into components
  q0 = q[1]
  q1 = q[2]
  q2 = q[3]
  q3 = q[4]
  
  gx = g[1]
  gy = g[2]
  gz = g[3]
  
  ax = a[1]
  ay = a[2]
  az = a[3]
  
  mx = m[1]
  my = m[2]
  mz = m[3]
  
  return(madgwick.update(q0, q1, q2, q3, 
                         gx, gy, gz, 
                         ax, ay, az, 
                         mx, my, mz, 
                         beta, frequency))
}
