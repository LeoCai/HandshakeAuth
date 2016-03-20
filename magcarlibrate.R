carlibrateNorth = function(mag, theta, fi){
  Mx = mag[,1]
  My = mag[,2]
  Mz = mag[,3]
  Hy = My*cos(theta) + Mx*sin(theta)*sin(fi) - Mz*cos(fi)*sin(theta)
  Hx = Mx*cos(fi) + Mz*sin(fi)
  return(Hy*Hy + Hx*Hx)
}
computeAngle = function(gacc){
  gx = gacc[,1]
  gy = gacc[,2]
  gz = gacc[,3]
  pitch = atan(gx / sqrt(gy*gy + gz*gz))
  roll = atan(gy / sqrt(gx*gx + gz*gz))
  return(list(pitch, roll))
}