library(rgl)
filterGravity = function(ax, ay, az, len) {
  gravity = as.double(as.vector(c(ax[1],ay[1],az[1])))
  #gravity = as.double(as.vector(c(0,0,0)))
  alpha = 0.85
  
  gv1 = as.double(c(1:len))
  gv2 = as.double(c(1:len))
  gv3 = as.double(c(1:len))
  gvectors = data.frame(gv1, gv2, gv3)
  
  lv1 = as.double(c(1:len))
  lv2 = as.double(c(1:len))
  lv3 = as.double(c(1:len))
  lvectors = data.frame(lv1, lv2, lv3)
  
  for (i in 1:len) {
    gravity[1] = alpha * gravity[1] + (1 - alpha) * ax[i]
    gravity[2] = alpha * gravity[2] + (1 - alpha) * ay[i]
    gravity[3] = alpha * gravity[3] + (1 - alpha) * az[i]
    
    gvectors[i, 1] = gravity[1]
    gvectors[i, 2] = gravity[2]
    gvectors[i, 3] = gravity[3]
    
    lvectors[i, 1] = ax[i] - gravity[1]
    lvectors[i, 2] = ay[i] - gravity[2]
    lvectors[i, 3] = az[i] - gravity[3]
  }
  return (list(gvectors, lvectors, combineVectors(lvectors)))
  
}


#验证数据
convertCoordinate = function (gv1, gv2, gv3, mg1, mg2, mg3, ac1, ac2, ac3) {
  Ax = gv1
  Ay = gv2
  Az = gv3
  Ex = mg1
  Ey = mg2
  Ez = mg3
  Hx = Ey * Az - Ez * Ay
  Hy = Ez * Ax - Ex * Az
  Hz = Ex * Ay - Ey * Ax
  normH = sqrt(Hx * Hx + Hy * Hy + Hz * Hz)
  invH = 1.0 / normH
  Hx = Hx * invH
  Hy = Hy * invH
  Hz = Hz * invH
  invA = 1.0 / sqrt(Ax * Ax + Ay * Ay + Az * Az)
  Ax = Ax * invA
  Ay = Ay * invA
  Az = Az * invA
  Mx = Ay * Hz - Az * Hy
  My = Az * Hx - Ax * Hz
  Mz = Ax * Hy - Ay * Hx
  rotateM = matrix(
    c(Hx,Hy,Hz, Mx,My,Mz, Ax,Ay,Az),nrow = 3, ncol = 3, byrow = T
  )
  newAccVector = solve(rotateM) %*% matrix(c(ac1, ac2, ac3))
  return(newAccVector)
}

multiConvert = function(gvectors, lvectors, mag, len) {
  c = data.frame()
  
  for (i in 1:len) {
    newAccVector = convertCoordinate(
      gvectors$gv1[i], gvectors$gv2[i], gvectors$gv3[i],
      mag$V1[i],mag$V2[i],mag$V3[i],
      lvectors$lv1[i], lvectors$lv2[i],lvectors$lv3[i]
    )
    #print(newAccVector)
    c = rbind(c , as.vector(newAccVector))
  }
  return(c)
}

plotAndCor2 = function(acc1, acc2, title1 = "") {
  matplot(data.frame(acc1,acc2),type = "l", xlab = "", ylab = "",lwd = 2)
  legend("topright",c("data1","data2"),lty = 1:2, col = 1:2)
  print(cor(acc1, acc2))
  corv = cor(acc1, acc2)
  title(main = paste(title1,"Cor:",round(corv,3)),xlab = "time", ylab = "magnitude")
  return(corv)
}

plotAndCor = function(acc1, acc2, title1 = "") {
  par(mfrow = c(3,1))
  corv = c(1:ncol(acc1))
  ts = c("x","y","z")
  for (i in 1:ncol(acc1)) {
    cor1 = cor(as.numeric(acc1[,i]), as.numeric(acc2[,i]))
    if(cor1 < 0){
      matplot(data.frame(as.numeric(acc1[,i]),-as.numeric(acc2[,i])),type = "l",ylab = "",lwd = 2)
      cor1 = -cor1
    }else{
      matplot(data.frame(as.numeric(acc1[,i]),as.numeric(acc2[,i])),type = "l",ylab = "", lwd = 2)
    }
    legend("topright",c("data1","data2"),lty = 1:2, col = 1:2)
    title(main = paste(title1, ts[i],"cor:",round(cor1,3)),xlab = "time", ylab = "magnitude")
    print(cor1)
    corv[i] =cor1
  }
  #title(paste(title1,"Cor:",round(corv,3)))
  return(corv)
}

combineVectors = function (data) {
  return(sqrt(as.numeric(data[,1]) ^2 +as.numeric(data[,2]) ^2  +as.numeric(data[,3]) ^2 ))
}

projection  = function(gx,gy,gz, ax,ay,az) {
  asbG = sqrt(gx * gx + gy * gy + gz * gz)
  return ((gx * ax + gy * ay + gz * az) / asbG)
}

carlibrate = function(fg, fl, theta = 0) {
  z = projection(fg[,1], fg[,2], fg[,3], fl[,1], fl[,2], fl[,3])
  cacc = combineVectors(fl)
  len_xy = sqrt(cacc^2 - z^2)
  x = len_xy*cos(theta)
  y = len_xy*sin(theta)
  return (data.frame(x,y,z))
}

vectorDist = function(v1, v2){
  sumV = 0
  for(i in 1:length(v1)){
    sumV = sumV + (v2[i]-v1[i])^2
  }
  return(sqrt(sumV))
}

plot3dDatas = function(d1, d2, t){
  d1 = data.frame(d1)
  d2 = data.frame(d2)
  colnames(d1) = 1:length(d1)
  colnames(d2) = 1:length(d2)
  d1$fac = factor(rep("A", nrow(d1)))
  d2$fac = factor(rep("B",nrow(d2)))
  d1 = data.frame(d1)
  d2 = data.frame(d2)
  open3d()
  dc = rbind(d1,d2)
  plot3d(dc[,1],dc[,2],dc[,3],col = as.numeric(dc$fac),type = "l",main = t,xlim = c(-15,15),ylim = c(-15,15),zlim=c(-15,15),xlab = "X",ylab = "Y",zlab = "Z",lwd = 2)
}
