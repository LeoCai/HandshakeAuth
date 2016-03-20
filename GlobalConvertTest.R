source("model/GloableConvert.R")
library(rgl)


ps = data.frame()
ps = rbind(points,c(1,2,0))
ps = rbind(points,c(2,6,1))
ps = rbind(points,c(3,3,3))
ps = rbind(points,c(-2,1,1))
ps = rbind(points,c(3,0,1))

gy = data.frame()
gy = rbind(gy,c(pi,0,0))
gy = rbind(gy,c(pi/2,0,0))
gy = rbind(gy,c(0,0,0))
gy = rbind(gy,c(0,0,0))
gy = rbind(gy,c(0,0,0))

open3d()
plot3d(ps[,1],ps[,2],ps[,3],type = "l")

rtMats = updateMatrixByMultiGYR(gy,I,1)
ps2 = data.frame()
for(i in 1:nrow(ps)){
  rtMat = matrix(as.numeric(rtMats[1,]), nrow= 3, ncol = 3 )
  p = rtMat %*% matrix(
    as.numeric(ps[i,]),nrow = 3, ncol = 1, byrow = T
  )
  ps2 = rbind(ps2, c(p[1,1],p[2,1],p[3,1]))
}

open3d()
plot3d(ps2[,1],ps2[,2],ps2[,3],type = "l")
