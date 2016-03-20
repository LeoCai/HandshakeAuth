library(rgl)



# (data_0_1 = readSingle(action, 0, downAngle ,5))
# (data_45_1 = readSingle(action, 45, downAngle ,8))
#
# dthetas_0_1 = getDeltaTheta(data_0_1)
# dthetas_45_1 = getDeltaTheta(data_45_1)

plot3DPhase = function (data, t) {
  r1 = cbind(data$V1,data$V6,data$V11)
  r2 = cbind(data$V2,data$V7,data$V12)
  r3 = cbind(data$V3,data$V8,data$V13)
  r4 = cbind(data$V4,data$V9,data$V14)
  r5 = cbind(data$V5,data$V10,data$V15)
  r1 = data.frame(r1)
  r2 = data.frame(r2)
  r3 = data.frame(r3)
  r4 = data.frame(r4)
  r5 = data.frame(r5)
  r1$fac = factor(rep("A", nrow(r1)))
  r2$fac = factor(rep("B", nrow(r2)))
  r3$fac = factor(rep("C", nrow(r3)))
  r4$fac = factor(rep("D", nrow(r4)))
  r5$fac = factor(rep("E", nrow(r5)))
  open3d()
  dc = rbind(r1,r2,r3,r4,r5)
  plot3d(dc[,1],dc[,2],dc[,3],col = as.numeric(dc$fac),type = "l",main = t)
}


