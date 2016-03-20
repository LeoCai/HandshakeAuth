source("DataLoader.R")
source("function.R")
source("model/GloableConvert.R")
file6A = "../handshake/1124/1124_ACC.csv"

acc_6A = read.csv(file_6A,header = F)

fileGYR6A = "../handshake/1124/1124_GYR.csv"

gyr_6A = read.csv(fileGYR6A,header = F)

len = nrow(acc_6A)

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
initMatrix = I

rtMats = updateMatrixByMultiGYR(gyr_6A, initMatrix ,0)

p1 = matrix(
  c(10,0,0),nrow = 3, ncol = 1, byrow = T
)
p2 = matrix(
  c(3,0,0),nrow = 3, ncol = 1, byrow = T
)
ps1 = data.frame()
ps1 = rbind(ps1, c(p1[1,1],p1[2,1],p1[3,1]))
ps2 = data.frame()
ps2 = rbind(ps2, c(p2[1,1],p2[2,1],p2[3,1]))
for(i in 1:nrow(rtMats)){
  rtMat = matrix(as.numeric(rtMats[i,]), nrow= 3, ncol = 3 )
  p = rtMat %*% p1
  ps1 = rbind(ps1,c(p[1,1],p[2,1],p[3,1]))
  p = rtMat %*% p2
  ps2 = rbind(ps2,c(p[1,1],p[2,1],p[3,1]))
}

open3d()
plot3d(ps1[,1],ps1[,2],ps1[,3],type = "p",ylim = c(-10,10) )

# for(i in 1:nrow(ps1)){
#   segments3d(c(1,ps1[i,1]),c(0,ps1[i,2]),c(0,ps1[i,3]),col=2,lwd=2)
# }






