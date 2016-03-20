source("magcarlibrate.R")
source("function.R")
library(rgl)
acc_6A = read.csv("./exp_6A/09_10_21_42_17_exp5_ACC.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_42_16_exp5_ACC.csv",header = F)

if (nrow(acc_6A) > nrow(acc_88)) {
  len = nrow(acc_88)
}else{
  len = nrow(acc_6A)
}
acc_6A = acc_6A[2:len,]
acc_88 = acc_88[1:(len-1),]
len = nrow(acc_6A)

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)

ca_acc6A = carlibrate(fg6A[[1]],fg6A[[2]],theta = 124)
ca_acc88 = carlibrate(fg88[[1]],fg88[[2]],theta = 1)

plotAndCor2(ca_acc6A$z,ca_acc88$z,title1 = "z")
#plotAndCor2(ca_acc6A$x,ca_acc88$x,title1 = "x")
#plotAndCor2(ca_acc6A$y,ca_acc88$y,title1 = "y")

ca_acc6A = fg6A[[2]]
ca_acc88 = fg88[[2]]

ca_acc6A$fac = factor(rep("A", nrow(ca_acc6A)))
ca_acc88$fac = factor(rep("B",nrow(ca_acc88)))

d1 = data.frame(ca_acc6A)
d2 = data.frame(ca_acc88)
dc  = rbind(d1,d2)

plot3d(dc[,1],dc[,2],dc[,3],col = as.numeric(dc$fac),size = 5 , type = "l")
