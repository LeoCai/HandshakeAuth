source("magcarlibrate.R")
source("function.R")

par(mfrow=c(3,1))

acc_6A = read.csv("./exp_6A/09_10_21_39_22_exp3_ACC.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_39_21_exp3_ACC.csv",header = F)

if (nrow(acc_6A) > nrow(acc_88)) {
  len = nrow(acc_88)
}else{
  len = nrow(acc_6A)
}
acc_6A = acc_6A[1:len,]
acc_88 = acc_88[1:len,]

len = nrow(acc_6A)

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)

plotAndCor(data.frame(fg6A[[2]]$lv1),data.frame(fg88[[2]]$lv1),title1 = "ACCX")
plotAndCor(data.frame(fg6A[[2]]$lv2),data.frame(fg88[[2]]$lv2),"ACCY")
plotAndCor(data.frame(fg6A[[2]]$lv3),data.frame(fg88[[2]]$lv3),"ACCZ")

summary(fg6A[[2]])
summary(fg88[[2]])

getGravityAngle = function(gx, gy, gz, lx, ly, lz){
  t1 = gx*lx + gy*ly + gz*lz
  t2 = sqrt(gx^2 + gy^2 + gz^2)
  t3 = sqrt(lx^2 + ly^2 + lz^2)
  return (acos(t1/(t2*t3)))
}
gangle6A = getGravityAngle(fg6A[[1]]$gv1,fg6A[[1]]$gv2,fg6A[[1]]$gv3, fg6A[[2]]$lv1, fg6A[[2]]$lv2, fg6A[[2]]$lv3)
gangle88 = getGravityAngle(fg88[[1]]$gv1,fg88[[1]]$gv2,fg88[[1]]$gv3, fg88[[2]]$lv1, fg88[[2]]$lv2, fg88[[2]]$lv3)
gangle6A[1] = 0
gangle88[1] = 0

corvs =plotAndCor(data.frame(gangle6A),data.frame(gangle88))
