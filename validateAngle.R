source("magcarlibrate.R")
source("function.R")
acc_6A = read.csv("./exp_6A/09_10_21_39_22_exp3_ACC.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_39_21_exp3_ACC.csv",header = F)
gyr_6A = read.csv("./exp_6A/09_10_21_39_22_exp3_GYR.csv",header = F)
gyr_88 = read.csv("./exp_88/09_10_21_39_21_exp3_GYR.csv",header = F)


if(nrow(acc_6A) > nrow(acc_88)){
  len = nrow(acc_88)
}else{
  len = nrow(acc_6A)
}
acc_6A = acc_6A[1:len,]
acc_88 = acc_88[1:len,]
gyr_6A = gyr_6A[1:(len-1),]
gyr_88 = gyr_88[1:(len-1),]


len = nrow(acc_6A)

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len )
angle_6A = computeAngle(fg6A[[1]])
angle_88= computeAngle(fg88[[1]])
sd(angle_6A[[2]])
sd(angle_88[[2]])

par(mfrow=c(2,1))

corv = plotAndCor(data.frame(angle_6A[[1]]),data.frame(angle_88[[1]]),"Pitch (X Angle)")

corv = plotAndCor(data.frame(angle_6A[[2]]),data.frame(angle_88[[2]]),"Roll (Y Angle)")

par(mfrow=c(3,1))
plotAndCor(data.frame(gyr_6A$V1),data.frame(gyr_88$V1),"GYR X")
plotAndCor(data.frame(gyr_6A$V2),data.frame(gyr_88$V2),"GYR Y")
plotAndCor(data.frame(gyr_6A$V3),data.frame(gyr_88$V3),"GYR Z")




