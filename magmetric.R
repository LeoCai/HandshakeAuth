source("magcarlibrate.R")
source("function.R")


acc_6A = read.csv("./exp_6A/09_10_21_42_25_exp5_ACC.csv",header = F)
mag_6A = read.csv("./exp_6A/09_10_21_39_17_exp3_MAG.csv",header = F)
gyr_6A = read.csv("./exp_6A/09_10_21_38_33_exp2_GYR.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_42_25_exp5_ACC.csv",header = F)
mag_88 = read.csv("./exp_88/09_10_21_39_16_exp3_MAG.csv",header = F)
gyr_88 = read.csv("./exp_88/09_10_21_38_32_exp2_GYR.csv",header = F)


if (nrow(acc_6A) > nrow(acc_88)) {
  len = nrow(acc_88)
}else{
  len = nrow(acc_6A)
}
acc_6A = acc_6A[2:len,]
acc_88 = acc_88[1:(len-1),]
mag_6A = mag_6A[1:len,]
mag_88 = mag_88[1:len,]
gyr_6A = gyr_6A[1:(len-1),]
gyr_88 = gyr_88[2:len,]

len = nrow(mag_6A)

par(mfrow=c(3,1))
corv = plotAndCor(data.frame(gyr_6A$V1),data.frame(gyr_88$V1),"GRYX")
corv = plotAndCor(data.frame(gyr_6A$V2),data.frame(gyr_88$V2),"GRYY")
corv = plotAndCor(data.frame(gyr_6A$V3),data.frame(gyr_88$V3),"GRYZ")


par(mfrow=c(3,1))
corv = plotAndCor(data.frame(mag_6A$V1),data.frame(mag_88$V1),"magX")
corv = plotAndCor(data.frame(mag_6A$V2),data.frame(mag_88$V2),"magY")
corv = plotAndCor(data.frame(mag_6A$V3),data.frame(mag_88$V3),"magZ")

par(mfrow=c(2,1))

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)

proj_6A = projection(fg6A[[1]]$gv1, fg6A[[1]]$gv2, fg6A[[1]]$gv3, fg6A[[2]]$lv1, fg6A[[2]]$lv2, fg6A[[2]]$lv3)
proj_88 = projection(fg88[[1]]$gv1, fg88[[1]]$gv2, fg88[[1]]$gv3, fg88[[2]]$lv1, fg88[[2]]$lv2, fg88[[2]]$lv3)
corv = plotAndCor(data.frame(proj_6A),data.frame(proj_88),"gravity projection")

cbacc_6A = combineVectors(fg6A[[2]])
cbacc_88 = combineVectors(fg88[[2]])
corv = plotAndCor(data.frame(cbacc_6A),data.frame(cbacc_88),"combineAcc")

library(signal)
bf = butter(9, 1/1.1, type="low")
proj_6A = filter(bf, proj_6A)
proj_88 = filter(bf, proj_88)
corv = plotAndCor(data.frame(proj_6A),data.frame(proj_88),"gravity projection lowpass")

bf = butter(8, 1/1.5, type="low")
cbacc_6A = filter(bf, cbacc_6A)
cbacc_88 = filter(bf, cbacc_88)
corv = plotAndCor(data.frame(cbacc_6A),data.frame(cbacc_88),"combineAcc LowPass")

