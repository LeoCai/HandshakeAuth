acc_6A = read.csv("./exp_6A/09_10_21_42_21_exp5_ACC.csv",header = F)
mag_6A = read.csv("./exp_6A/09_10_21_42_21_exp5_MAG.csv",header = F)

acc_88 = read.csv("./exp_88/09_10_21_42_21_exp5_ACC.csv",header = F)
mag_88 = read.csv("./exp_88/09_10_21_42_21_exp5_MAG.csv",header = F)

sqrt(mag_88$V1 * mag_88$V1 + mag_88$V2 * mag_88$V2 + mag_88$V3 * mag_88$V3)

nrow(acc_88)
len = nrow(acc_6A)

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)

carlibrateACC66 = multiConvert(fg6A[[1]],fg6A[[2]],mag_6A,len)
carlibrateACC88 = multiConvert(fg88[[1]],fg88[[2]],mag_88,len)

plotAndCor(carlibrateACC66[1:20,], carlibrateACC88[1:20,])
plotAndCor(fg6A[[2]],fg88[[2]])
lacc6A = fg6A[[2]]
lacc88 = fg88[[2]]


plotAndCor(data.frame(combineVectors(fg6A[[2]][1:20,])),data.frame(combineVectors(fg88[[2]][1:20,])))
plotAndCor(data.frame(combineVectors(mag_6A[1:20,])), data.frame(combineVectors(mag_88[1:20,])))
plotAndCor(mag_6A[1:20,], mag_88[1:20,])

mag_88
mag_6A


#cor(carlibrateACC66[,1],carlibrateACC88[,1])
#cor(carlibrateACC66[,2],carlibrateACC88[,2])
#cor(carlibrateACC66[,3],carlibrateACC88[,3])


