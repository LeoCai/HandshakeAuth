source("magcarlibrate.R")
source("function.R")

par(mfrow=c(2,1))

acc_6A = read.csv("./exp_6A/09_10_21_42_29_exp5_ACC.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_42_29_exp5_ACC.csv",header = F)

if (nrow(acc_6A) > nrow(acc_88)) {
  len = nrow(acc_88)
}else{
  len = nrow(acc_6A)
}
acc_6A = acc_6A[2:len,]
acc_88 = acc_88[1:(len-1),]
mag_6A = mag_6A[1:len,]
mag_88 = mag_88[1:len,]

len = nrow(acc_6A)

fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)

proj_6A = projection(fg6A[[1]]$gv1, fg6A[[1]]$gv2, fg6A[[1]]$gv3, fg6A[[2]]$lv1, fg6A[[2]]$lv2, fg6A[[2]]$lv3)
proj_88 = projection(fg88[[1]]$gv1, fg88[[1]]$gv2, fg88[[1]]$gv3, fg88[[2]]$lv1, fg88[[2]]$lv2, fg88[[2]]$lv3)


corv = plotAndCor(data.frame(proj_6A),data.frame(proj_88),"gravity projection")

cbacc_6A = combineVectors(fg6A[[2]])
cbacc_88 = combineVectors(fg88[[2]])
corv = plotAndCor(data.frame(cbacc_6A),data.frame(cbacc_88),"combineAcc")
