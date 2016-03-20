library(rgl)
acc_6A = read.csv("../0921/exp6A/09_21_14_32_22_exp1_ACC.csv",header = F)
acc_88 = read.csv("../0921/exp88/01_26_05_10_36_exp1_ACC.csv",header = F)
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
cacc6A = combineVectors(fg6A[[2]])
cacc88 = combineVectors(fg88[[2]])
fg6A
pg6A = projection(fg6A[[1]][,1],fg6A[[1]][,2],fg6A[[1]][,3],fg6A[[2]][,1],fg6A[[2]][,2],fg6A[[2]][,3])
pg88 = projection(fg88[[1]][,1],fg88[[1]][,2],fg88[[1]][,3],fg88[[2]][,1],fg88[[2]][,2],fg88[[2]][,3])
plotAndCor2(pg6A,pg88,title1 = "pg")

par(mfrow=c(1,1))
plotAndCor2(cacc6A,cacc88,title1 = "cacc")

fft6A = Mod(fft(cacc6A)[2:(length(cacc6A)/2)])
fft88 = Mod(fft(cacc88)[2:(length(cacc88)/2)])
plotAndCor2(fft6A,fft88,"fft")

fg6A[[2]]$fac = factor(rep("A", nrow(fg6A[[2]])))
fg88[[2]]$fac = factor(rep("B",nrow(fg88[[2]])))
d1 = data.frame(fg6A[[2]])
d2 = data.frame(fg88[[2]])
dc  = d2
open3d()
plot3d(dc[,1],dc[,2],dc[,3],col = rainbow(nrow(dc)),type = "l")




fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)[[1]]
fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)[[z1]]

fg6A$fac = factor(rep("A", nrow(fg6A)))
fg88$fac = factor(rep("B",nrow(fg88)))
d1 = data.frame(fg6A)
d2 = data.frame(fg88)
dc  = rbind(d1,d2)


open3d()
plot3d(dc$gv1,dc$gv2,dc$gv3,col = as.numeric(dc$fac),type = "l")
