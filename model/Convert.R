source("DataLoader.R")
source("model/GloableConvert.R")
# file6A = "./exp_24/exp6A/10_24_20_48_54_exp1_K_ACC.csv"
# file88 = "./exp_24/exp88/10_24_20_48_53_exp1-K_ACC.csv"

file6A = "../handshake/1118/acc_mi.csv"
file88 = "../handshake/1118/acc_sam.csv"
# file6A = "../handshake/1125/acc_mi.csv"
# file88 = "../handshake/1125/acc_sam.csv"

accData = readExpData(file6A, file88, 0, 0)

# fileGYR6A = "./exp_24/exp6A/10_24_20_48_54_exp1_K_GYR.csv"
# fileGYR88 = "./exp_24/exp88/10_24_20_48_53_exp1-K_GYR.csv"

fileGYR6A = "../handshake/1118/gyr_mi.csv"
fileGYR88 = "../handshake/1118/gyr_sam.csv"
# fileGYR6A = "../handshake/1125/gyr_mi.csv"
# fileGYR88 = "../handshake/1125/gyr_sam.csv"

gyrData = readGRYData(fileGYR6A, fileGYR88, 0, 0)

initTheta6A = 0

start = 10;
#len = nrow(gyrData$gyr_6A)
end = 30;

accData$fg6A

gaccInit6A = accData$fg6A[[1]][start,]
gaccInit88 = accData$fg88[[1]][start,]
gyr_6A = gyrData$gyr_6A[start:end, ]
gyr_88 = gyrData$gyr_88[start:end, ]
lacc6A = accData$lacc6A[start:end, ]
lacc88 = accData$lacc88[start:end, ]
cacc6A = accData$cacc6A[start:end]
cacc88 = accData$cacc88[start:end]
pg6A = accData$pg6A[start:end]
pg88 = accData$pg88[start:end]
caCor = cor(cacc6A,cacc88)
gpCor = cor(pg6A,pg88)

iterTheta = function(initTheta6A, initTheta88){
  
  initMatrix6A = getInitMatrix(gaccInit6A, initTheta6A)
  initMatrix88 = getInitMatrix(gaccInit88, initTheta88)
  
  (matrix6A = updateMatrixByMultiGYR(gyr_6A, initMatrix6A,0.02))
  (matrix88 = updateMatrixByMultiGYR(gyr_88, initMatrix88,0.02))
  
  accGloble6A = getGlobleAccs(lacc6A, matrix6A)
  accGloble88 = getGlobleAccs(lacc88, matrix88)
  
#   c1 = dtw(as.numeric(accGloble6A[,1]),as.numeric(accGloble88[,1]))$distance
#   c2 = dtw(as.numeric(accGloble6A[,2]),as.numeric(accGloble88[,2]))$distance
#   c3 = dtw(as.numeric(accGloble6A[,3]),as.numeric(accGloble88[,3]))$distance
  
  c1 = abs(cor(as.numeric(accGloble6A[,1]),as.numeric(accGloble88[,1])))
  c2 = abs(cor(as.numeric(accGloble6A[,2]),as.numeric(accGloble88[,2])))
  c3 = abs(cor(as.numeric(accGloble6A[,3]),as.numeric(accGloble88[,3])))
  print(paste(initTheta88,"sd=",sd(c(c1,c2,c3)), "mean=",mean(c(c1,c2,c3)),c1,c2,c3))
  return(list(cor = c(c1,c2,c3), globleAcc6A = accGloble6A, globleAcc88 = accGloble88, iterCor = c(c1,c2,c3)))
}

iterNum = 100;

iterCors = data.frame()
for (i in 1:iterNum){
  initTheta88 = 2*pi/iterNum*i
  rs = iterTheta(initTheta6A,initTheta88)
  iterCors = rbind(iterCors,rs$iterCor)
  # print(i)
  #plot3dDatas(rs$globleAcc6A,rs$globleAcc88,"")
}
# 
matplot(seq(0,2*pi, length.out = iterNum ),ylim = c(0,1),main="iterate theta similarity" ,lty = 1,lwd=2, iterCors,type = "l",col = 1:3, xlab = "InitTheta", ylab="Similarity")
legend("topright",c("x", "y","z"),lty = c(1,1,1),col=c(1,2,3) )
#abline(h = caCor, lty = 2, col = 1)
#abline(h = gpCor, lty = 2, col = 2)

# initTheta88 = 5.02654824574367
initTheta88 = 4.14690230273853
# initTheta88 = 4.27256600888212
# initTheta88 = 1.8
end = nrow(gyrData$gyr_6A)
gyr_6A = gyrData$gyr_6A[start:end, ]
gyr_88 = gyrData$gyr_88[start:end, ]
lacc6A = accData$lacc6A[start:end, ]
lacc88 = accData$lacc88[start:end, ]
cbacc6A = accData$cacc6A[start:end]
cbacc88 = accData$cacc88[start:end]
pg6A = accData$pg6A[start:end]
pg88 = accData$pg88[start:end]


rs = iterTheta(initTheta6A,initTheta88)

plot(lacc6A$lv1,type="l",lwd = 2)
plotAndCor(lacc6A,lacc88,"linear accelerometer")
plotAndCor2(cbacc6A,cbacc88,"resultant accelerometer")
plotAndCor2(pg6A,pg88,"projection on gravity direction")
plotAndCor(rs$globleAcc6A,rs$globleAcc88,"accelerometer after transformation")


ga6A = cbind(as.numeric(rs$globleAcc6A[,1]),-as.numeric(rs$globleAcc6A[,2]),as.numeric(rs$globleAcc6A[,3]))
plotAndCor(ga6A,rs$globleAcc88)

plot3dDatas(ga6A,rs$globleAcc88,"accelerometer after transformation")
# 
plot3dDatas(lacc6A,lacc88,"linear accelerometer")
# 
# initTheta88 = 2*pi/100*21
# rs = iterTheta(initTheta6A,initTheta88)
# plotAndCor2(as.numeric(rs$globleAcc6A[,1]),as.numeric(rs$globleAcc88[,1]),title1 = "Global Data")


