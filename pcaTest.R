source("DataLoader.R")

plot3dDatas = function(d1, d2, t){
  d1 = data.frame(d1)
  d2 = data.frame(d2)
  colnames(d1) = 1:length(d1)
  colnames(d2) = 1:length(d2)
  d1$fac = factor(rep("A", nrow(d1)))
  d2$fac = factor(rep("B",nrow(d2)))
  d1 = data.frame(d1)
  d2 = data.frame(d2)
  open3d()
  dc = rbind(d1,d2)
  plot3d(dc[,1],dc[,2],dc[,3],col = as.numeric(dc$fac),type = "l",title = t)
}

file_6A = "../0921/exp6A/09_21_14_37_17_exp1_ACC.csv"
file_88 = "../0921/exp88/01_26_05_09_59_exp1_ACC.csv"
expData = readExpData(file_6A,file_88,ahead = 0, behind = 1)


epca6A <- prcomp(expData$lacc6A,
                 center = TRUE,
                 scale. = FALSE) 
plot(epca6A,type="l")
summary(epca6A)
pre6A = predict(epca6A, newdata=expData$lacc6A)

epca88 <- prcomp(expData$lacc88,
               center = TRUE,
               scale. = FALSE) 
plot(epca88,type="l")
summary(epca88)
pre88 = predict(epca88, newdata=expData$lacc88)

plotAndCor2(pre6A[,1],pre88[,1],"x")
plotAndCor2(pre6A[,2],pre88[,2],"y")
plotAndCor2(pre6A[,3],pre88[,3],"z")

plotAndCor2(expData$lacc6A[,1],expData$lacc8[,1],"x")
plotAndCor2(expData$lacc6A[,2],expData$lacc8[,2],"y")
plotAndCor2(expData$lacc6A[,3],expData$lacc8[,3],"z")

#plot3dDatas(pre6A,pre88,"asda")
#plot3dDatas(expData$lacc6A,pre6A,"B _P")
#plot3dDatas(expData$lacc88,pre88,"B _P")

#plot3dDatas(pre6A,pre88,"PCA")
#plot3dDatas(expData$lacc6A,expData$lacc8,"BEFORE_PCA")

cd = data.frame()
for(i in 1:nrow(expData$lacc6A)){
    #print(expData$lacc6A[i,])
  cd = rbind(cd,as.vector(convertCoordinate(1,0,0,0,1,2,expData$lacc6A[i,1],expData$lacc6A[i,2],expData$lacc6A[i,3])))
}
colnames(cd) = c("X","Y","Z")
#plot3dDatas(expData$lacc6A,cd,"B _P")


hori6A = sqrt((expData$cacc6A)^2 - (expData$pg6A)^2)
hori88 = sqrt((expData$cacc88)^2 - (expData$pg88)^2)
plotAndCor2(hori6A,hori88)

plotAndCor2(hori6A,hori88, "hori")
plotAndCor2(expData$pg6A,expData$pg88, "Diffent Time Same People")
plotAndCor2(expData$cacc6A,expData$cacc88, "hori")
hori6A = expData$lacc6A - (expData$fg6A[[1]] / combineVectors(expData$fg6A[[1]]))*expData$pg6A
hori88 = expData$lacc88 - (expData$fg88[[1]] / combineVectors(expData$fg88[[1]]))*expData$pg88

epca6A <- prcomp(hori6A,
                 center = TRUE,
                 scale. = FALSE) 
plot(epca6A,type="l")
summary(epca6A)
pre6A = predict(epca6A, newdata=hori6A)

epca88 <- prcomp(hori88,
                 center = TRUE,
                 scale. = FALSE) 
plot(epca88,type="l")
summary(epca88)
pre88 = predict(epca88, newdata=hori88)
plotAndCor2(pre6A[,1],pre88[,1],"PCA")
plotAndCor2(pre6A[,2],pre88[,2],"PCA")
plotAndCor2(pre6A[,3],pre88[,3],"PCA")
plotAndCor2(hori6A[,1],hori88[,1],"BEFORE PCA")
plotAndCor2(hori6A[,2],hori88[,2],"BERFOE PCA")
plotAndCor2(hori6A[,3],hori88[,3],"BEFORE PCA")

plot3dDatas(d1 = hori6A,d2 = hori88 ,t = "")
plot3dDatas(d1 = pre6A,d2 = pre88 ,t = "")


plot3dDatas(d1 = expData$lacc6A,d2 = expData$lacc88 ,t = "")
#png(filename = "as.png",bg = "transparent")
#dev.off()
#rgl.postscript("persp3dd.pdf","pdf") 
#pre6A = predict(epca6A, newdata=expData$lacc6A)
#pre88 = predict(epca88, newdata=expData$lacc88)
#plot3dDatas(d1 = pre6A,d2 = pre88 ,t = "")








