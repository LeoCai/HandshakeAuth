source("DataLoader.R")
file_6A = "../0921/exp6A/09_21_14_33_06_exp1_ACC.csv"
file_88 = "../0921/exp88/01_26_05_06_25_exp1_ACC.csv"
expData = readExpData(file_6A ,file_88 )
lacc6A = expData$lacc6A
lacc88 = expData$lacc88


k6A = c(1:(nrow(lacc6A)-1))

for(i in 1:(nrow(lacc6A)-1)){
  k6A[i] = (as.double(vectorDist(lacc6A[i,],lacc6A[i+1,])))
}
plot(k6A[1:10],type = "l", col = rainbow(10))
tl6A = lacc6A[1:50,]
plot3d(tl6A[,1],tl6A[,2],tl6A[,3],type = "l", col = rainbow(10))

k88 = c(1:(nrow(lacc88)-1))

for(i in 1:(nrow(lacc88)-1)){
  k88[i] = (as.double(vectorDist(lacc88[i,],lacc88[i+1,])))
}
plotAndCor2(k6A,k88,"dist")
