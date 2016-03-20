source("./function.R")
library(zoo)

file_6A = "../handshake/1125/raw/acc_mi.csv"
file_88 = "../handshake/1125/raw/acc_sam.csv"


#5单位平滑，消除噪点
#已阈值消除微小移动
#用大于窗口平均值的点作为峰值
#寻找地一个峰值开始的点
detect = function(file_6A){
  acc_6A = read.csv(file_6A,header = F)
  acc_6A = acc_6A[1:200,]
  len = nrow(acc_6A)
  fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
  cacc6A = combineVectors(fg6A[[2]])
  # plot(cacc6A,type="l")
  plot(cacc6A,type = "l")
  varRoll = rollapply(cacc6A,10,sd, align = c("center"))
  plot(varRoll,type="l")
  meanRoll = rollapply(cacc6A,5,mean)
  peakRoll = rollapply(meanRoll, 5,
                       function(x){ xCenterIndex = (length(x)+1)/2;
                         
                         if(x[xCenterIndex] > 5&& x[xCenterIndex]>x[xCenterIndex-1] && x[xCenterIndex]>x[xCenterIndex+1]&& x[xCenterIndex] > (mean(x) + 0.5*sd(x)) ) return (x[xCenterIndex]) else return (0) }
                      )
  meanPeak = mean(peakRoll[peakRoll>0])
  sdPeak = sd(peakRoll[peakRoll>0])
  # plot(varRoll,type="l")
  plot(peakRoll,type="l")
  peakRoll = rollapply(peakRoll, 1, 
            function(x){ if(x>meanPeak) return(2) else if(x!=0) return (1) else return(0)  })
  # print(varRoll)
  peakIndexs = which(peakRoll>0)
  startIndex = peakIndexs[1]
  endIndex = peakIndexs[length(peakIndexs)]
  plot(peakRoll[startIndex:endIndex],type="l")
  
  
}
detect(file_6A)
detect(file_88)







