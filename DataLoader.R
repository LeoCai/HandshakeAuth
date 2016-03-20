source("magcarlibrate.R")
source("function.R")

readExpData = function(file_6A, file_88,ahead = 0,behind = 0){
  acc_6A = read.csv(file_6A,header = F)
  acc_88 = read.csv(file_88,header = F)
  if (nrow(acc_6A) > nrow(acc_88)) {
    len = nrow(acc_88)
  }else{
    len = nrow(acc_6A)
  }
  acc_6A = acc_6A[(1+ahead):(len-behind),]
  acc_88 = acc_88[(1+behind):(len-ahead),]
  len = nrow(acc_6A)
  
  fg6A = filterGravity(acc_6A$V1,acc_6A$V2,acc_6A$V3,len)
  fg88 = filterGravity(acc_88$V1,acc_88$V2,acc_88$V3,len)
  cacc6A = combineVectors(fg6A[[2]])
  cacc88 = combineVectors(fg88[[2]])
  
  pg6A = projection(fg6A[[1]][,1],fg6A[[1]][,2],fg6A[[1]][,3],fg6A[[2]][,1],fg6A[[2]][,2],fg6A[[2]][,3])
  pg88 = projection(fg88[[1]][,1],fg88[[1]][,2],fg88[[1]][,3],fg88[[2]][,1],fg88[[2]][,2],fg88[[2]][,3])
  
  allData = list(acc_6A = acc_6A,acc_88=acc_88,fg6A=fg6A,fg88=fg88,cacc6A=cacc6A,cacc88=cacc88,pg6A=pg6A,pg88=pg88,lacc6A = fg6A[[2]],lacc88 = fg88[[2]])
  return(allData)
}

readGRYData = function(file_6A, file_88, ahead = 0, behind = 0){
  gyr_6A = read.csv(file_6A,header = F)
  gyr_88 = read.csv(file_88,header = F)
  if (nrow(gyr_6A) > nrow(gyr_88)) {
    len = nrow(gyr_88)
  }else{
    len = nrow(gyr_6A)
  }
  gyr_6A = gyr_6A[(1+ahead):(len-behind),]
  gyr_88 = gyr_88[(1+behind):(len-ahead),]
  len = nrow(gyr_6A)
  allData = list(gyr_6A = gyr_6A, gyr_88 = gyr_88)
  return(allData)
}

#expData = readExpData(file_6A, file_88)
