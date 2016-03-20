f1 = dir("exp_6A/")
f1
for(i in 1:length(f1)){
  file.rename(paste("exp_6A/",f1[i],sep = ""),paste("exp_6A/",i,substr(f1[i],16,1000),sep=""))
}
paste("exp_6A/",f1,sep = "")

f1 = dir("exp_88/")
for(i in 1:length(f1)){
  file.rename(paste("exp_88/",f1[i],sep = ""),paste("exp_88/",i,substr(f1[i],16,1000),sep=""))
}
paste("exp_88/",f1,sep = "")
