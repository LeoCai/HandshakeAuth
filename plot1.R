acc_6A = read.csv("./exp_6A/09_10_21_42_21_exp5_ACC.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_42_21_exp5_ACC.csv",header = F)
acc_6A
acc_88
acc_6A = acc_6A[2:30,]
acc_88 = acc_88[1:29,]

matplot(data.frame(acc_6A$V3,acc_88$V3),type="l")
cor(acc_6A$V3,acc_88$V3)

acc_6A = read.csv("./exp_6A/09_10_21_38_27_exp2_ACC.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_38_26_exp2_ACC.csv",header = F)
acc_6A
acc_88
#acc_6A = acc_6A[2:30,]
acc_88 = acc_88[2:41,]

matplot(data.frame(acc_6A$V1,acc_88$V1),type="l")
cor(acc_6A$V1,acc_88$V1)


acc_6A = read.csv("./exp_6A/09_10_21_38_27_exp2_MAG.csv",header = F)
acc_88 = read.csv("./exp_88/09_10_21_38_26_exp2_MAG.csv",header = F)
acc_6A
acc_88
acc_6A = acc_6A[1:40,]

#acc_88 = acc_88[2:41,]

matplot(data.frame(acc_6A$V1,acc_88$V1),type="l")
cor(acc_6A$V1,acc_88$V1)
fft(c(1,2,1))
plot(abs(fft(acc_6A$V1)),type="l")
