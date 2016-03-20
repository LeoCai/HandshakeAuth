source("DataLoader.R")

file_6A = "../0921/exp6A/09_21_14_32_22_exp1_ACC.csv"
file_88 = "../0921/exp88/01_26_05_05_40_exp1_ACC.csv"
expData = readExpData(file_6A,file_88)
summary(expData$lacc6A$lv2)
#summary(expData$lacc88$lv2)

file_6A = "../0921/exp6A/09_21_14_33_06_exp1_ACC.csv"
file_88 = "../0921/exp88/01_26_05_06_25_exp1_ACC.csv"
expData = readExpData(file_6A,file_88)
summary(expData$lacc6A$lv2)
#summary(expData$lacc88$lv2)

file_6A = "../0921/exp6A/09_21_14_36_41_exp1_ACC.csv"
file_88 = "../0921/exp88/01_26_05_09_59_exp1_ACC.csv"
expData = readExpData(file_6A,file_88)
summary(expData$lacc6A$lv2)
#summary(expData$lacc88$lv2)

file_6A = "../0921/exp6A/09_21_14_37_17_exp1_ACC.csv"
file_88 = "../0921/exp88/01_26_05_10_36_exp1_ACC.csv"
expData = readExpData(file_6A,file_88)
summary(expData$lacc6A$lv2)
#summary(expData$lacc88$lv2)
