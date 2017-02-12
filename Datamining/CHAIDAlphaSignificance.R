library(CHAID)
library(caTools)
setwd("D:\\PGPBA\\PGPBA-GreatLakes\\Modules\\DataMining\\discussion\\2")
getwd()

ustable = read.table("house-votes-84.data", dec=",")

f <-file("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data", open="r" ,encoding="UTF-8")
usdata <- read.csv(f, dec=",", header=F)
str(usdata)
write.csv(usdata, file = "usdata.csv")
withblankdata = read.csv("usdata.csv",header = TRUE,sep = ",",stringsAsFactors = TRUE)
#There are many ? values so replacing the ? with NA values and then would use MICE package to impute the NA values
str(withblankdata)
withblankdata[withblankdata==""]=c(NA)

library(mice)
tempData <- mice(withblankdata,m=5,maxit=50,meth='pmm',seed=500)
finalData = complete(tempData,1)
finalData=finalData[,-c(1)]
str(finalData)
write.csv(finalData, file = "finaldata.csv")
finalData = read.csv("finaldata.csv",header = TRUE,sep = ",",stringsAsFactors = TRUE)
finalData=finalData[,-c(1)]
summary(finalData)
attach(finalData)
usdata_chaid_ctrl = chaid_control(
  minbucket = 10, minsplit = 30, alpha2 = .05, alpha4 = .01
)
chaidtree.usdata = chaid(V1 ~ .,data = finalData, control = usdata_chaid_ctrl)
print(chaidtree.usdata)
plot(chaidtree.usdata)

usdata_chaid_ctrl1 = chaid_control(
  minbucket = 10, minsplit = 30, alpha2 = .01, alpha4 = .05
)
chaidtree.usdata1 = chaid(V1 ~ .,data = finalData, control = usdata_chaid_ctrl1)
print(chaidtree.usdata1)
plot(
  chaidtree.usdata1
)

usdata_chaid_ctrl2 = chaid_control(
  minbucket = 10, minsplit = 30, alpha2 = .05, alpha4 = .05,minprob = 0.1
)
chaidtree.usdata2 = chaid(V1 ~ .,data = finalData, control = usdata_chaid_ctrl2)
print(chaidtree.usdata2)
plot(
  chaidtree.usdata2
)