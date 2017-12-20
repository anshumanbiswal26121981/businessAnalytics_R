library(ggplot2)
library(scales)
library(dplyr)
library(reshape)
library(PerformanceAnalytics)
library(Hmisc)
library(caTools)
setwd("E:\\PGPBA\\PGPBA-GreatLakes\\Modules\\DataMining\\Assignment\\Assignment2")
hrdata = read.csv("HR_Employee_Attrition_Data.csv",
                  header = TRUE,
                  sep = ",")

hrdata = hrdata[, -c(9, 10, 22, 27)]
hrdata.transform = hrdata
str(hrdata)
# we see that there are a total of 2940 observations with 35 variables. There is no NA values in the data.Some variables needs to be converted to factor variables.
#Lets convert some variables into factor variable.
hrdata$Education <- as.factor(hrdata$Education)
hrdata$EnvironmentSatisfaction <-
  as.factor(hrdata$EnvironmentSatisfaction)
hrdata$JobInvolvement <- as.factor(hrdata$JobInvolvement)
hrdata$JobSatisfaction <- as.factor(hrdata$JobSatisfaction)
hrdata$PerformanceRating <- as.factor(hrdata$PerformanceRating)
hrdata$RelationshipSatisfaction <-
  as.factor(hrdata$RelationshipSatisfaction)
hrdata$WorkLifeBalance <- as.factor(hrdata$WorkLifeBalance)
hrdata$JobLevel <- as.factor(hrdata$JobLevel)
hrdata$StockOptionLevel <- as.factor(hrdata$StockOptionLevel)

str(hrdata)


#Neural net
set.seed(123)
hrdata.sample = hrdata


# Creating Development and Validation Sample
hrdata.sample$split <- runif(nrow(hrdata.sample), 0, 1)

hrdata.sample <- hrdata.sample[order(hrdata.sample$split), ]

#Now, if you view the hrdata.sample dataset again you would notice a new column added at the end
#Now, you can split the dataset to training and testing as follows
hrdata.train  <- hrdata.sample[which(hrdata.sample$split <= 0.7), ]
hrdata.test <- hrdata.sample[which(hrdata.sample$split > 0.7), ]
c(nrow(hrdata.train), nrow(hrdata.test))
#remove the split columns used for partitioning from both train and test data set
hrdata.train <- hrdata.train[, -c(32)]
hrdata.test <- hrdata.test[, -c(32)]
str(hrdata.train)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(quantile(hrdata.train$Age, probs = seq(0, 1, by = 0.10))), include.lowest = TRUE)
}
str(hrdata.train)
hrdata.new.train = hrdata.train
hrdata.train$AgeGroup = sapply(hrdata.train$Age, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(quantile(
    hrdata.train$DailyRate, probs = seq(0, 1, by = 0.10)
  )), include.lowest = TRUE)
}
hrdata.train$DailyRateGroup = sapply(hrdata.train$DailyRate, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$DistanceFromHome,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$DistanceFromHomeGroup = sapply(hrdata.train$DistanceFromHome, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$HourlyRate,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$HourlyRateGroup = sapply(hrdata.train$HourlyRate, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$MonthlyIncome,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$MonthlyIncomeGroup = sapply(hrdata.train$MonthlyIncome, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$MonthlyRate,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$MonthlyRateGroup = sapply(hrdata.train$MonthlyRate, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$NumCompaniesWorked,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$NumCompaniesWorkedGroup = sapply(hrdata.train$NumCompaniesWorked, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$PercentSalaryHike,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$PercentSalaryHikeGroup = sapply(hrdata.train$PercentSalaryHike, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$TotalWorkingYears,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$TotalWorkingYearsGroup = sapply(hrdata.train$TotalWorkingYears, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$TrainingTimesLastYear,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$TrainingTimesLastYearGroup = sapply(hrdata.train$TrainingTimesLastYear, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$YearsAtCompany,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$YearsAtCompanyGroup = sapply(hrdata.train$YearsAtCompany, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$YearsInCurrentRole,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$YearsInCurrentRoleGroup = sapply(hrdata.train$YearsInCurrentRole, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$YearsSinceLastPromotion,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$YearsSinceLastPromotionGroup = sapply(hrdata.train$YearsSinceLastPromotion, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$YearsWithCurrManager,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$YearsWithCurrManagerGroup = sapply(hrdata.train$YearsWithCurrManager, ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.train$YearsWithCurrManager,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.train$YearsWithCurrManagerGroup = sapply(hrdata.train$YearsWithCurrManager, ApplyQuantile)
str(hrdata.train)

backup.hrdata.train = hrdata.train
hrdata.train.allfactorvar = backup.hrdata.train[, unlist(lapply(backup.hrdata.train, is.factor))]
backup.hrdata.train.allfactorvar = hrdata.train.allfactorvar
str(hrdata.train.allfactorvar)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(quantile(hrdata.test$Age, probs = seq(0, 1, by = 0.10))), include.lowest = TRUE)
}
str(hrdata.test)
hrdata.new.test = hrdata.test
hrdata.test$AgeGroup = sapply(hrdata.test$Age, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(quantile(
    hrdata.test$DailyRate, probs = seq(0, 1, by = 0.10)
  )), include.lowest = TRUE)
}
hrdata.test$DailyRateGroup = sapply(hrdata.test$DailyRate, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$DistanceFromHome,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$DistanceFromHomeGroup = sapply(hrdata.test$DistanceFromHome, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$HourlyRate,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$HourlyRateGroup = sapply(hrdata.test$HourlyRate, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$MonthlyIncome,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$MonthlyIncomeGroup = sapply(hrdata.test$MonthlyIncome, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$MonthlyRate,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$MonthlyRateGroup = sapply(hrdata.test$MonthlyRate, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$NumCompaniesWorked,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$NumCompaniesWorkedGroup = sapply(hrdata.test$NumCompaniesWorked, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$PercentSalaryHike,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$PercentSalaryHikeGroup = sapply(hrdata.test$PercentSalaryHike, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$TotalWorkingYears,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$TotalWorkingYearsGroup = sapply(hrdata.test$TotalWorkingYears, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$TrainingTimesLastYear,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$TrainingTimesLastYearGroup = sapply(hrdata.test$TrainingTimesLastYear, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$YearsAtCompany,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$YearsAtCompanyGroup = sapply(hrdata.test$YearsAtCompany, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$YearsInCurrentRole,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$YearsInCurrentRoleGroup = sapply(hrdata.test$YearsInCurrentRole, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$YearsSinceLastPromotion,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$YearsSinceLastPromotionGroup = sapply(hrdata.test$YearsSinceLastPromotion, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$YearsWithCurrManager,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$YearsWithCurrManagerGroup = sapply(hrdata.test$YearsWithCurrManager, ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x, breaks = c(unique(
    quantile(
      hrdata.test$YearsWithCurrManager,
      probs = seq(0, 1, by = 0.10),
      na.rm = TRUE
    )
  )), include.lowest = TRUE)
}
hrdata.test$YearsWithCurrManagerGroup = sapply(hrdata.test$YearsWithCurrManager, ApplyQuantile)
str(hrdata.test)

backup.hrdata.test = hrdata.test
hrdata.test.allfactorvar = backup.hrdata.test[, unlist(lapply(backup.hrdata.test, is.factor))]
backup.hrdata.test.allfactorvar = hrdata.test.allfactorvar
str(hrdata.test.allfactorvar)
str(hrdata.train.allfactorvar)


#making the levels of some factors equal for test and train data
hrdata.test.allfactorvar$TotalWorkingYearsGroup <-
  as.character(hrdata.test.allfactorvar$TotalWorkingYearsGroup)
hrdata.test.allfactorvar$isTest <-
  rep(1, nrow(hrdata.test.allfactorvar))
hrdata.train.allfactorvar$isTest <-
  rep(0, nrow(hrdata.train.allfactorvar))

fullSet <- rbind(hrdata.test.allfactorvar, hrdata.train.allfactorvar)
fullSet$TotalWorkingYearsGroup <-
  as.factor(fullSet$TotalWorkingYearsGroup)

test.new <- fullSet[fullSet$isTest == 1, ]
test.new = test.new[, -32]
str(test.new)

train.new <- fullSet[fullSet$isTest == 0, ]
train.new = train.new[, -32]
str(train.new)

library(nnet)
## 1. Fit a Single Hidden Layer Neural Network using Least Squares
train.nnet <-
  nnet(
    Attrition ~ .,
    train.new,
    size = 3,
    rang = 0.07,
    Hess = FALSE,
    decay = 15e-4,
    maxit = 250
  )
## Use TEST data for testing the trained model
test.nnet <- predict(train.nnet, test.new, type = ("class"))
## MisClassification Confusion Matrix
table(test.new$Attrition, test.nnet)
## One can maximize the Accuracy by changing the "size" while training the neural network. SIZE refers to the number of nodes in the hidden layer.
which.is.max(test.nnet)  ## To Find which row break ties at random (Maximum position in vector)

##2. Use Multinomial Log Linear models using Neural Networks
train.mlln <- multinom(Attrition ~ ., train.new)
##USe TEST data for testing the trained model
test.mlln <- predict(train.mlln, test.new)
##Misclassification or Confusion Matrix
table(test.new$Attrition, test.mlln)

##3. Training Neural Network Using neuralnet
library(neuralnet)

## Check for all Input Independent Variables to be Integer or Numeric or complex matrix or vector arguments. If they are not any one of these, then tranform them accordingly
str(hrdata.train)
str(hrdata.test)

## It can be observed that all are either integer or factor. Now these factors have to be transformed to numeric.
## One cannot use directly as.numeric() to convert factors to numeric as it has limitations.
## First, Lets convert factors having character levels to numeric levels

hrdata.transform$Attrition = factor(
  hrdata.transform$Attrition,
  levels = c("Yes", "No"),
  labels = c(1, 0)
)
hrdata.transform$BusinessTravel = factor(
  hrdata.transform$BusinessTravel,
  levels = levels(hrdata.transform$BusinessTravel),
  labels = c(1, 2, 3)
)
hrdata.transform$Department = factor(
  hrdata.transform$Department,
  levels = levels(hrdata.transform$Department),
  labels = c(1, 2, 3)
)
hrdata.transform$EducationField = factor(
  hrdata.transform$EducationField,
  levels = levels(hrdata.transform$EducationField),
  labels = c(1, 2, 3, 4, 5, 6)
)
hrdata.transform$Gender = factor(
  hrdata.transform$Gender,
  levels = levels(hrdata.transform$Gender),
  labels = c(1, 2)
)
hrdata.transform$JobRole = factor(
  hrdata.transform$JobRole,
  levels = levels(hrdata.transform$JobRole),
  labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
)
hrdata.transform$MaritalStatus = factor(
  hrdata.transform$MaritalStatus,
  levels = levels(hrdata.transform$MaritalStatus),
  labels = c(1, 2, 3)
)
hrdata.transform$OverTime = factor(
  hrdata.transform$OverTime,
  levels = levels(hrdata.transform$OverTime),
  labels = c(1, 2)
)

## Now convert these numerical factors into numeric
hrdata.transform$Attrition <-
  as.numeric(as.character(hrdata.transform$Attrition))
hrdata.transform$BusinessTravel <-
  as.numeric(as.character(hrdata.transform$BusinessTravel))
hrdata.transform$Department <-
  as.numeric(as.character(hrdata.transform$Department))
hrdata.transform$EducationField <-
  as.numeric(as.character(hrdata.transform$EducationField))
hrdata.transform$Gender <-
  as.numeric(as.character(hrdata.transform$Gender))
hrdata.transform$JobRole <-
  as.numeric(as.character(hrdata.transform$JobRole))
hrdata.transform$MaritalStatus <-
  as.numeric(as.character(hrdata.transform$MaritalStatus))
hrdata.transform$OverTime <-
  as.numeric(as.character(hrdata.transform$OverTime))
hrdata.transform$PerformanceRating <-
  as.numeric(as.character(hrdata.transform$PerformanceRating))
hrdata.transform$RelationshipSatisfaction <-
  as.numeric(as.character(hrdata.transform$RelationshipSatisfaction))
hrdata.transform$StockOptionLevel <-
  as.numeric(as.character(hrdata.transform$StockOptionLevel))
hrdata.transform$WorkLifeBalance <-
  as.numeric(as.character(hrdata.transform$WorkLifeBalance))
hrdata.transform$Education <-
  as.numeric(as.character(hrdata.transform$Education))
hrdata.transform$WorkLifeBalance <-
  as.numeric(as.character(hrdata.transform$WorkLifeBalance))
hrdata.transform$EnvironmentSatisfaction <-
  as.numeric(as.character(hrdata.transform$EnvironmentSatisfaction))
hrdata.transform$JobInvolvement <-
  as.numeric(as.character(hrdata.transform$JobInvolvement))
hrdata.transform$JobLevel <-
  as.numeric(as.character(hrdata.transform$JobLevel))
hrdata.transform$JobSatisfaction <-
  as.numeric(as.character(hrdata.transform$JobSatisfaction))


str(hrdata.transform)
## Now all the variables are wither intergers or numeric
## Now we shall partition the data into train and test data
library(caret)
set.seed(1234567)
train2 <-
  createDataPartition(hrdata.transform$Attrition, p = 0.7, list = FALSE)
trainnew <- hrdata.transform[train2, ]
testnew <- hrdata.transform[-train2, ]
str(trainnew)
str(testnew)

str(hrdata)
#scale the data
x <- hrdata.transform[,-2]
nn.devscaled <- scale(x)

nn.devscaled <- cbind(hrdata.transform[2], nn.devscaled)

set.seed(891023)
train3 <-  createDataPartition(nn.devscaled$Attrition, p = 0.7, list = FALSE)
scaledTraindata <- nn.devscaled[train3, ]
scaledTestdata <- nn.devscaled[-train3, ]

## Now lets run the neuralnet model on Train dataset
trainnew.nnbp <-
  neuralnet(
    Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome +
      Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate +
      JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus +
      MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike +
      PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears +
      TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole +
      YearsSinceLastPromotion + YearsWithCurrManager,
    data = scaledTraindata,
    hidden = c(6,3),
    threshold = 0.01,
    err.fct = "sse",
    linear.output = FALSE,
    lifesign = "full",
    lifesign.step = 10,
    stepmax = 1e6
  )
summary(trainnew.nnbp)
## The distribution of the estimated results

quantile(trainnew.nnbp$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
##(Smoother the Curve- Better is the model prediction)
## Plot the trained Neural Network

plot(trainnew.nnbp, rep = "best")

## To check your prediction accuracy of training model
columnc = c(
  "Age",
  "BusinessTravel",
  "DailyRate",
  "Department",
  "DistanceFromHome",
  "Education",
  "EducationField",
  "EnvironmentSatisfaction",
  "Gender",
  "HourlyRate",
  "JobInvolvement",
  "JobLevel",
  "JobRole" ,
  "JobSatisfaction",
  "MaritalStatus",
  "MonthlyIncome" ,
  "MonthlyRate",
  "NumCompaniesWorked",
  "OverTime" ,
  "PercentSalaryHike",
  "PerformanceRating",
  "RelationshipSatisfaction",
  "StockOptionLevel",
  "TotalWorkingYears",
  "TrainingTimesLastYear",
  "WorkLifeBalance" ,
  "YearsAtCompany" ,
  "YearsInCurrentRole" ,
  "YearsSinceLastPromotion",
  "YearsWithCurrManager"
)


testnew2 <- subset(scaledTestdata, select = columnc)
testnew.nnbp <- compute(trainnew.nnbp, testnew2, rep = 1)
## MisClassification Confusion Matrix
table(testnew$Attrition, testnew.nnbp$net.result)
cbind(testnew$Attrition, testnew.nnbp$net.result)
print(testnew.nnbp)

## Error Computation
misClassTable = data.frame(Attrition = scaledTraindata$Attrition,
                           Predict.score = trainnew.nnbp$net.result[[1]])
misClassTable$Predict.class = ifelse(misClassTable$Predict.score > 0.21, 1, 0)
with(misClassTable, table(Attrition, Predict.class))

library(e1071)
confusionMatrix(misClassTable$Attrition, misClassTable$Predict.class)
sum((misClassTable$Attrition - misClassTable$Predict.score) ^ 2) / 2


decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
misClassTable$deciles <- decile(misClassTable$Predict.score)

## Ranking code
library(data.table)
tmp_DT = data.table(misClassTable)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 1)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp)
library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)
rank
#First decile has got 92% and is capturing 55% of total attrition value
#similalry second decile has got 40% and is capturing 79% of total attrition value


pr.nn1 <- compute(trainnew.nnbp,testnew2)

pr.nn1_ <- pr.nn1$net.result*(max(scaledTestdata$Attrition)-min(scaledTestdata$Attrition))+min(scaledTestdata$Attrition)
test.r1 <- (scaledTestdata$Attrition)*(max(scaledTestdata$Attrition)-min(scaledTestdata$Attrition))+min(scaledTestdata$Attrition)

MSE.nn1 <- sum((test.r1 - pr.nn1_)^2)/nrow(scaledTestdata)
MSE.nn1
# 0.08652548533

#Now lets change the model by selecting variables that are important as per random forest and lets see if the model improves or not

trainnew.nnbp1 <-
  neuralnet(
    Attrition ~ OverTime+EducationField+StockOptionLevel+PercentSalaryHike+JobRole+MonthlyIncome+YearsAtCompany+TotalWorkingYears+DistanceFromHome+Age+MonthlyRate+HourlyRate+DailyRate,
    data = scaledTraindata,
    hidden = c(6,3),
    threshold = 0.01,
    err.fct = "sse",
    linear.output = FALSE,
    lifesign = "full",
    lifesign.step = 10,
    stepmax = 1e6
  )
summary(trainnew.nnbp1)
plot(trainnew.nnbp1, rep = "best")

#Performance evaluation

misClassTable1 = data.frame(Attrition = scaledTraindata$Attrition,
                           Predict.score = trainnew.nnbp1$net.result[[1]])
misClassTable1$Predict.class = ifelse(misClassTable1$Predict.score > 0.21, 1, 0)
with(misClassTable1, table(Attrition, Predict.class))

confusionMatrix(misClassTable1$Attrition, misClassTable1$Predict.class)
#Error calculation
sum((misClassTable1$Attrition - misClassTable1$Predict.score) ^ 2) / 2



#decile
## deciling
misClassTable1$deciles <- decile(misClassTable1$Predict.score)

## Ranking code
tmp_DT = data.table(misClassTable1)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 1)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp)
library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)
rank
#First decile has rrrate of 80% and accounts for 48% of Attrition value
#Second decile has rrrate of 42% and accounts for 73% of Attrition value
#OverTime+EducationField+StockOptionLevel+PercentSalaryHike+JobRole+MonthlyIncome+YearsAtCompany+TotalWorkingYears+DistanceFromHome+Age+MonthlyRate+HourlyRate+DailyRate,
column=c(20,8,24,14,21,17,28,25,6,2,18,11,4)
pr.nn <- compute(trainnew.nnbp1,scaledTestdata[,column])

pr.nn_ <- pr.nn$net.result*(max(scaledTestdata$Attrition)-min(scaledTestdata$Attrition))+min(scaledTestdata$Attrition)
test.r <- (scaledTestdata$Attrition)*(max(scaledTestdata$Attrition)-min(scaledTestdata$Attrition))+min(scaledTestdata$Attrition)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(scaledTestdata)
MSE.nn
#we then compare the two MSEs


print(paste(MSE.nn1,MSE.nn))
save.image(file="hr_neuralnet.RData")