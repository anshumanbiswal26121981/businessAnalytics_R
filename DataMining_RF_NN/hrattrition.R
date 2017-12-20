library(ggplot2)
library(scales)
library(dplyr)
library(reshape)
library(PerformanceAnalytics)
library(Hmisc)
library(caTools)
setwd("E:\\PGPBA\\PGPBA-GreatLakes\\Modules\\DataMining\\Assignment\\Assignment2")
hrdata = read.csv("HR_Employee_Attrition_Data.csv",header = TRUE,sep = ",")
dim(hrdata)
str(hrdata)

#'data.frame':	2940 obs. of  35 variables:
# $ Age                     : int  41 49 37 33 27 32 59 30 38 36 ...
# $ Attrition               : Factor w/ 2 levels "No","Yes": 2 1 2 1 1 1 1 1 1 1 ...
# $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 2 3 2 3 2 3 3 2 3 ...
# $ DailyRate               : int  1102 279 1373 1392 591 1005 1324 1358 216 1299 ...
# $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 2 2 2 2 2 2 2 2 ...
# $ DistanceFromHome        : int  1 8 2 3 2 2 3 24 23 27 ...
# $ Education               : int  2 1 2 4 1 2 3 1 3 3 ...
# $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 2 5 2 4 2 4 2 2 4 ...
# $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ EmployeeNumber          : int  1 2 3 4 5 6 7 8 9 10 ...
# $ EnvironmentSatisfaction : int  2 3 4 4 1 4 3 4 4 3 ...
# $ Gender                  : Factor w/ 2 levels "Female","Male": 1 2 2 1 2 2 1 2 2 2 ...
# $ HourlyRate              : int  94 61 92 56 40 79 81 67 44 94 ...
# $ JobInvolvement          : int  3 2 2 3 3 3 4 3 2 3 ...
# $ JobLevel                : int  2 2 1 1 1 1 1 1 3 2 ...
# $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 7 3 7 3 3 3 3 5 1 ...
# $ JobSatisfaction         : int  4 2 3 3 2 4 1 3 3 3 ...
# $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 3 2 3 2 2 3 2 1 3 2 ...
# $ MonthlyIncome           : int  5993 5130 2090 2909 3468 3068 2670 2693 9526 5237 ...
# $ MonthlyRate             : int  19479 24907 2396 23159 16632 11864 9964 13335 8787 16577 ...
# $ NumCompaniesWorked      : int  8 1 6 1 9 0 4 1 0 6 ...
# $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ OverTime                : Factor w/ 2 levels "No","Yes": 2 1 2 2 1 1 2 1 1 1 ...
# $ PercentSalaryHike       : int  11 23 15 11 12 13 20 22 21 13 ...
# $ PerformanceRating       : int  3 4 3 3 3 3 4 4 4 3 ...
# $ RelationshipSatisfaction: int  1 4 2 3 4 3 1 2 2 2 ...
# $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
# $ StockOptionLevel        : int  0 1 0 0 1 0 3 1 0 2 ...
# $ TotalWorkingYears       : int  8 10 7 8 6 8 12 1 10 17 ...
# $ TrainingTimesLastYear   : int  0 3 3 3 3 2 3 2 2 3 ...
# $ WorkLifeBalance         : int  1 3 3 3 3 2 2 3 3 2 ...
# $ YearsAtCompany          : int  6 10 0 8 2 7 1 1 9 7 ...
# $ YearsInCurrentRole      : int  4 7 0 7 2 7 0 0 7 7 ...
# $ YearsSinceLastPromotion : int  0 1 0 3 2 3 0 0 1 7 ...
# $ YearsWithCurrManager    : int  5 7 0 0 2 6 0 0 8 7 ...


# we see that there are a total of 2940 observations with 35 variables. There is no NA values in the data.Some variables needs to be converted to factor variables.
#Lets convert some variables into factor variable.
hrdata = hrdata[,-c(9,10,27)]
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

# 'data.frame':	2940 obs. of  35 variables:
#   $ Age                     : int  41 49 37 33 27 32 59 30 38 36 ...
# $ Attrition               : Factor w/ 2 levels "No","Yes": 2 1 2 1 1 1 1 1 1 1 ...
# $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 2 3 2 3 2 3 3 2 3 ...
# $ DailyRate               : int  1102 279 1373 1392 591 1005 1324 1358 216 1299 ...
# $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 2 2 2 2 2 2 2 2 ...
# $ DistanceFromHome        : int  1 8 2 3 2 2 3 24 23 27 ...
# $ Education               : Factor w/ 5 levels "1","2","3","4",..: 2 1 2 4 1 2 3 1 3 3 ...
# $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 2 5 2 4 2 4 2 2 4 ...
# $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ EmployeeNumber          : int  1 2 3 4 5 6 7 8 9 10 ...
# $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 2 3 4 4 1 4 3 4 4 3 ...
# $ Gender                  : Factor w/ 2 levels "Female","Male": 1 2 2 1 2 2 1 2 2 2 ...
# $ HourlyRate              : int  94 61 92 56 40 79 81 67 44 94 ...
# $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 2 2 3 3 3 4 3 2 3 ...
# $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 2 1 1 1 1 1 1 3 2 ...
# $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 7 3 7 3 3 3 3 5 1 ...
# $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 2 3 3 2 4 1 3 3 3 ...
# $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 3 2 3 2 2 3 2 1 3 2 ...
# $ MonthlyIncome           : int  5993 5130 2090 2909 3468 3068 2670 2693 9526 5237 ...
# $ MonthlyRate             : int  19479 24907 2396 23159 16632 11864 9964 13335 8787 16577 ...
# $ NumCompaniesWorked      : int  8 1 6 1 9 0 4 1 0 6 ...
# $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
# $ OverTime                : Factor w/ 2 levels "No","Yes": 2 1 2 2 1 1 2 1 1 1 ...
# $ PercentSalaryHike       : int  11 23 15 11 12 13 20 22 21 13 ...
# $ PerformanceRating       : Factor w/ 2 levels "3","4": 1 2 1 1 1 1 2 2 2 1 ...
# $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 1 4 2 3 4 3 1 2 2 2 ...
# $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
# $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 1 2 1 1 2 1 4 2 1 3 ...
# $ TotalWorkingYears       : int  8 10 7 8 6 8 12 1 10 17 ...
# $ TrainingTimesLastYear   : int  0 3 3 3 3 2 3 2 2 3 ...
# $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 1 3 3 3 3 2 2 3 3 2 ...
# $ YearsAtCompany          : int  6 10 0 8 2 7 1 1 9 7 ...
# $ YearsInCurrentRole      : int  4 7 0 7 2 7 0 0 7 7 ...
# $ YearsSinceLastPromotion : int  0 1 0 3 2 3 0 0 1 7 ...
# $ YearsWithCurrManager    : int  5 7 0 0 2 6 0 0 8 7 ...

sapply(hrdata, is.numeric)
# Age                 Attrition           BusinessTravel                DailyRate
# TRUE                    FALSE                    FALSE                     TRUE
# Department         DistanceFromHome                Education           EducationField
# FALSE                     TRUE                    FALSE                    FALSE
# EmployeeCount           EmployeeNumber  EnvironmentSatisfaction           Gender
# TRUE                     TRUE                    FALSE                    FALSE
# HourlyRate           JobInvolvement                 JobLevel              JobRole
# TRUE                    FALSE                    FALSE                    FALSE
# JobSatisfaction            MaritalStatus            MonthlyIncome         MonthlyRate
# FALSE                    FALSE                     TRUE                     TRUE
# NumCompaniesWorked      Over18                 OverTime        PercentSalaryHike
# TRUE                    FALSE                    FALSE                     TRUE
# PerformanceRating RelationshipSatisfaction            StandardHours         StockOptionLevel
# FALSE                    FALSE                     TRUE                    FALSE
# TotalWorkingYears    TrainingTimesLastYear          WorkLifeBalance           YearsAtCompany
# TRUE                     TRUE                    FALSE                     TRUE
# YearsInCurrentRole  YearsSinceLastPromotion     YearsWithCurrManager
# TRUE                     TRUE                     TRUE

summary(hrdata)
counts <- table(hrdata$Department)
bp = barplot(counts, main="Employee distribution by department", 
        xlab="Number of Employees")
## Add text at top of bars
text(x = bp, y = counts, label = counts, pos = 1, cex = 0.8, col = "red")
## Add x-axis labels 

countsAttrition <- table(hrdata$Attrition)
bpAttr = barplot(countsAttrition, main="Attrition Count", 
             xlab="Count")
## Add text at top of bars
text(x = bpAttr, y = countsAttrition, label = countsAttrition, pos = 1, cex = 0.8, col = "red")

  countsbygender <- table(hrdata$Gender)
  bpSex = barplot(countsbygender, main="Gender Count", xlab="Count")
  ## Add text at top of bars
  text(x = bpSex, y = countsbygender, label = countsbygender, pos = 1, cex = 0.8, col = "red")

  library(plotrix)
  slices <- table(hrdata$Gender) 
  lbls <- names(slices)
  pct <- prop.table(slices)*100
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie3D(slices,labels = lbls,explode = 0.1, main="gender ratio")
  
  countsbyBusinessTravle <- table(hrdata$BusinessTravel)
  bpBt = barplot(countsbyBusinessTravle, main="Count by Business Travel", xlab="Count")
  ## Add text at top of bars
  text(x = bpBt, y = countsbyBusinessTravle, label = countsbyBusinessTravle, pos = 1, cex = 0.8, col = "red")
  lbls <- names(countsbyBusinessTravle)
  pct <- round(prop.table(countsbyBusinessTravle)*100,1)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie3D(countsbyBusinessTravle,labels = lbls,explode = 0.1, main="Distribution by Business travel")
  

  countsbyEF <- table(hrdata$EducationField)
  bpBt = barplot(countsbyEF, main="Count by Education field", xlab="Count",las=2,cex.names = 0.5,beside = TRUE)
  ## Add text at top of bars
  text(x = bpBt, y = countsbyEF, label = countsbyEF, pos = 1, cex = 0.5, col = "red")
  
  lbls <- names(countsbyEF)
  pct <- round(prop.table(countsbyEF)*100,1)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  oldpar=par()
  par(cex=0.3)
  pie3D(countsbyEF,labels = lbls,explode = 0.1, main="Distribution by Education field")
  dev.off()
  par(oldpar)
  
  summary(hrdata$EducationField)
  
  countsbyjobrole <- table(hrdata$JobRole)
  bpJr = barplot(countsbyjobrole, main="Count by Job Role", xlab="Count",las=2,cex.names = 0.5,beside = TRUE)
  ## Add text at top of bars
  text(x = bpJr, y = countsbyjobrole, label = countsbyjobrole, pos = 1, cex = 0.5, col = "red")
  
  lbls <- names(countsbyjobrole)
  pct <- round(prop.table(countsbyjobrole)*100,1)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  oldpar=par()
  par(cex=0.3)
  pie3D(countsbyjobrole,labels = lbls,explode = 0.1, main="Distribution by Job Role")
  dev.off()
  par(oldpar)
  summary(hrdata$JobRole)
  
  distrByMaritalStat <- table(hrdata$MaritalStatus)
  lbls <- names(distrByMaritalStat)
  pct <- round(prop.table(distrByMaritalStat)*100,1)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  oldpar=par()
  par(cex=0.3)
  pie3D(distrByMaritalStat,labels = lbls,explode = 0.1, main="Distribution by Marital Status")
  dev.off()
  par(oldpar)
  
  # Age           Attrition            BusinessTravel   DailyRate                         Department   DistanceFromHome
# Min.   :18.00   No :2466   Non-Travel       : 300   Min.   : 102.0   Human Resources       : 126   Min.   : 1.000
# 1st Qu.:30.00   Yes: 474   Travel_Frequently: 554   1st Qu.: 465.0   Research & Development:1922   1st Qu.: 2.000
# Median :36.00              Travel_Rarely    :2086   Median : 802.0   Sales                 : 892   Median : 7.000
# Mean   :36.92                                       Mean   : 802.5                                 Mean   : 9.193
# 3rd Qu.:43.00                                       3rd Qu.:1157.0                                 3rd Qu.:14.000
# Max.   :60.00                                       Max.   :1499.0                                 Max.   :29.000
#
# Education          EducationField EmployeeCount EmployeeNumber   EnvironmentSatisfaction    Gender
# 1: 340    Human Resources :  54   Min.   :1     Min.   :   1.0   1:568                   Female:1176
# 2: 564    Life Sciences   :1212   1st Qu.:1     1st Qu.: 735.8   2:574                   Male  :1764
# 3:1144    Marketing       : 318   Median :1     Median :1470.5   3:906
# 4: 796    Medical         : 928   Mean   :1     Mean   :1470.5   4:892
# 5:  96    Other           : 164   3rd Qu.:1     3rd Qu.:2205.2
#           Technical Degree: 264   Max.   :1     Max.   :2940.0
#
# HourlyRate     JobInvolvement JobLevel                      JobRole    JobSatisfaction  MaritalStatus
# Min.   : 30.00   1: 166         1:1086   Sales Executive          :652   1:578           Divorced: 654
# 1st Qu.: 48.00   2: 750         2:1068   Research Scientist       :584   2:560           Married :1346
# Median : 66.00   3:1736         3: 436   Laboratory Technician    :518   3:884           Single  : 940
# Mean   : 65.89   4: 288         4: 212   Manufacturing Director   :290   4:918
# 3rd Qu.: 84.00                  5: 138   Healthcare Representative:262
# Max.   :100.00                           Manager                  :204
#                                         (Other)                   :430
# MonthlyIncome    MonthlyRate    NumCompaniesWorked Over18   OverTime   PercentSalaryHike PerformanceRating
# Min.   : 1009   Min.   : 2094   Min.   :0.000      Y:2940   No :2108   Min.   :11.00     3:2488
# 1st Qu.: 2911   1st Qu.: 8045   1st Qu.:1.000               Yes: 832   1st Qu.:12.00     4: 452
# Median : 4919   Median :14236   Median :2.000                          Median :14.00
# Mean   : 6503   Mean   :14313   Mean   :2.693                          Mean   :15.21
# 3rd Qu.: 8380   3rd Qu.:20462   3rd Qu.:4.000                          3rd Qu.:18.00
# Max.   :19999   Max.   :26999   Max.   :9.000                          Max.   :25.00
#
# RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear WorkLifeBalance
# 1:552                    Min.   :80    0:1262           Min.   : 0.00     Min.   :0.000         1: 160
# 2:606                    1st Qu.:80    1:1192           1st Qu.: 6.00     1st Qu.:2.000         2: 688
# 3:918                    Median :80    2: 316           Median :10.00     Median :3.000         3:1786
# 4:864                    Mean   :80    3: 170           Mean   :11.28     Mean   :2.799         4: 306
#                          3rd Qu.:80                     3rd Qu.:15.00     3rd Qu.:3.000
#                          Max.   :80                     Max.   :40.00     Max.   :6.000
#
# YearsAtCompany   YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager
# Min.   : 0.000   Min.   : 0.000     Min.   : 0.000          Min.   : 0.000
# 1st Qu.: 3.000   1st Qu.: 2.000     1st Qu.: 0.000          1st Qu.: 2.000
# Median : 5.000   Median : 3.000     Median : 1.000          Median : 3.000
# Mean   : 7.008   Mean   : 4.229     Mean   : 2.188          Mean   : 4.123
# 3rd Qu.: 9.000   3rd Qu.: 7.000     3rd Qu.: 3.000          3rd Qu.: 7.000
# Max.   :40.000   Max.   :18.000     Max.   :15.000          Max.   :17.000

age = hrdata[,1]
boxplot(age,main = "box plot of age")
hist(age,main = "histogram of the age")

dailyrate = hrdata[,4]
boxplot(age,main = "box plot of daily rate")
hist(age,main = "histogram of the daily rate")

distanceFromHome = hrdata[,6]
boxplot(distanceFromHome,main = "box plot of distance from home")
hist(distanceFromHome,main = "histogram of the distance from home")

hourlyrate = hrdata[,13]
boxplot(hourlyrate,main = "box plot of hourly rate")
hist(hourlyrate,main = "histogram of hourly rate")

monthlyincome = hrdata[,19]
boxplot(monthlyincome,main = "box plot of monthly income")
hist(monthlyincome,main = "histogram of monthly income")

monthlyrate = hrdata[,20]
boxplot(monthlyrate,main = "box plot of monthly rate")
hist(monthlyrate,main = "histogram of monthly rate")

numberOfCompaniesWorked = hrdata[,21]
boxplot(numberOfCompaniesWorked,main = "box plot of no.of companies worked")
hist(numberOfCompaniesWorked,main = "histogram of no.of companies worked")


#Hypothesis
#Ho= Job role doesnot affect the Attrition or Attrition is independent of jobrole or there is no strong corelation between jobrole and Attrition
#Ha= There is a strong corelation between Attrition and JobRole or Attrion and job role are not independent or job role affect the attrition rate
library("MASS")
tbl.jrattr=table(hrdata$JobRole,hrdata$Attrition)
chisq.test(tbl.jrattr)
# Pearson's Chi-squared test
# 
# data:  tbl.jrattr
# X-squared = 172.38, df = 8, p-value < 2.2e-16
#Since p<0.05 so we reject null and accept alternate hypothesis.We conclude that there is strong corelation between Attrition and job role

#Attrition rate across job roles
ggplot(hrdata, aes(x = JobRole, fill = Attrition)) + stat_count(width = 0.5) +
  xlab("Job Role") + ylab("Count") + labs(fill = "Attrition")
ggplot(hrdata, aes(x = JobRole)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())


#Ho= Job level doesnot affect the Attrition or Attrition is independent of job level or there is no strong corelation between joblevel and Attrition
#Ha= There is a strong corelation between Attrition and JobLevel or Attrion and job level are not independent or job level affect the attrition rate

tbl.jlattr=table(hrdata$Attrition,hrdata$JobLevel)
chisq.test(tbl.jlattr)
# Pearson's Chi-squared test
# 
# data:  tbl.jlattr
# X-squared = 145.06, df = 4, p-value < 2.2e-16

#Attrition rate across job labels
ggplot(hrdata, aes(x = JobLevel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())


#Ho= Overtime doesnot affect the Attrition or Attrition is independent of Overtime or there is no strong corelation between Overtime and Attrition
#Ha= There is a strong corelation between Attrition and Overtime or Attrion and Overtime are not independent or Overtime affect the attrition rate

tbl.otattr=table(hrdata$OverTime,hrdata$Attrition)
chisq.test(tbl.otattr)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tbl.otattr
# X-squared = 176.61, df = 1, p-value < 2.2e-16
#p value is less than 0.05 and hence we reject the null and accept the alt. hypothesis
#Attrition rate by overtime value
ggplot(hrdata, aes(x = OverTime)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())


#Ho= Job involvement doesnot affect the Attrition or Attrition is independent of JobInvolvement or there is no strong corelation between JobInvolvement and Attrition
#Ha= There is a strong corelation between Attrition and JobInvolvement or Attrion and JobInvolvement are not independent or JobInvolvement affect the attrition rate
tbl.jinattr=table(hrdata$Attrition,hrdata$JobInvolvement)
tbl.jinattr
chisq.test(tbl.jinattr)
#p value is less than 0.05 and hence we reject the null and accept the alt. hypothesis

#Attrition rate by JobInvolvement
ggplot(hrdata, aes(x = JobInvolvement)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

#Ho= Business Travel doesnot affect the Attrition or Attrition is independent of Business Travel or there is no strong corelation between Business Travel and Attrition
#Ha= There is a strong corelation between Attrition and Business Travel or Attrion and Business Travel are not independent or Business Travel affect the attrition rate
tbl.btnattr=table(hrdata$Attrition,hrdata$BusinessTravel)
print(chisq.test(tbl.btnattr))
#p value is less than 0.05 and hence we reject the null and accept the alt. hypothesis

#Attrition rate by BusinessTravel
ggplot(hrdata, aes(x = BusinessTravel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())


##Attrition rate by Business Travel and Jobrole
library(reshape)
hrdata.m = melt(hrdata)
ggplot(hrdata.m, aes(x = BusinessTravel)) + geom_bar(aes(fill = Attrition),position = 'fill') +
  scale_y_continuous(labels = percent_format()) + facet_wrap( ~ JobRole)

##Attrition rate by overtime and Jobrole
ggplot(hrdata.m, aes(x = OverTime)) + geom_bar(aes(fill = Attrition),position = 'fill') +
  scale_y_continuous(labels = percent_format()) + facet_wrap( ~ JobRole)

##Attrition rate by Worklife balance and Jobrole
ggplot(hrdata.m, aes(x = WorkLifeBalance)) + geom_bar(aes(fill = Attrition),position = 'fill') +
  scale_y_continuous(labels = percent_format()) + facet_wrap( ~ JobRole)


# Code: standard deviation to determine if we need to scale
sapply(hrdata, sd)

# Age                      Attrition           BusinessTravel                DailyRate
# 9.1338192                0.3678004                0.6653417              403.4404468
# Department         DistanceFromHome                Education           EducationField
# 0.5277025                8.1054851                1.0239907                1.3311426
# EmployeeCount           EmployeeNumber  EnvironmentSatisfaction               Gender
# 0.0000000              848.8492210                1.0928962                0.4899813
# HourlyRate           JobInvolvement                 JobLevel                  JobRole
# 20.3259687                0.7114401                1.1067516                2.4614024
# JobSatisfaction            MaritalStatus            MonthlyIncome         MonthlyRate
# 1.1026585                0.7299965             4707.1557696             7116.5750213
# NumCompaniesWorked          Over18                 OverTime        PercentSalaryHike
# 2.4975840                0.0000000                0.4505298                3.6593150
# PerformanceRating RelationshipSatisfaction        StandardHours         StockOptionLevel
# 0.3607621                1.0810249                0.0000000                0.8519317
# TotalWorkingYears    TrainingTimesLastYear          WorkLifeBalance     YearsAtCompany
# 7.7794579                1.2890513                0.7063556                6.1254828
# YearsInCurrentRole  YearsSinceLastPromotion     YearsWithCurrManager
# 3.6225206                3.2218820                3.5675290

#We see significant difference in SD in our variables so lets scale the data
backup = hrdata
numeric.columns = backup[,unlist(lapply(backup,is.numeric))]
scaled.numeric.columns = scale(numeric.columns)
backup[,unlist(lapply(backup,is.numeric))] =  scaled.numeric.columns
hrdatascaled = backup
hrdatascaled.clean = hrdatascaled[,-c(9,27)]
hrdatascaled.clean.numericonly = hrdatascaled.clean[,unlist(lapply(hrdatascaled.clean,is.numeric))]

# Code: get the most highly correlated variables
hrdataHighCorr <- function(df)
{
  # find the correlations
  cor.matrix <- cor(df)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cor.matrix) <- 0
  cor.matrix[lower.tri(cor.matrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cor.matrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  df = fm[order(abs(fm$Correlation),decreasing = T),]
  res = subset(df, ((df$Correlation >= 0.5 &
                       df$Correlation <= 1) |
                      (df$Correlation >= -1 & df$Correlation <= -0.5)
  ))
  res
}
hrdataHighCorr(numeric.columns)
#               First.Variable         Second.Variable Correlation
# 194           MonthlyIncome       TotalWorkingYears   0.7728932
# 286          YearsAtCompany    YearsWithCurrManager   0.7692124
# 252          YearsAtCompany      YearsInCurrentRole   0.7587537
# 287      YearsInCurrentRole    YearsWithCurrManager   0.7143648
# 188                     Age       TotalWorkingYears   0.6803805
# 233       TotalWorkingYears          YearsAtCompany   0.6281332
# 269          YearsAtCompany YearsSinceLastPromotion   0.6184089
# 270      YearsInCurrentRole YearsSinceLastPromotion   0.5480562
# 228           MonthlyIncome          YearsAtCompany   0.5142848
# 288 YearsSinceLastPromotion    YearsWithCurrManager   0.5102236
#mydata = numeric.columns[,c(1,7,12,14,15,16,17)]
library(PerformanceAnalytics)

correlationf <-
  function (R, histogram = TRUE, method = c("pearson", "kendall",
                                            "spearman"), ...)
  {
    x = checkData(R, method = "matrix")
    if (missing(method))
      method = method[1]
    panel.cor <-
      function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs",
               method, cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = use, method = method)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (missing(cex.cor))
          cex <- 0.8 / strwidth(txt)
        test <- cor.test(x, y, method = method)
        Signif <- symnum(
          test$p.value, corr = FALSE, na = FALSE,
          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***",
                                                                   "**", "*", ".", " ")
        )
        text(0.5, 0.5, txt, cex = cex * (abs(r) + 1) / 1.3)
        text(0.8, 0.8, Signif, cex = cex, col = 2)
      }
    f <- function(t) {
      dnorm(t, mean = mean(x), sd = sd.xts(x))
    }
    hist.panel = function(x, ...) {
      par(new = TRUE)
      hist(
        x, col = "light gray", probability = TRUE, axes = FALSE,
        main = "", breaks = "FD"
      )
      lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
      rug(x)
    }
    if (histogram)
      pairs(
        x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,
        diag.panel = hist.panel, method = method, ...
      )
    else
      pairs(
        x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,
        method = method, ...
      )
  }

correlationf(numeric.columns, histogram = TRUE, pch = '+',cex.cor.scale = 100)

#PCA
backup.hrdata = hrdata
hrdatanumericonly = backup.hrdata[,unlist(lapply(backup.hrdata,is.numeric))]
hrdatanumericonly = hrdatanumericonly[,-c(4,11)]
hrdata.pca <- prcomp(hrdatanumericonly,scale. = T)
print(hrdata.pca)


summary(hrdata.pca)
biplot(hrdata.pca)
std_dev <- hrdata.pca$sdev
pr_var <- std_dev ^ 2

#Eigen Values
pr_var
# [1] 4.0168448 1.6502343 1.0689120 1.0599450 1.0240371 1.0013568 0.9772699 0.9449862 0.9168311 0.7223793 0.5306896
# [12] 0.4698434 0.2832211 0.1933840 0.1400655

#We aim to find the components which explain the maximum variance. This is because, we want to retain as much information as possible using these components. So, higher is the explained variance, higher will be the information contained in those components.

#To compute the proportion of variance explained by each component, we simply divide the variance by sum of total variance. This results in:
prop_varex <- pr_var / sum(pr_var)
prop_varex
# [1] 0.26778965 0.11001562 0.07126080 0.07066300 0.06826914 0.06675712 0.06515133 0.06299908 0.06112207 0.04815862
# [11] 0.03537931 0.03132289 0.01888140 0.01289226 0.00933770
#This shows that first principal component explains 26.8% variance. Second component explains 11% variance. Third component explains 7.12% variance.Eleventh component explains 3.5% of variance and so on. So, how do we decide how many components should we select for modeling stage ?

#The answer to this question is provided by a scree plot. A scree plot is used to access components or factors which explains the most of variability in the data. It represents values in descending order.
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")
minor.tick(nx = 1,tick.ratio = 1)
minor.tick(ny = 1,tick.ratio = 1)
#From the plot it is seen that ~4 components 98% of variance is explained
plot(hrdata.pca)
#From the plot it is confirmed  that ~4 components 98% of variance is explained

hrdata.pca$rotation[,1:5]

#Here we see component 1 seems to be influenced TotalWorkingYears and YearsAtTheCompany. Component 2 is influenced mainly by Age and NumberOfCompaniesWorked. C3 by DistanceFromHome,PercentSalaryHike and TrainingTimesLastYear.
#C4 by DailyRate and MonthlyRate, C5 by hourly rate and salary hike.
#C6 by SalaryHike and TrainingTime,C7 by monthly rate, C8 by Daily rate and Hourly rate, C9 by Distance from Home,C10 by NumberOfCompaniesWorked
#C11 by YearsSinceLastPromotion and C12 by Age,MonthlyIncome,YearsWithCurrentManager

#Lets do some factor analysis
fit.3 <-
  factanal(hrdatascaled.clean.numericonly,factors = 3,rotation = "varimax")
fit.3

fit.4 <-
  factanal(hrdatascaled.clean.numericonly,factors = 4,rotation = "varimax")
fit.4

fit.5 <-
  factanal(hrdatascaled.clean.numericonly,factors = 5,rotation = "varimax")
fit.5
fit.6 <-
  factanal(hrdatascaled.clean.numericonly,factors = 6,rotation = "varimax")
fit.6
fit.7 <-  factanal(hrdatascaled.clean.numericonly,factors = 7,rotation = "varimax")

# We can "clean up" the factor pattern in several ways. One way is to hide small
# loadings, to reduce the visual clutter in the factor pattern. Another is to reduce the
# number of decimal places from 3 to 2. A third way is to sort the loadings to make the
# simple structure more obvious. The following command does all three.
print(fit.6, digits = 2, cutoff = .2, sort = TRUE)




#RandomForest
set.seed(123)
str(hrdata)
hrdata.sample = hrdata


# Creating Development and Validation Sample
hrdata.sample$split <- runif(nrow(hrdata.sample), 0, 1);
hrdata.sample <- hrdata.sample[order(hrdata.sample$split),]

#Now, if you view the hrdata.sample dataset again you would notice a new column added at the end
#Now, you can split the dataset to training and testing as follows
hrdata.train  <- hrdata.sample[which(hrdata.sample$split <= 0.7),]
hrdata.test <- hrdata.sample[which(hrdata.sample$split > 0.7),]
c(nrow(hrdata.train), nrow(hrdata.test))
#remove the split columns used for partitioning from both train and test data set
hrdata.train <- hrdata.train[,-c(33)]
hrdata.test <- hrdata.test[,-c(33)]
str(hrdata.train)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(quantile(hrdata.train$Age,probs = seq(0,1,by = 0.10))),include.lowest = TRUE)
}
str(hrdata.train)
hrdata.new.train = hrdata.train
hrdata.train$AgeGroup = sapply(hrdata.train$Age,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(quantile(
    hrdata.train$DailyRate,probs = seq(0,1,by = 0.10)
  )),include.lowest = TRUE)
}
hrdata.train$DailyRateGroup = sapply(hrdata.train$DailyRate,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$DistanceFromHome,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$DistanceFromHomeGroup = sapply(hrdata.train$DistanceFromHome,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$HourlyRate,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$HourlyRateGroup = sapply(hrdata.train$HourlyRate,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$MonthlyIncome,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$MonthlyIncomeGroup = sapply(hrdata.train$MonthlyIncome,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$MonthlyRate,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$MonthlyRateGroup = sapply(hrdata.train$MonthlyRate,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$NumCompaniesWorked,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$NumCompaniesWorkedGroup = sapply(hrdata.train$NumCompaniesWorked,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$PercentSalaryHike,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$PercentSalaryHikeGroup = sapply(hrdata.train$PercentSalaryHike,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$TotalWorkingYears,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$TotalWorkingYearsGroup = sapply(hrdata.train$TotalWorkingYears,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$TrainingTimesLastYear,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$TrainingTimesLastYearGroup = sapply(hrdata.train$TrainingTimesLastYear,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$YearsAtCompany,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$YearsAtCompanyGroup = sapply(hrdata.train$YearsAtCompany,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$YearsInCurrentRole,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$YearsInCurrentRoleGroup = sapply(hrdata.train$YearsInCurrentRole,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$YearsSinceLastPromotion,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$YearsSinceLastPromotionGroup = sapply(hrdata.train$YearsSinceLastPromotion,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$YearsWithCurrManager,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$YearsWithCurrManagerGroup = sapply(hrdata.train$YearsWithCurrManager,ApplyQuantile)
str(hrdata.train)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.train$YearsWithCurrManager,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.train$YearsWithCurrManagerGroup = sapply(hrdata.train$YearsWithCurrManager,ApplyQuantile)
str(hrdata.train)

backup.hrdata.train = hrdata.train
hrdata.train.allfactorvar = backup.hrdata.train[,unlist(lapply(backup.hrdata.train,is.factor))]
backup.hrdata.train.allfactorvar = hrdata.train.allfactorvar
str(hrdata.train.allfactorvar)
backup.hrdata.train = hrdata.train

ApplyQuantile <- function(x) {
  cut(x,breaks = c(quantile(hrdata.test$Age,probs = seq(0,1,by = 0.10))),include.lowest = TRUE)
}
str(hrdata.test)
hrdata.new.test = hrdata.test
hrdata.test$AgeGroup = sapply(hrdata.test$Age,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(quantile(
    hrdata.test$DailyRate,probs = seq(0,1,by = 0.10)
  )),include.lowest = TRUE)
}
hrdata.test$DailyRateGroup = sapply(hrdata.test$DailyRate,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$DistanceFromHome,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$DistanceFromHomeGroup = sapply(hrdata.test$DistanceFromHome,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$HourlyRate,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$HourlyRateGroup = sapply(hrdata.test$HourlyRate,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$MonthlyIncome,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$MonthlyIncomeGroup = sapply(hrdata.test$MonthlyIncome,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$MonthlyRate,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$MonthlyRateGroup = sapply(hrdata.test$MonthlyRate,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$NumCompaniesWorked,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$NumCompaniesWorkedGroup = sapply(hrdata.test$NumCompaniesWorked,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$PercentSalaryHike,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$PercentSalaryHikeGroup = sapply(hrdata.test$PercentSalaryHike,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$TotalWorkingYears,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$TotalWorkingYearsGroup = sapply(hrdata.test$TotalWorkingYears,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$TrainingTimesLastYear,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$TrainingTimesLastYearGroup = sapply(hrdata.test$TrainingTimesLastYear,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$YearsAtCompany,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$YearsAtCompanyGroup = sapply(hrdata.test$YearsAtCompany,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$YearsInCurrentRole,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$YearsInCurrentRoleGroup = sapply(hrdata.test$YearsInCurrentRole,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$YearsSinceLastPromotion,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$YearsSinceLastPromotionGroup = sapply(hrdata.test$YearsSinceLastPromotion,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$YearsWithCurrManager,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$YearsWithCurrManagerGroup = sapply(hrdata.test$YearsWithCurrManager,ApplyQuantile)
str(hrdata.test)

ApplyQuantile <- function(x) {
  cut(x,breaks = c(unique(
    quantile(
      hrdata.test$YearsWithCurrManager,probs = seq(0,1,by = 0.10),na.rm = TRUE
    )
  )),include.lowest = TRUE)
}
hrdata.test$YearsWithCurrManagerGroup = sapply(hrdata.test$YearsWithCurrManager,ApplyQuantile)
str(hrdata.test)

backup.hrdata.test = hrdata.test
hrdata.test.allfactorvar = backup.hrdata.test[,unlist(lapply(backup.hrdata.test,is.factor))]
backup.hrdata.test.allfactorvar = hrdata.test.allfactorvar
str(hrdata.test.allfactorvar)
str(hrdata.train.allfactorvar)

#making the levels of some factors equal for test and train data
hrdata.test.allfactorvar$DistanceFromHomeGroup <- as.character(hrdata.test.allfactorvar$DistanceFromHomeGroup)
hrdata.test.allfactorvar$YearsAtCompanyGroup = as.character(hrdata.test.allfactorvar$YearsAtCompanyGroup)
hrdata.test.allfactorvar$isTest <- rep(1,nrow(hrdata.test.allfactorvar))
hrdata.train.allfactorvar$isTest <- rep(0,nrow(hrdata.train.allfactorvar))

fullSet <- rbind(hrdata.test.allfactorvar,hrdata.train.allfactorvar)
fullSet$DistanceFromHomeGroup <- as.factor(fullSet$DistanceFromHomeGroup)
fullSet$YearsAtCompanyGroup <- as.factor(fullSet$YearsAtCompanyGroup)

test.new <- fullSet[fullSet$isTest==1,]
test.new = test.new[,-33]
str(test.new)

train.new <- fullSet[fullSet$isTest==0,]
train.new = train.new[,-33]

library(randomForest)
library(randomForestSRC)
set.seed(2016)

# Manual Search
library(caret)
library(e1071)
train.new=train.new[,-13]
train.new=na.omit(train.new)
library(functional)
train.new[apply(train.new, 1, Compose(is.finite, all)),]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#tunegrid=expand.grid(.mtry=c(1:15), .ntree=c(500,1000, 1500, 2000, 2500))
seed=2016

#Lest find the best ntree and mtry .Caution: this algorithms takes 48 hours to populate the values for the Hr attrition csv file
new.modellist <- list()
for (ntree in c(500,1000, 1500,2000,2500)) {
  set.seed(seed)
  print(ntree)
  for(mtryt in c(3:15)){
    tunegrid=expand.grid(.mtry=mtryt)
    key <-toString(paste(toString(ntree),toString(mtryt),sep = "_"))
    print(key)
    print(tunegrid)
    fit <- train(train.new$Attrition~., data=train.new, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control, ntree=ntree)
    print("new fit added")
    new.modellist[[key]] <- fit
    }
  
}
save.image(file="hr_rf.RData")
# compare results
results <- resamples( new.modellist)
summary(results)
#From the resukt we find that the mean accuracy of 94.51% ,which is good when ntree is 1500 and with mtry of 12

arf <-  randomForest(  Attrition ~ .,data = train.new,importance = TRUE,proximity = TRUE,ntree =1500, mtry=12, keep.forest = TRUE)
print(arf)

plot(arf,main = "")
legend(
  "topright", c("OOB", "0", "1"), text.col = 1:6, lty = 1:3, col = 1:3
)
title(main = "Error Rates Random Forest hrdata.train")
arf$err.rate

#pred= predict(arf,test.new)
#table(pred,test.new$Attrition)
# pred    No  Yes
# No  1739    0
# Yes    0  319
#Misclassification rate is 0/2058 which is awesome

#validating the model

## Scoring syntax
train.new$predict.class <- predict(arf, train.new, type="class")
train.new$predict.score <- predict(arf, train.new, type="prob")
head(train.new)

## deciling

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

train.new$deciles <- decile(train.new$predict.score[,2])
library(plyr)
x = (count(train.new$Attrition == "No"))$freq + (count(train.new$Attrition == "No"))$freq 
x[1]
## Ranking code
train.new$Attrition = factor(
  train.new$Attrition,
  levels = c("Yes", "No"),
  labels = c(1, 0)
)
train.new$Attrition <-
  as.numeric(as.character(train.new$Attrition))
library(data.table)
tmp_DT = data.table(train.new)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 1)),
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)




#RandomForest performance


library(ROCR)

testp4 <- predict(arf,train.new,type = 'prob')[,2]
pred4 <- prediction(testp4,train.new$Attrition)
#performance in terms of true and false positive rates
perf4 <- performance(pred4,"tpr","fpr")
#plot the curve
plot(perf4,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
#compute area under curve
KS <- max(attr(perf4, 'y.values')[[1]]-attr(perf4, 'x.values')[[1]])
auc <- performance(pred4,"auc"); 
auc <- as.numeric(auc@y.values)

minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")

#important factors
#plot variable importance
# The variable importance plot lists variables in terms of importance using the decrease in
# accuracy metric, of loss of predictive power if the variable is dropped, vs. the importance
# in terms of Gini index, a measure of separation of classes.

print(importance(arf,type = 2)) 
varImpPlot(arf)

save.image(file="hr_rf.RData")