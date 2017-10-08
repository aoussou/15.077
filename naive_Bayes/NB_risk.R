rm(list=ls())
cat("\014")  

## WARNING: it is assumed you are running 
# this script using RStudio (2015 version)
# if not please alter the following lines
# to set your directory accordingly.
library('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
#########################################
library(gdata)
library("kknn")
library("caret")
library(e1071)

data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

average[average < 9.5] = 0;
average[average >= 9.5] = 1;


data.math.av <- data.math

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average

#################################################################

indG20 <- which(data.math$G2==0)
indG30 <- which(data.math$G3==0)

risk <- rep(0, nrow(data.math))
risk[indG20] = 1
risk[indG30] = 1

data.math$G1 <- NULL 
data.math$G2 <- NULL
data.math$G3 <- NULL
data.math["risk"] <- risk

data.risk <- data.math

#################################################################


#################################################################
set.seed(5)
ind <- sample( 1:nrow(data.risk), size = ceiling (0.8*nrow(data.risk) ) ) 
train <- data.risk[ind,]
test <- data.risk[-ind,]
#################################################################

NB<- naiveBayes(as.factor(risk) ~ .,train,laplace = 0)

PRED <- predict(NB,test)

alarm <- table(factor(PRED, levels=min(test$risk):max(test$risk)),factor(test$risk, levels=min(test$risk):max(test$risk)))

TN <- alarm[1,1]
TP <- alarm[2,2]
FP <- alarm[2,1]
FN <- alarm[1,2]

pre <- TP/(TP + FP)
rec <- TP/(TP + FN)
fval <- 2*rec*pre/(rec+pre)


library('pROC')

###### WRONG ##############
roc(as.numeric(test$risk),as.numeric(PRED))
plot.roc(as.numeric(test$risk),as.numeric(PRED))
###############################################
roc(as.numeric(PRED),as.numeric(test$risk))
plot.roc(as.numeric(PRED),as.numeric(test$risk))

library('ROCR')
ROC_pred<- prediction(test$risk,PRED)
ROC_perf<- performance(ROC_pred,"tpr","fpr")
auc_ROCR <- performance(ROC_pred,"auc")
auc_ROCR
plot(ROC_perf)

ROC_pred<- prediction(PRED,test$risk)
ROC_perf<- performance(ROC_pred,"tpr","fpr")
auc_ROCR <- performance(ROC_pred,"auc")
auc_ROCR
plot(ROC_perf)

pre
rec
fval