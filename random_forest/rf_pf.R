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
library("caret")
library(randomForest)

data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")

#########################

### ELIMINATE G3 = 0 #####
# 
# indG20 <- which(data.math$G2==0)
# data.math <- data.math[-indG20, ]
# 
# indG30 <- which(data.math$G3==0)
# data.math <- data.math[-indG30, ]


#########################

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

average[average < 9.5] = 0;
average[average >= 9.5] = 1;

data.math.av <- data.math

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average

#################################################################
set.seed(1)
ind <- sample( 1:nrow(data.math.av), size = ceiling (0.8*nrow(data.math.av) ) ) 
train = data.math.av[ind,]
test = data.math.av[-ind,]
ntest <- nrow(test)
ntrain <- nrow(train)
################################################################

for (i in 1:ncol(test))
{
  test[,i] <- as.character(test[,i])
}

test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
fullSet <- rbind(test,train)

for (i in 1:ncol(test))
{
  fullSet[,i] <- as.factor(fullSet[,i])
}

test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]

test$isTest <- NULL
train$isTest <- NULL

fit <- randomForest(average ~ ., method="class", data=train, importance=TRUE, ntree=2000)


pred <- predict(fit,test,type="class")
pred_train <- predict(fit,train,type = "class")
test$average<- as.numeric(as.character(test$average)) 

table(factor(pred, levels=min(test$average):max(test$average)),factor(test$average, levels=min(test$average):max(test$average)))

cM <- confusionMatrix(pred,test$average)
cMp <- cM$table/ntest*100
cMp

miss <- sum((as.numeric(pred)
                 - test$average)^2)/ntest

miss_train <- sum((as.numeric(pred_train)
             - as.numeric(train$average))^2)/ntrain

#norm_test_err <- test_err/mean(test$average)

varImpPlot(fit)