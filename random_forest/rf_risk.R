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


################################################################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
################################################################

### ELIMINATE G3 = 0 #####

indG20 <- which(data.math$G2==0)
indG30 <- which(data.math$G3==0)

risk <- rep(0, nrow(data.math))
risk[indG20] = 1
risk[indG30] = 1

#########################
data.math$G1 <- NULL 
data.math$G2 <- NULL
data.math$G3 <- NULL
data.math["risk"] <- risk

data.risk <- data.math

##################################

# ind <- c(1:nrow(data.risk)) 
# train = data.math[ind,]
# test = data.math[-ind,]
# ntest <- nrow(test)
# ntrain <- nrow(train)
# 
# for (i in 1:ncol(test))
# {
#   test[,i] <- as.character(test[,i])
# }
# 
# test$isTest <- rep(1,nrow(test))
# train$isTest <- rep(0,nrow(train))
# fullSet <- rbind(test,train)
# 
# for (i in 1:ncol(test))
# {
#   fullSet[,i] <- as.factor(fullSet[,i])
# }
# 
# test <- fullSet[fullSet$isTest==1,]
# train <- fullSet[fullSet$isTest==0,]
# 
# test$isTest <- NULL
# train$isTest <- NULL
# 
# 
# globFit <- randomForest(risk~ ., method="class", data=train, importance=TRUE, ntree=2000)
# varImpPlot(globFit)

#################################################################
set.seed(1)
ind <- sample( 1:nrow(data.risk), size = ceiling (0.8*nrow(data.risk) ) ) 
train = data.risk[ind,]
test = data.risk[-ind,]
ntest <- nrow(test)
ntrain <- nrow(train)
##############################
# DATA <- train
# V = c(1:nrow(train))
# set.seed(355)
# vec <- replicate(10,sample(V,length(V),replace = TRUE))
# vec <- as.vector(vec)
# train <- train[vec,]
# train$row.names <- NULL
###########################

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


fit <- randomForest(risk~ ., method="class", data=train, importance=TRUE, ntree=2000)


pred <- predict(fit,test,type="class")
pred_train <- predict(fit,train,type = "class")
test$risk <- as.numeric(as.character(test$risk)) 

table(factor(pred, levels=min(test$risk):max(test$risk)),factor(test$risk, levels=min(test$risk):max(test$risk)))

cM <- confusionMatrix(pred,test$risk)
cMp <- cM$table/ntest*100
cMp

miss <- sum((as.numeric(pred)
                 - test$risk)^2)/ntest

miss_train <- sum((as.numeric(pred_train)
             - as.numeric(train$risk))^2)/ntrain

#norm_test_err <- test_err/mean(test$average)

