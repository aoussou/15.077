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
library("e1071")
library("caret")
#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
#########################

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

average[average < 9.5] = 0;
average[average >= 9.5] = 1;


data.math.av <- data.math

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average

pf<- average
pf [pf <9.5 ] = 0;
pf [pf >= 9.5] = 1;

#################################################################
set.seed(1)
ind <- sample( 1:nrow(data.math.av), size = ceiling (0.8*nrow(data.math.av) ) ) 
train = data.math.av[ind,]
test = data.math.av[-ind,]
Ntest <- nrow(test)

##############################


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

pf_test <- pf[-ind]

###############################
tc <- tune.control(cross = 5)

tune.out=tune(svm,average ~ ., data = train, kernel="radial", ranges=list(cost=c(.01, .1, 1, 10), gamma=c(.01,.1, 1, 10,100)),tunecontrol = tc ) 

pred= as.data.frame(predict(tune.out$best.model,test))
# # 

pred_pf <- as.vector(pred[,1])
# pred_pf [pred_pf <9.5 ] = 0;
# pred_pf [pred_pf >= 9.5] = 1;

table(pred[,1],test$average)

cMpf <- confusionMatrix(pred_pf[,1],as.numeric(pred_pf))
cMppf <- cMpf$table/Ntest*100
cMppf

#confusionMatrix(pred[,1],test$average)
cM <- confusionMatrix(pred[,1],test$average)
cMp <- cM$table/Ntest*100
cMp
test_err <- sqrt(sum((as.numeric(as.vector(pred))
                      - test$average)^2))/Ntest

norm_test_err <- test_err/mean(test$average)