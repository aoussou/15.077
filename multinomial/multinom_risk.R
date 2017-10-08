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
library(MASS)
library(caret)
library(SDMTools)
library(unbalanced)
#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
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
set.seed(5) #15 is good 115 is very good; 55 ok for curve
ind <- sample( 1:nrow(data.risk), size = ceiling (0.8*nrow(data.risk) ) ) 
train = data.risk[ind,]
test = data.risk[-ind,]
ntest <- nrow(test)
ntrain <- nrow(train)
#################################################################


#glmfit <- glm(train[,1:ncol(train)-1],train[,ncol(train)], data=train tunecontrol = tune.control(cross = 5))
glmfit <- glm(risk ~ ., family = binomial(logit), data = train)

logit_imp <- varImp(glmfit,usemodel = glm)
logit_imp2<- as.data.frame(logit_imp)
logit_imp2$name <- row.names(logit_imp2)
imp_sort <- logit_imp2[order(-logit_imp2$Overall),]
imp_sort


lpred <- predict(glmfit,test)

PRED <- 1/(1+ exp(-lpred))
pred <- PRED
OT <- optim.thresh(test$risk, PRED, threshold = 101)
ot <- OT$min.ROC.plot.distance[1]

pred[pred<ot] = 0
pred[pred>ot] = 1

alarm<-table(factor(pred, levels=min(test$risk):max(test$risk)),factor(test$risk, levels=min(test$risk):max(test$risk)))


library('pROC')
#plot.roc(as.numeric(test$risk),as.numeric(PRED))
plot.roc(as.numeric(test$risk),as.numeric(PRED),xlab="False Positive Rate",ylab="True Positive Rate",main = "Binomial Logit ROC curve")

library('ROCR')
ROC_pred<- prediction(PRED,test$risk)
PR_perf <- performance(ROC_pred, "prec", "rec")
plot(PR_perf,main = "Binomial Logit PR curve")

TN <- alarm[1,1]
TP <- alarm[2,2]
FP <- alarm[2,1]
FN <- alarm[1,2]

pre <- TP/(TP + FP)
rec <- TP/(TP + FN)

fval <- 2*rec*pre/(rec+pre)

pre
rec
fval

library(pracma)
X<-PR_perf@x.values
Y<- PR_perf@y.values
X <- unlist(X)
Y <- unlist(Y)
X <- X[2:length(X)]
Y <- Y[2:length(Y)]
trapz(X,Y)
