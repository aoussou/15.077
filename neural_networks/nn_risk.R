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
library("caret")
library("nnet")
library("e1071")
library('SDMTools')
#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
#####################################

data.math$school <- ifelse(data.math$school=="GP", 1, 0)
data.math$address <- ifelse(data.math$address=="R", 1, 0)
data.math$sex <- ifelse(data.math$sex=="F", 1, 0)
data.math$famsize <- ifelse(data.math$famsize=="LE3", 1, 0)
data.math$Pstatus <- ifelse(data.math$Pstatus=="A", 1, 0)

###################################

data.math$schoolsup <- ifelse(data.math$schoolsup=="yes", 1, 0)
data.math$famsup <- ifelse(data.math$famsup=="yes", 1, 0)
data.math$paid <- ifelse(data.math$paid=="yes", 1, 0)
data.math$activities <- ifelse(data.math$activities=="yes", 1, 0)
data.math$nursery <- ifelse(data.math$nursery=="yes", 1, 0)
data.math$higher <- ifelse(data.math$higher=="yes", 1, 0)
data.math$internet<- ifelse(data.math$internet=="yes", 1, 0)
data.math$romantic<- ifelse(data.math$romantic=="yes", 1, 0)

#########################

Mjob <- model.matrix( ~ Mjob - 1,data.math) 
data.math$Mjob <- NULL 
data.math["Mteacher"] <- Mjob[,"Mjobteacher"] 
data.math["Mservices"] <- Mjob[,"Mjobservices"] 
data.math["Mhealth"] <- Mjob[,"Mjobhealth"] 
data.math["Mhome"] <- Mjob[,"Mjobat_home"] 

Fjob <- model.matrix( ~ Fjob - 1,data.math) 
data.math$Fjob <- NULL 
data.math["Fteacher"] <- Fjob[,"Fjobteacher"] 
data.math["Fservices"] <- Fjob[,"Fjobservices"] 
data.math["Fhealth"] <- Fjob[,"Fjobhealth"] 
data.math["Fhome"] <- Fjob[,"Fjobat_home"] 


Reason <- model.matrix( ~ reason - 1,data.math) 
data.math$reason <- NULL 
data.math["reason_course"] <- Reason[,"reasoncourse"] 
data.math["reason_home"] <- Reason[,"reasonhome"] 
data.math["reason_rep"] <- Reason[,"reasonreputation"]

Guardian <- model.matrix( ~ guardian - 1,data.math) 
data.math$guardian <- NULL 
data.math["g_mother"] <- Guardian[,"guardianmother"] 
data.math["g_father"] <- Guardian[,"guardianfather"] 

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
set.seed(5)
ind <- sample( 1:nrow(data.risk), size = ceiling (0.8*nrow(data.risk) ) ) 
train = data.risk[ind,]
test = data.risk[-ind,]
ntest <- nrow(test)
ntrain <- nrow(train)
######################


nnfit <- tune.nnet(train[,1:ncol(train)-1],train[,ncol(train)], data=train, size = c(1:10), decay = c(0.01,0.1),
          trace = FALSE, tunecontrol = tune.control(cross = 5))

mod <- nnfit$best.model

PRED<-predict(mod,newdata = test)

pred<- PRED

OT <- optim.thresh(test$risk, PRED, threshold = 101)
ot <- OT$min.ROC.plot.distance[1]

pred[pred<ot] = 0
pred[pred>ot] = 1

table(factor(pred, levels=min(test$risk):max(test$risk)),factor(test$risk, levels=min(test$risk):max(test$risk)))
alarm<-table(factor(pred, levels=min(test$risk):max(test$risk)),factor(test$risk, levels=min(test$risk):max(test$risk)))



library('pROC')
plot.roc(as.numeric(test$risk),as.numeric(PRED),xlab="False Positive Rate",ylab="True Positive Rate",main="Neural Network ROC")

library('ROCR')
ROC_pred<- prediction(PRED,test$risk)
ROC_perf<- performance(ROC_pred,"tpr","fpr")
PR_perf <- performance(ROC_pred, "prec", "rec")
#plot(ROC_perf)
plot(PR_perf,main = "Neural Network PR curve")

auc_ROCR <- performance(ROC_pred,"auc")
auc_ROCR

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
