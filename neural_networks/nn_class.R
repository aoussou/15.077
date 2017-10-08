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

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3
# 
# average[average < 9.5] = 0;
# average[average >= 9.5] = 1;


data.math.av <- data.math

average[average <= 4] = 1;
average[(4 <average ) & (average <= 8)] = 2;
average[(8 <average ) & (average <= 12)] = 3;
average[(12 <average ) & (average <= 16)] = 4;
average[(16 <average ) & (average <= 20)] = 5;

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average

#################################################################

set.seed(55)
ind <- sample( 1:nrow(data.math.av), size = ceiling (0.8*nrow(data.math.av) ) ) 
train = data.math.av[ind,]
test = data.math.av[-ind,]
ntest <- nrow(test)

# ##############################
# 
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
# 
# ###############################

nnfit <- tune.nnet(train[,1:ncol(train)-1],train[,ncol(train)], data=train, size = c(1:10), decay = c(0.01,0.1),
          trace = FALSE, tunecontrol = tune.control(cross = 5))

mod <- nnfit$best.model
pred<-predict(mod,newdata = train[,1:ncol(train)-1],type = "class")


pred[(pred < 4)] = 1;
pred[(4 <= pred ) & (pred < 8)] = 2;
pred[(8 <= pred ) & (pred < 12)] = 3;
pred[(12 <= pred ) & (pred < 16)] = 4;
pred[(16 <=pred ) & (pred <= 20)] = 5;

test$average[test$average <= 4] = 1;
test$average[(4 <test$average ) & (test$average <= 8)] = 2;
test$average[(8 <test$average ) & (test$average <= 12)] = 3;
test$average[(12 <test$average ) & (test$average <= 16)] = 4;
test$average[(16 <test$average ) & (test$average <= 20)] = 5;

table(factor(pred, levels=min(test$average):max(test$average)),factor(test$average, levels=min(test$average):max(test$average)))