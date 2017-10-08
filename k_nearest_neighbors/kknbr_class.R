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

#########################

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

# average[average <= 4] = 1;
# average[(4 <average ) & (average <= 8)] = 2;
# average[(8 <average ) & (average <= 12)] = 3;
# average[(12 <average ) & (average <= 16)] = 4;
# average[(16 <average ) & (average <= 20)] = 5;

data.math.av <- data.math

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average


#################################################################
set.seed(1)
ind <- sample( 1:nrow(data.math.av), size = ceiling (0.8*nrow(data.math.av) ) ) 
train <- data.math.av[ind,]
test <- data.math.av[-ind,]

Nav <- ncol(train)
ntrain <- nrow(train)
ntest <- nrow(test)

Caverage <- average
Caverage[average <= 4] = 1;
Caverage[(4 <average ) & (average <= 8)] = 2;
Caverage[(8 <average ) & (average <= 12)] = 3;
Caverage[(12 <average ) & (average <= 16)] = 4;
Caverage[(16 <average ) & (average <= 20)] = 5;

Caverage_test<- Caverage[-ind] 
#################################################################

knnfit<- train.kknn(average ~ .,train,kmax=20, kcv = 5, kernel= NULL, ykernel = NULL)

kopt <- knnfit$best.parameters[2]

train_pred <-predict(knnfit,train)
test_pred <- predict(knnfit,test)

test_pred_c <- test_pred
test_pred_c [test_pred_c <= 4] = 1;
test_pred_c[(4 < test_pred_c ) & (test_pred_c <= 8)] = 2;
test_pred_c[(8 < test_pred_c ) & (test_pred_c <= 12)] = 3;
test_pred_c[(12 < test_pred_c ) & (test_pred_c <= 16)] = 4;
test_pred_c[(16 < test_pred_c ) & (test_pred_c <= 20)] = 5;

table(factor(test_pred_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))
table(factor(test_pred_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))/ntest*100
