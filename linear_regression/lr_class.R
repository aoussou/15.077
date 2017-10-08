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
library("e1071")
library("parcor")
library(DAAG)
library("caret")

################################################################
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

###########################

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

data.math.av <- data.math

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average

Caverage <- average
Caverage[average <= 4] = 1;
Caverage[(4 <average ) & (average <= 8)] = 2;
Caverage[(8 <average ) & (average <= 12)] = 3;
Caverage[(12 <average ) & (average <= 16)] = 4;
Caverage[(16 <average ) & (average <= 20)] = 5;

#################################################################
set.seed(1)
ind <- sample( 1:nrow(data.math.av), size = ceiling (0.8*nrow(data.math.av) ) ) 
train <- data.math.av[ind,]
test <- data.math.av[-ind,]

mean_test <- mean(test$average)
Caverage_test<- Caverage[-ind] 
###############################################

# DATA <- train
# V = c(1:nrow(train))
# set.seed(355)
# vec <- replicate(10,sample(V,length(V),replace = TRUE))
# vec <- as.vector(vec)
# train <- train[vec,]
# train$row.names <- NULL

##############################################



Nav <- ncol(train)
ntrain <- nrow(train)
ntest <- nrow(test)
#fit <- lm(average~.,data=train)
OLS <- tune(lm, average~.,data=train) 
#OLS <- cv.lm(df=train, fit, m=10) 

OLS_pred_train <- as.matrix(train[,1:Nav-1])%*% (OLS$best.model$coefficients[2:Nav] ) + OLS$best.model$coefficients[1]
OLS_train_err <- sqrt(sum((OLS_pred_train - train$average)^2)/ntrain)
OLS_pred_test <- as.matrix(test[,1:Nav-1])%*% (OLS$best.model$coefficients[2:Nav] ) + OLS$best.model$coefficients[1]
OLS_test_err <- sqrt(sum((OLS_pred_test - test$average)^2)/ntest)
mean_OLS_test <- mean(OLS_pred_test)

OLS_pred_test_c <- OLS_pred_test
OLS_pred_test_c [OLS_pred_test_c <= 4] = 1;
OLS_pred_test_c[(4 < OLS_pred_test_c ) & (OLS_pred_test_c <= 8)] = 2;
OLS_pred_test_c[(8 < OLS_pred_test_c ) & (OLS_pred_test_c <= 12)] = 3;
OLS_pred_test_c[(12 < OLS_pred_test_c ) & (OLS_pred_test_c <= 16)] = 4;
OLS_pred_test_c[(16 < OLS_pred_test_c ) & (OLS_pred_test_c <= 20)] = 5;

table(factor(OLS_pred_test_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))
table(factor(OLS_pred_test_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))/ntest*100


#### ALREADY DOING CROSS VALIDATION
RIDGE <- ridge.cv(as.matrix(train[,1:Nav-1]), as.matrix(train[,Nav]), scale = TRUE, k = 10, plot.it = FALSE)
RIDGE_pred_train <- as.matrix(train[,1:Nav-1])%*% (RIDGE$coefficients ) + RIDGE$intercept
RIDGE_train_err <- sqrt(sum((RIDGE_pred_train - train$average)^2)/ntrain)
RIDGE_pred_test <- as.matrix(test[,1:Nav-1])%*% (RIDGE$coefficients ) + RIDGE$intercept
RIDGE_test_err <- sqrt(sum((RIDGE_pred_test - test$average)^2)/ntest)
mean_RIDGE_test <- mean(RIDGE_pred_test)

RIDGE_pred_test_c <- RIDGE_pred_test
RIDGE_pred_test_c [RIDGE_pred_test_c <= 4] = 1;
RIDGE_pred_test_c[(4 < RIDGE_pred_test_c ) & (RIDGE_pred_test_c <= 8)] = 2;
RIDGE_pred_test_c[(8 < RIDGE_pred_test_c ) & (RIDGE_pred_test_c <= 12)] = 3;
RIDGE_pred_test_c[(12 < RIDGE_pred_test_c ) & (RIDGE_pred_test_c <= 16)] = 4;
RIDGE_pred_test_c[(16 < RIDGE_pred_test_c ) & (RIDGE_pred_test_c <= 20)] = 5;

table(factor(RIDGE_pred_test_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))
table(factor(RIDGE_pred_test_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))/ntest*100


#### ALREADY DOING CROSS VALIDATION
LASSO <- mylars(as.matrix(train[,1:Nav-1]) , as.matrix(train[,Nav]), 
                k = 10, use.Gram=TRUE , normalize=TRUE , intercept=TRUE)
LASSO_pred_train <- as.matrix(train[,1:Nav-1])%*% (LASSO$coefficients ) + LASSO$intercept
LASSO_train_err <- sqrt(sum((LASSO_pred_train - train$average)^2)/ntrain)
LASSO_pred_test <- as.matrix(test[,1:Nav-1])%*% (LASSO$coefficients ) + LASSO$intercept
LASSO_test_err <- sqrt(sum((LASSO_pred_test - test$average)^2)/ntest)
LASSOcoeff <- as.data.frame(LASSO$coefficients)

LASSO_pred_test_c <- LASSO_pred_test
LASSO_pred_test_c [LASSO_pred_test_c <= 4] = 1;
LASSO_pred_test_c[(4 < LASSO_pred_test_c ) & (LASSO_pred_test_c <= 8)] = 2;
LASSO_pred_test_c[(8 < LASSO_pred_test_c ) & (LASSO_pred_test_c <= 12)] = 3;
LASSO_pred_test_c[(12 < LASSO_pred_test_c ) & (LASSO_pred_test_c <= 16)] = 4;
LASSO_pred_test_c[(16 < LASSO_pred_test_c ) & (LASSO_pred_test_c <= 20)] = 5;

table(factor(LASSO_pred_test_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))
table(factor(LASSO_pred_test_c, levels=min(Caverage_test):max(Caverage_test)),factor(Caverage_test, levels=min(Caverage_test):max(Caverage_test)))/ntest*100


mean_LASSO_test <- mean(LASSO_pred_test)


