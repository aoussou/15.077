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
library(nnet)
library(caret)
#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
#################################################################

average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

average[average <= 4] = 1;
average[(4 <average ) & (average <= 8)] = 2;
average[(8 <average ) & (average <= 12)] = 3;
average[(12 <average ) & (average <= 16)] = 4;
average[(16 <average ) & (average <= 20)] = 5;
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
###############################################

mn <- multinom(average~ ., data = train, trace = FALSE)
pred <- predict(mn,test)
table(factor(pred, levels=min(test$average):max(test$average)),factor(test$average, levels=min(test$average):max(test$average)))

logit_imp <- varImp(mn)
logit_imp2<- as.data.frame(logit_imp)
logit_imp2$name <- row.names(logit_imp2)
imp_sort <- logit_imp2[order(-logit_imp2$Overall),]
imp_sort

