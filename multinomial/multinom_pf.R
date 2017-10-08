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

#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
#################################################################

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
###############################################

glmfit <- glm(average ~ ., family = binomial(logit), data = train)

lpred <- predict(glmfit,test)


PRED <- 1/(1+ exp(-lpred))

pred <- PRED
pred[pred<.5] = 0
pred[pred>=.5] = 1


table(factor(pred, levels=min(test$average):max(test$average)),factor(test$average, levels=min(test$average):max(test$average)))

library('pROC')
plot.roc(as.numeric(test$average),as.numeric(PRED))

