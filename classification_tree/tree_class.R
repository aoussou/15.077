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
library(rpart)
library(e1071)
library(ggplot2)
library(rattle)
library("caret")

#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
#########################

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
set.seed(4)
ind <- sample( 1:nrow(data.math.av), size = ceiling (0.8*nrow(data.math.av) ) ) 
train = data.math.av[ind,]
test = data.math.av[-ind,]
Ntest <- nrow(test)

##############################################

#fit <- rpart(average ~ ., method="class", data=train,minbucket=1)
fit <- rpart(average ~ ., method="class", data=train,minbucket=10,maxdepth=5)
bestk <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
pfit<- prune(fit, cp = bestk) 

plotcp(fit) # visualize cross-validation results 
plotcp(pfit) # visualize cross-validation results 

plot(fit, uniform=TRUE,  main= " grades ")
#text(fit, use.n=TRUE, all=TRUE, cex=.8)
text(fit, cex=.8)
# text(fit, use.n=TRUE, cex=.8)

plot(pfit, uniform=TRUE, main= " grades  ")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#text(pfit, use.n=TRUE,  cex=.8)
text(pfit, cex=.8)

# printcp(fit) # display the results 
#summary(pfit) # detailed summary of splits


pred <- predict(fit,test,type="class")

table(factor(pred, levels=min(test$average):max(test$average)),factor(test$average, levels=min(test$average):max(test$average)))

asRules(fit, compact=TRUE)
asRules(pfit, compact=TRUE)
