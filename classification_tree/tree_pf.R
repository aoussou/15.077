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
Ntest <- nrow(test)
###############################################

# DATA <- train
# V = c(1:nrow(train))
# set.seed(355)
# vec <- replicate(10,sample(V,length(V),replace = TRUE))
# vec <- as.vector(vec)
# train <- train[vec,]
# train$row.names <- NULL

##############################################

fit <- rpart(average ~ ., method="class", data=train,minbucket=1)
bestk <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
pfit<- prune(fit, cp = bestk) 

plotcp(fit) # visualize cross-validation results 
plotcp(pfit) # visualize cross-validation results 

plot(fit, uniform=TRUE, main= " eBay auction 1 - unpruned ")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
text(fit,  cex=.8)

plot(pfit, uniform=TRUE, main= " eBay auction 1 - pruned ")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
text(pfit,  cex=.8)

asRules(fit, compact=TRUE)
asRules(pfit, compact=TRUE)

# printcp(fit) # display the results 
#summary(pfit) # detailed summary of splits


pred <- predict(fit,test,type="class")

table(factor(pred, levels=min(test$average):max(test$average)),factor(test$average, levels=min(test$average):max(test$average)))

# cM <- confusionMatrix(pred,test$average)
# cMp <- cM$table/Ntest*100
# cMp

