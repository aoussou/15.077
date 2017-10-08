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
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")

#########################

indG20 <- which(data.math$G2==0)
indG30 <- which(data.math$G3==0)

risk <- rep(0, nrow(data.math))
risk[indG20] = 1
risk[indG30] = 1
#########################

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

fit <- rpart(risk ~ ., method="class", data=train,minbucket=1)
bestk <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
pfit<- prune(fit, cp = bestk) 

plotcp(fit) # visualize cross-validation results 
plotcp(pfit) # visualize cross-validation results 

plot(fit, uniform=TRUE, main= " risk of failure ")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
text(fit,  cex=.8)

plot(pfit, uniform=TRUE, main= " risk of failure - pruned ")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
text(pfit,  cex=.8)

asRules(fit, compact=TRUE)
asRules(pfit, compact=TRUE)

# printcp(fit) # display the results 
#summary(pfit) # detailed summary of splits


pred <- predict(fit,test,type="class")

table(factor(pred, levels=min(test$risk):max(test$risk)),factor(test$risk, levels=min(test$risk):max(test$risk)))

cM <- confusionMatrix(pred,test$risk)
cMp <- cM$table/ntest*100
cMp

miss <- sum((as.numeric(as.vector(pred))
                      - test$risk)^2)/ntest
