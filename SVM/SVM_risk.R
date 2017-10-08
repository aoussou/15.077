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
library("e1071")
library("caret")
#########################
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")
#################################################################

### ELIMINATE G3 = 0 #####

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
##############################
# DATA <- train
# V = c(1:nrow(train))
# set.seed(55)
# vec <- replicate(10,sample(V,length(V),replace = TRUE))
# vec <- as.vector(vec)
# train <- train[vec,]
# train$row.names <- NULL
###########################

for (i in 1:ncol(test))
{
  test[,i] <- as.character(test[,i])
}

test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
fullSet <- rbind(test,train)

for (i in 1:ncol(test))
{
  fullSet[,i] <- as.factor(fullSet[,i])
}

test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]

test$isTest <- NULL
train$isTest <- NULL
#############################
tc <- tune.control(cross = 5)

tune.out=tune(svm,risk ~ ., data = train, kernel="radial", ranges=list(cost=c(.01, .1, 1, 10), gamma=c(.01,.1, 1, 10,100)),tunecontrol = tc ) 

pred= as.data.frame(predict(tune.out$best.model,test))

table(pred[,1],test$risk)

#confusionMatrix(pred[,1],test$risk)
cM <- confusionMatrix(pred[,1],test$risk)
cMp <- cM$table/ntest*100
cMp

norm_test_err <- test_err/mean(test$average)