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
library("caret")
library(DAAG)
library(nortest)
library(e1071)
data.math <- read.csv("student-mat.csv",stringsAsFactors = FALSE)
data.math.org <- read.csv("student-mat.csv")

indG20 <- which(data.math$G2==0)
indG30 <- which(data.math$G3==0)

risk <- rep(0, nrow(data.math))
risk[indG20] = 1
risk[indG30] = 1

sum(risk)

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

sum(data.math$G1==0)
sum(data.math$G2==0)/395*100
sum(data.math$G3==0)/395*100

GRADES = data.math.org[,(ncol(data.math.org)-2):ncol(data.math.org)]



###########################
# indG20 <- which(data.math$G2==0)
# data.math <- data.math[-indG20, ]
# 
# indG30 <- which(data.math$G3==0)
# data.math <- data.math[-indG30, ]
# 
# GRADES <- GRADES[-indG20, ]
# GRADES <- GRADES[-indG30, ]
###########################



average = (data.math$G1 + data.math$G2 + data.math$G3 )/3

sum(data.math$G1==0)
sum(data.math$G2==0)
sum(data.math$G3==0)

data.math.av <- data.math

data.math.av$G1 <- NULL 
data.math.av$G2 <- NULL
data.math.av$G3 <- NULL
data.math.av["average"] <- average



med <- apply(GRADES, 1, median) 
med_av <- median(average)
mean_av <- mean(average)
hist(average, breaks = 20)

hist(data.math$G1,breaks = 20,main = "1st term grade")
hist(data.math$G2,breaks = 20,main = "2nd term grade")
hist(data.math$G3,breaks = 20,main = "3rd term grade")
hist(average, main = "average grade",breaks = 20)
hist(med, main = "median of grades during the 3 terms", breaks = 20)

n = length(average)
sd(average)

shapiro.test(average)
ad.test(average)
lillie.test(average)
shapiro.test(med)
ad.test(med)
lillie.test(med)

plot((1:n - 1)/(n - 1), sort(average), type="l",
       main = "Quantiles",
       xlab = "Sample Fraction",
       ylab = "Sample Quantile")


###### CORRELATION WITH AVERAGE ######
mcor<-cor(data.math.av)
mcor2 <- mcor
mcor2[abs(mcor)<.2]=0
mcor2 <- round(mcor2,digits = 1)
#mcor2[ mcor2 == 0.0]<- round(mcor2 == 0.0)
av_cor <- as.data.frame( mcor[ncol(mcor),])

###### CORRELATION WITH MED ######

data.math.med <- data.math
data.math.med["med"] <- med

data.math.med$G1 <- NULL 
data.math.med$G2 <- NULL
data.math.med$G3 <- NULL

mcor_med<-cor(data.math.med)
mcor2_med <- mcor_med
mcor2_med[abs(mcor_med)<.1]=0

med_cor <- as.data.frame( mcor_med[ncol(mcor_med),])


#### CORRELATION WITH RISK #####

data.math.risk <- data.math
data.math.risk["risk"] <- risk

data.math.risk$G1 <- NULL 
data.math.risk$G2 <- NULL
data.math.risk$G3 <- NULL

mcor_risk<-cor(data.math.risk)
mcor2_risk <- mcor_risk
mcor2_risk[abs(mcor_risk)<.1]=0

risk_cor <- as.data.frame( mcor_risk[ncol(mcor_risk),])

which.max(risk_cor[1:nrow(risk_cor)-1,])

skewness(data.math.org$G3)
