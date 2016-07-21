fedFunds = read.csv("federalFundsRate.csv",stringsAsFactors=FALSE)
head(fedFunds)
str(fedFunds)

#Problem 1
table(fedFunds$RaisedFedFunds)
294/nrow(fedFunds)

#Problem 2
table(fedFunds$Chairman, fedFunds$RaisedFedFunds)

#Problem 3
fedFunds$Chairman = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)
summary(fedFunds)
#Randowm Forest classification requires outcome variable to be a factor

#Problem 4
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
training = subset(fedFunds, spl==TRUE)
testing = subset(fedFunds,spl==FALSE)

#Problem 5
reg1 = glm(RaisedFedFunds~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection,data=training,family=binomial)
summary(reg1)
coef(reg1)

#Problem 6
predict(reg1, newdata = data.frame(PreviousRate=c(1.7),Streak=c(-3),Unemployment=c(5.1),HomeownershipRate=c(65.3),DemocraticPres=as.factor(c(0)),MonthsUntilElection=c(18)),type="response")

#Problem 7
#Log odds inceased by 34.8%
#exp(0.347829)=1.41599 odds increased by 41.6%

#Problem 8
reg1.pred = predict(reg1, newdata = testing, type="response")
summary(reg1.pred)
#table(testing$RaisedFedFunds, reg1.pred > 0.5)
table(reg1.pred > 0.5)
table(testing$RaisedFedFunds)
#91

#Problem 9
library(ROCR)
ROCRpred = prediction(reg1.pred, testing$RaisedFedFunds)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Problem 10
#The AUC is the proportion of time the model can differentiate between a randomly selected true positive and true negative.

#Problem 11
library(ROCR)
perf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity",  ylab="Sensitivity")    
abline(0, 1)
#0

#Problem 12
table(testing$RaisedFedFunds, reg1.pred > 0.37)

#Problem 13
#20

#Problem 14
set.seed(201)

library(rpart)
library(rpart.plot)
numFolds = trainControl( method = "cv", number = 10 ) # 10 folds
cartGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 
train(RaisedFedFunds ~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data=training, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
#0.016

#Problem 15
fedFunds.CART1 = rpart(RaisedFedFunds ~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data=training,method="class",cp=0.016)
prp(fedFunds.CART1)

#Problem 16
predict(fedFunds.CART1, newdata = data.frame(PreviousRate=c(1.7),Streak=c(-3),Unemployment=c(5.1),HomeownershipRate=c(65.3),DemocraticPres=as.factor(c(0)),MonthsUntilElection=c(18)),type="class")

#Problem 17
PredictCV = predict(fedFunds.CART1, newdata = testing, type = "class")
table(testing$RaisedFedFunds, PredictCV)
(64+48)/nrow(testing)
