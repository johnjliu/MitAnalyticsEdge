gerber=read.csv("gerber.csv")
head(gerber)
str(gerber)
summary(gerber)

#Problem 1.1
table(gerber$voting)
108696 / nrow(gerber)


#Problem 1.2
tapply(gerber$voting, gerber$civicduty,mean)
#0.3145377

tapply(gerber$voting, gerber$hawthorne,mean)
#0.3223746

tapply(gerber$voting, gerber$self,mean)
#0.3451515

tapply(gerber$voting, gerber$neighbors,mean)
#0.3779482

tapply(gerber$voting, gerber$control,mean)

#Problem 1.3
logModel1 = glm(voting~civicduty+hawthorne+self+neighbors, data = gerber, family=binomial)
summary(logModel1)

#Problem 1.4
predictTest1 = predict(logModel1, type="response")
table(gerber$voting, predictTest1 > 0.3)
(134513+51966)/nrow(gerber)

#Problem 1.5
table(gerber$voting, predictTest1 > 0.5)
(235388+0)/nrow(gerber)

#Problem 1.6
library(ROCR)
votePred = prediction(predictTest1, gerber$voting)
as.numeric(performance(votePred, "auc")@y.values)


#Problem 2.1
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#Problem 2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#Problem 2.3
prp(CARTmodel2)

#Problem 2.4
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors  + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#Problem 3.1
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4,digits=6)
abs(0.296638 - 0.34)

#Problem 3.2
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5,digits=6)
abs(0.334176 - 0.290456)
abs(0.345818 - 0.302795)


#Problem 3.3
logModel2 = glm(voting~control+sex, data = gerber, family=binomial)
summary(logModel2)

#Problem 3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logModel2 , newdata=Possibilities, type="response")
abs(0.290456 -0.2908065)

#Problem 3.5
logModel3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(logModel3)

#Problem 3.6
predict(logModel3 , newdata=Possibilities, type="response")
abs(0.290456 -0.2904558) #0

