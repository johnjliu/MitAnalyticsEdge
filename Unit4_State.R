data(state)
statedata=data.frame(state.x77)
str(statedata)
head(statedata)
summary(statedata)

#Problem 1.1
reg1 =  lm(Life.Exp~.,data=statedata)
summary(reg1)

#Problem 1.2
pred = predict(reg1)
summary(pred)
SSE = sum((pred - statedata$Life.Exp)^2)
SSE


#Problem 1.3
reg2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(reg2)


#Problem 1.4
pred = predict(reg2)
summary(pred)
SSE = sum((pred - statedata$Life.Exp)^2)
SSE


#Problem 1.5
cor(statedata)
#first answer


#Problem 2.1
library(rpart)
library(rpart.plot)
stateCART1 = rpart(Life.Exp ~ ., data=statedata)
prp(stateCART1)

#problem 2.2
predictTest = predict(stateCART1)
summary(predictTest)
SSE = sum((predictTest - statedata$Life.Exp)^2)
SSE

#problem 2.3
stateCART2 = rpart(Life.Exp ~ ., data=statedata,minbucket=5)
prp(stateCART2)

#Problem 2.4
#The default bucket was larger

#Problem 2.5
predictTest = predict(stateCART2)
summary(predictTest)
SSE = sum((predictTest - statedata$Life.Exp)^2)
SSE

#Problem 2.6
stateCART3 = rpart(Life.Exp ~ Area, data=statedata,minbucket=1)
prp(stateCART3)

predictTest = predict(stateCART3)
summary(predictTest)
SSE = sum((predictTest - statedata$Life.Exp)^2)
SSE

#Problem 2.7
# second answer

#Problem 3.1
library(caret)
library(e1071)
set.seed(111)
numFolds = trainControl( method = "cv", number = 10 ) # 10 folds
cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 
train(Life.Exp ~., data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
#cp = 0.12


#Problem 3.2
stateCART4 = rpart(Life.Exp ~., data=statedata,cp = 0.12)
prp(stateCART4)

#Problem 3.3
predictTest = predict(stateCART4)
summary(predictTest)
SSE = sum((predictTest - statedata$Life.Exp)^2)
SSE


#Problem 3.4
set.seed(111)
train(Life.Exp ~Area, data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
#cp = 0.02
stateCART5 = rpart(Life.Exp ~Area, data=statedata,cp = 0.02)
prp(stateCART5)


#Problem 3.5
# 4

#Problem 3.6
# 9579  51000


#Problem 3.7
predictTest = predict(stateCART5)
summary(predictTest)
SSE = sum((predictTest - statedata$Life.Exp)^2)
SSE
#second answer
