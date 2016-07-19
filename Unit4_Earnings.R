census=read.csv("census.csv")
str(census)
summary(census)
head(census)

#Problem 1.1
set.seed(2000)
split=sample.split(census$over50k,SplitRatio = 0.6)
censusTrain = subset(census, split==TRUE)
str(censusTrain)
censusTest = subset(census, split==FALSE)
str(censusTest)
census.log1 = glm(over50k ~.,data=census, family="binomial")
summary(census.log1)

#Problem 1.2
predictTest = predict(census.log1, type="response", newdata=censusTest)
summary(predictTest)
table(censusTest$over50k, predictTest > 0.5)
(9054+1887)/nrow(censusTest)

#Problem 1.3
table(censusTest$over50k)
9713/nrow(censusTest)

#Problem 1.4
library(ROCR)
ROCRpred = prediction(predictTest, censusTest$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)


#Problem 2.1
library(rpart)
library(rpart.plot)
census.CART1 = rpart(over50k ~., data=censusTrain,method="class")
prp(census.CART1)

#Problem 2.2
#Relationship

#Problem 2.3
#Education & Capital Gain

#problem 2.4
predictTest = predict(census.CART1, type="class", newdata=censusTest)
summary(predictTest)
table(censusTest$over50k, predictTest)
(9243+1596)/nrow(censusTest)

#Problem 2.6
predictTest = predict(census.CART1, newdata=censusTest)
predictTest = predictTest[,2]
ROCRpred = prediction(predictTest, censusTest$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)




#Problem 3.1
library(caret)
library(e1071)
set.seed(1)
trainSmall = censusTrain[sample(nrow(censusTrain), 2000), ]
set.seed(1)
forestSmall=randomForest(over50k ~., data=trainSmall)
forestSmallTest = predict(forestSmall,newdata = censusTest)
table(censusTest$over50k, forestSmallTest)
(9586 + 1093)/nrow(censusTest)

#Problem 3.2
vu = varUsed(forestSmall, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forestSmall$forest$xlevels[vusorted$ix]))

#Problem 3.3
varImpPlot(forestSmall)


#Problem 4.1
numFolds = trainControl( method = "cv", number = 10 ) # 10 folds
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
train(over50k ~., data = censusTrain, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
#cp = 0.002


#Problem 4.2
censusCV = rpart(over50k ~., data = censusTrain, method="class", cp = 0.002)
PredictCV = predict(censusCV, newdata = censusTest, type = "class")
table(censusTest$over50k, PredictCV)
(9178+1838)/nrow(censusTest)

#Problem 4.3
prp(censusCV)
