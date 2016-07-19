letters=read.csv("letters_ABPR.csv")
str(letters)
head(letters)

#Problem 1.1
letters$isB=as.factor(letters$letter=="B")
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain=subset(letters,split == TRUE)
lettersTest=subset(letters,split == FALSE)
table(lettersTest$isB)
1175/nrow(lettersTest)

#Problem 1.2
library(rpart)
library(rpart.plot)
cartB=rpart(isB ~ .-letter,data=lettersTrain, method = "class")
prp(cartB)
letters_pred = predict(cartB,newdata = lettersTest, type = "class")
table(lettersTest$isB, letters_pred)
(1118+340)/nrow(lettersTest)

#Problem 1.3
library(randomForest)
set.seed(1000)
forestB=randomForest(isB ~ .-letter, data=lettersTrain)
letters_pred2 = predict(forestB,newdata = lettersTest)
table(lettersTest$isB, letters_pred2)
(1166+374)/nrow(lettersTest)


#Problem 2.1
letters$letter=as.factor(letters$letter)
str(letters)
set.seed(1000)
split = sample.split(letters$letter, SplitRatio = 0.5)
lettersTrain=subset(letters,split == TRUE)
lettersTest=subset(letters,split == FALSE)
table(lettersTest$letter)
401/nrow(lettersTest)

#Problem 2.2
cartAll=rpart(letter ~ .-isB,data=lettersTrain, method = "class")
prp(cartAll)
letters_pred2 = predict(cartAll,newdata = lettersTest, type = "class")
table(lettersTest$letter, letters_pred2)
(350+314+366+338)/nrow(lettersTest)

#Problem 2.3
set.seed(1000)
#forestB=randomForest(letter ~ .-isB,data=lettersTrain)
forestAll=randomForest(letter ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=lettersTrain)
letters_predAll = predict(forestAll,newdata = lettersTest)
table(lettersTest$letter, letters_predAll)
(395+374+397+362)/nrow(lettersTest)

