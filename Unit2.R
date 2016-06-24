
###############Climate Change
climateData = read.csv("climate_change.csv")
head(climateData)
str(climateData)

climate_train = subset(climateData,Year<=2006)
str(climate_train)

climate_test = subset(climateData, Year>2006)
str(climate_test)

reg1 =  lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=climate_train)
summary(reg1)

cor(climate_test)

reg2 =  lm(Temp ~ MEI + N2O  + TSI + Aerosols,data=climate_train)
summary(reg2)

regStep = step(reg1)
summary(regStep)

pred = predict(regStep,newdata = climate_test)
summary(pred)


SSE = sum((pred - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2


###################Reading Test Scores
pisaTrain = read.csv("pisa2009train.csv")
str(pisaTrain)
fix(pisaTrain)

pisaTest = read.csv("pisa2009Test.csv")

tapply(pisaTrain$readingScore,pisaTrain$male==1,mean)

summary(pisaTrain)

pisaTrain=na.omit(pisaTrain)
str(pisaTrain)

pisaTest=na.omit(pisaTest)
nrow(pisaTest)

str(pisaTrain)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore=lm(readingScore ~., data=pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE


predScores = predict(lmScore,newdata = pisaTest)
summary(predScores)
range(predScores)
637.7-353.2

SSE_Test = sum((predScores - pisaTest$readingScore)^2)
RMSE_TEST = sqrt(SSE_Test/nrow(pisaTest))
SSE_Test
RMSE_TEST

mean(pisaTrain$readingScore)
SST_Test = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST_Test

R2_Test = 1 - SSE_Test/SST_Test
R2_Test


#####################Detecting Flu Epidemics via Search Engine Query Data
fluTrain = read.csv("FluTrain.csv")
str(fluTrain)
head(fluTrain)
summary(fluTrain)

which.max(fluTrain$ILI)
fluTrain[303,]

which.max(fluTrain$Queries)
fluTrain[303,]

hist(fluTrain$ILI)

plot(fluTrain$Queries,log(fluTrain$ILI))

logReg1 = lm(log(ILI)~Queries, data=fluTrain)
summary(logReg1)

cor(log(fluTrain$ILI),fluTrain$Queries) ^ 2

fluTest = read.csv("FluTest.csv")
predTest1 = exp(predict(logReg1,newdata = fluTest))
predTest1
fluTest$Week
which(fluTest$Week == '2012-03-11 - 2012-03-17')
predTest1[11]

(fluTest$ILI[11] - predTest1[11])/fluTest$ILI[11]
SSE_flu = sum((predTest1 - fluTest$ILI)^2)
RMSE_flu = sqrt(SSE_flu/nrow(fluTest))
RMSE_flu

library("zoo")
ILILag2=lag(zoo(fluTrain$ILI),-2,na.pad = TRUE)
head(ILILag2)
fluTrain$ILILag2 = coredata(ILILag2)
str(fluTrain)
summary(fluTrain)

plot(log(fluTrain$ILILag2),log(fluTrain$ILI))

logReg2 = lm(log(ILI)~Queries + log(ILILag2), data=fluTrain)
summary(logReg2)


ILILag2=lag(zoo(fluTest$ILI),-2,na.pad = TRUE)
head(ILILag2)
fluTest$ILILag2 = coredata(ILILag2)
str(fluTest)

fluTest$ILILag2[1] = fluTrain$ILI[416]
fluTest$ILILag2[2] = fluTrain$ILI[417]


predTest2 = exp(predict(logReg2,newdata = fluTest))
predTest2

SSE_flu2 = sum((predTest2 - fluTest$ILI)^2)
RMSE_flu2 = sqrt(SSE_flu2/nrow(fluTest))
RMSE_flu2


#################State Data
stateData = read.csv("statedata.csv")
str(stateData)
head(stateData)

plot(stateData$x, stateData$y)

sort(tapply(stateData$HS.Grad,stateData$state.region,mean))

boxplot( Murder ~state.region, data=stateData)
tapply(stateData$Murder,stateData$state.region, median )

NEData = subset(stateData, state.region=="Northeast")
NEData
which.max(NEData$Murder)
NEData[6,]

LE.Model1 = lm(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area, data = stateData )
summary(LE.Model1)

plot(stateData$Income, stateData$Life.Exp)

LE.Model2 = lm(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost, data = stateData )
summary(LE.Model2)

LE.Model3 = lm(Life.Exp ~ Population+ Income+ Murder+ HS.Grad+ Frost, data = stateData )
summary(LE.Model3)

LE.Model4 = lm(Life.Exp ~ Population+ Murder+ HS.Grad+ Frost, data = stateData )
summary(LE.Model4)

sort(predict(LE.Model4))
which.min(predict(LE.Model4))
stateData[1,]

which.min(stateData$Life.Exp)
stateData[40,]

stateData[which.max(predict(LE.Model4)),]
stateData[which.max(stateData$Life.Exp),]

sort(abs(LE.Model4$residuals))
stateData[which.min(abs(LE.Model4$residuals)),]
stateData[which.max(abs(LE.Model4$residuals)),]



####################Forecasting Elantra Sales 
elantra = read.csv("elantra.csv")
head(elantra)
str(elantra)
elantraTrain = subset(elantra, Year <= 2012)
str(elantraTrain)
elantraTest = subset(elantra,Year > 2012)

salesModel1 = lm(ElantraSales~Unemployment +CPI_all+CPI_energy+Queries,data=elantraTrain)
summary(salesModel1)

salesModel2 = lm(ElantraSales~Month + Unemployment +CPI_all+CPI_energy+Queries,data=elantraTrain)
summary(salesModel2)

110.69 * 2
110.69 * 4

elantraTrain$MonthFactor = as.factor(elantraTrain$Month)
elantraTest$MonthFactor = as.factor(elantraTest$Month)
salesModel3 = lm(ElantraSales~MonthFactor + Unemployment +CPI_all+CPI_energy+Queries,data=elantraTrain)
summary(salesModel3)

pairs(elantraTrain)
cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

salesModel4 = lm(ElantraSales~MonthFactor + Unemployment +CPI_all+CPI_energy,data=elantraTrain)
summary(salesModel4)

salesPred4 = predict(salesModel4,newdata = elantraTest)
salesPred4
SSE_sales = sum((salesPred4 - elantraTest$ElantraSales)^2)
SSE_sales

mean(elantraTrain$ElantraSales)

SST_sales = sum((mean(elantraTrain$ElantraSales)- elantraTest$ElantraSales)^2)
R2_sales_test = 1 - SSE_sales/SST_sales
R2_sales_test

max(abs(salesPred4 - elantraTest$ElantraSales))

which.max(abs(salesPred4 - elantraTest$ElantraSales))  #returns 14 5 the fifth row
elantraTest[5,]
