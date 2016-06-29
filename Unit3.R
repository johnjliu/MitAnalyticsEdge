
####################Popularity of music records
songs = read.csv("songs.csv")
str(songs)
summary(songs$year)
nrow(subset(songs, year==2010))
table(songs$year)

nrow(subset(songs, artistname =="Michael Jackson"))

MichaelJackson = subset(songs, artistname == "Michael Jackson")
MichaelJackson

MichaelJackson[MichaelJackson$Top10 == TRUE, ]$songtitle

table(songs$timesignature)

songs[which.max(songs$tempo),]
songs$songtitle[which.max(songs$tempo)]

songsTrain = subset(songs,year < 2010)
songsTest = subset(songs, year == 2010)
str(songsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[ , !(names(songsTrain) %in% nonvars) ]
songsTest = songsTest[ , !(names(songsTest) %in% nonvars) ]
songsLog1 = glm(Top10 ~., data = songsTrain, family=binomial)
summary(songsLog1)

cor(songsTrain$loudness,songsTrain$energy)

songsLog2 = glm(Top10 ~.-loudness, data = songsTrain, family=binomial)
summary(songsLog2)

songsLog3 = glm(Top10 ~.-energy, data = songsTrain, family=binomial)
summary(songsLog3)

predictTest = predict(songsLog3, type="response", newdata=songsTest)
table(songsTest$Top10, predictTest > 0.45)
(309+19)/(309+5+40+19)

table(songsTest$Top10, predictTest >= 0.45)

nrow(subset(songsTest, songsTest$Top10 == 1 & predictTest >= 0.45))

19/(40+19)

309/(309+5)


###################Predicting parole violators
parole = read.csv("parole.csv")
str(parole)

table(parole$violator)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)
table(parole$state)

set.seed(144)
library(caTools)
split=sample.split(parole$violator, SplitRatio = 0.7)
paroleTrain = subset(parole, split == TRUE)
paroleTest = subset(parole, split==FALSE)


paroleLog1 = glm(violator~.,data=paroleTrain, family = binomial)
summary(paroleLog1)


exp(-4.2411574 + 0.3869904 * 1 + 0.8867192 * 1 -0.0001756 * 50 -0.1238867 * 3 +  0.0802954*12 + 0.6837143 * 2 )


predictTest = predict(paroleLog1, type="response", newdata=paroleTest)
summary(predictTest)
table(paroleTest$violator, predictTest > 0.5)
(12)/(12 + 11)
167/(167+12)
(167+12)/(167+12+11+12)

table(paroleTest$violator)
179/(179 + 23)

library(ROCR)
ROCRpred = prediction(predictTest, paroleTest$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)



#################Predicting loan repayment
loans=read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/(8045+1533)

loansWithNA = subset(loans, is.na(log.annual.inc)|is.na(days.with.cr.line)|is.na(revol.util)|is.na(inq.last.6mths)|is.na(delinq.2yrs)|is.na(pub.rec))
str(loansWithNA)
table(loansWithNA$not.fully.paid)
12/(50+12)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans),"not.fully.paid")
#imputed = complete(mice(loans[vars.for.imputation]))
imputed = read.csv("loans_imputed.csv")
str(imputed)
summary(imputed)
loans[vars.for.imputation] = imputed

set.seed(144)
split=sample.split(loans$not.fully.paid,SplitRatio = 0.7)
loansTrain = subset(loans, split==TRUE)
str(loansTrain)
loansTest = subset(loans, split==FALSE)
str(loansTest)
loans.log1 = glm(not.fully.paid~.,data=loansTrain, family="binomial")
summary(loans.log1)

# split=sample.split(imputed$not.fully.paid,SplitRatio = 0.7)
# imputedTrain = subset(imputed, split==TRUE)
# str(imputedTrain)
# imputedTest = subset(imputed, split==FALSE)
# str(imputedTest)
# loans.log1 = glm(not.fully.paid~.,data=imputedTrain, family="binomial")
# summary(loans.log1)



-9.317e-03*(700 - 710)

exp(-9.317e-03 * 700) /exp(-9.317e-03 * 710)


predictTest = predict(loans.log1, type="response", newdata=loansTest)
summary(predictTest)
table(loansTest$not.fully.paid, predictTest > 0.5)
(2400 + 3)/(2400 + 13 + 457 + 3)

table(loansTest$not.fully.paid)
2413/(2413 + 460)

library(ROCR)
ROCRpred = prediction(predictTest, loansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


loans.log2 = glm(not.fully.paid~int.rate,data=loansTrain,family = binomial)
summary(loans.log2)
cor(loansTrain$int.rate, loansTrain$fico)


predictTest = predict(loans.log2, type="response", newdata=loansTest)
summary(predictTest)
table(loansTest$not.fully.paid, predictTest > 0.5)

ROCRpred = prediction(predictTest, loansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

10 * exp(0.06 * 3)

loansTest$profit = exp(loansTest$int.rate*3) - 1
loansTest$profit[loansTest$not.fully.paid==1] = -1
max(loansTest$profit) * 10

highInterest = subset(loansTest,loansTest$int.rate >= 0.15)
str(highInterest)
summary(highInterest$profit)
table(highInterest$not.fully.paid)
110/(327+110)

highInterest$predicted.risk = predict(loans.log1, type="response", newdata=highInterest)
summary(highInterest$predicted.risk)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, highInterest$predicted.risk <= cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
