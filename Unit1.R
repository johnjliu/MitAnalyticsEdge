#mvtWeek1
CHICAGO=read.csv("mvtWeek1.csv")
str(CHICAGO)
summary(CHICAGO)

max(CHICAGO$ID)

min(CHICAGO$Beat)

nrow(subset(CHICAGO,Arrest==TRUE))
summary(CHICAGO)

CHICAGO[1,]$Date
CHICAGO$Date[1]

DateConvert=as.Date(strptime(CHICAGO$Date,"%m/%d/%y %H:%M"))
summary(DateConvert)


CHICAGO$Month=months(DateConvert)
CHICAGO$Weekday=weekdays(DateConvert)
CHICAGO$Date=DateConvert

table(CHICAGO$Month)
which.min(table(CHICAGO$Month))

table(CHICAGO$Weekday)
which.max(table(CHICAGO$Weekday))

table(CHICAGO$Arrest,CHICAGO$Month)

hist(CHICAGO$Date,breaks=14)

hist(CHICAGO[CHICAGO$Arrest==TRUE,]$Date,breaks=14)
boxplot(Date~Arrest,data=CHICAGO)

table(CHICAGO$Arrest,CHICAGO$Year)
summary(CHICAGO$Arrest)
2152/(18517 + 2152)

1212/(1212+13068)

550/(550+13542)


sort(table(CHICAGO$LocationDescription))

nrow(subset(CHICAGO,LocationDescription=='STREET' | LocationDescription=='PARKING LOT/GARAGE(NON.RESID.)'| LocationDescription=='ALLEY'|LocationDescription=='GAS STATION'|LocationDescription=='DRIVEWAY - RESIDENTIAL'))
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5=subset(CHICAGO,LocationDescription %in% TopLocations)
summary(Top5)


Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription,Top5$Arrest)
439/(439+1672)

table(Top5$LocationDescription,Top5$Weekday)


###############Stock dynamics###############################
IBM=read.csv("IBMStock.csv")
str(IBM)
fix(IBM)
GE=read.csv("GEStock.csv")
str(GE)
PG=read.csv("ProcterGambleStock.csv")
str(PG)
COKE=read.csv("CocaColaStock.csv")
str(COKE)
BOEING=read.csv("BoeingStock.csv")
str(BOEING)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
str(IBM)
GE$Date = as.Date(GE$Date, "%m/%d/%y")
str(GE)
COKE$Date = as.Date(COKE$Date, "%m/%d/%y")
str(COKE)
PG$Date = as.Date(PG$Date, "%m/%d/%y")
str(PG)
BOEING$Date = as.Date(BOEING$Date, "%m/%d/%y")
str(BOEING)

min(IBM$Date)

max(IBM$Date)

mean(IBM$StockPrice)

min(GE$StockPrice)

max(COKE$StockPrice)

median(BOEING$StockPrice)

sd(PG$StockPrice)

plot(COKE$Date, COKE$StockPrice,type="l",col="red")
lines(PG$Date,PG$StockPrice,lty=2)
abline(v=as.Date(c("2000-03-01")),lwd=1)

plot(COKE$Date[301:432], COKE$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(PG$Date[301:432],PG$StockPrice[301:432],col="blue")
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col="green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col="purple")
lines(BOEING$Date[301:432],BOEING$StockPrice[301:432])

abline(v=as.Date(c("1997-09-01")),lwd=1)
abline(v=as.Date(c("1997-11-01")),lwd=1)

abline(v=as.Date(c("2004-01-01")),lwd=1)
abline(v=as.Date(c("2005-12-31")),lwd=1)

sort(tapply(IBM$StockPrice,months(IBM$Date),mean))
mean(IBM$StockPrice)

sort(tapply(GE$StockPrice,months(GE$Date),mean))
mean(GE$StockPrice)
sort(tapply(COKE$StockPrice,months(COKE$Date),mean))
mean(COKE$StockPrice)



###########################demographics and employment in the united states
CPS=read.csv("CPSData.csv")
str(CPS)
summary(CPS)


sort(table(CPS$Industry))

sort(table(CPS$State))

table(CPS$Citizenship)
(116639+7073) / sum(table(CPS$Citizenship))

table(CPS$Race)
table(CPS$Race, CPS$Hispanic)

summary(CPS)

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))


table(CPS$State, is.na(CPS$MetroAreaCode))

table(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

str(CPS$MetroAreaCode)
MetroAreaMap=read.csv("MetroAreaCodes.csv")
str(MetroAreaMap)

str(CPS$CountryOfBirthCode)
CountryMap=read.csv("CountryCodes.csv")
str(CountryMap)

CPS=merge(CPS,MetroAreaMap,by.x="MetroAreaCode",by.y="Code",all.x=TRUE)
str(CPS)
summary(CPS)

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))

sort(tapply(CPS$Education=="No high school diploma",CPS$MetroArea,mean,na.rm=TRUE))

CPS=merge(CPS,CountryMap,by.x="CountryOfBirthCode",by.y="Code",all.x=TRUE)
str(CPS)
summary(CPS)

sort(table(CPS$Country))

table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",CPS$Country!="United States")
1668/(1668+3736)

sort(tapply(CPS$Country=="India",CPS$MetroArea,sum,na.rm=TRUE))

sort(tapply(CPS$Country=="Brazil",CPS$MetroArea,sum,na.rm=TRUE))

sort(tapply(CPS$Country=="Somalia",CPS$MetroArea,sum,na.rm=TRUE))
