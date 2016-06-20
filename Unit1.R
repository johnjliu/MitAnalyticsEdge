WHO=read.csv("WHO.csv")
str(WHO)
summary(WHO$Over60)
which.min(WHO$Over60)
WHO[183,]
which.max(WHO$LiteracyRate)
WHO[44,]

tapply(WHO$ChildMortality,WHO$Region,mean)

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



