# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)


# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

-804.63 + 2737.77 * 0.338 + 1584.91 * 0.54
-804.63 + 2737.77 * 0.391 + 1584.91 * 0.45
-804.63 + 2737.77 * 0.369 + 1584.91 * 0.374
-804.63 + 2737.77 * 0.313 + 1584.91 * 0.447
-804.63 + 2737.77 * 0.361 + 1584.91 * 0.5


teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor2012 = cor(teamRank,wins2012)
cor2012

cor2013 = cor(teamRank,wins2013)
cor2013
