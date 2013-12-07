#load libraries
library(lattice)
library(party)

#read train and test data
train <- read.csv('train.csv', header = T)
test  <- read.csv('test.csv', header = T)

head(train)
pairs(train)
plot(train)

train[,2] <- as.factor(train[,2])
cut.age <- equal.count(train$Age,4)
train.survived <- subset(train, Survived == 1)
train.deceased <- subset(train, Survived == 0)

par(mfrow=c(3,2))
hist(train.survived$Age)
hist(train.deceased$Age)

hist(train.survived$SibSp)
hist(train.deceased$SibSp)
hist(train.survived$Parch)
hist(train.deceased$Parch)


xyplot(Survived ~ Age, data=train, facets=cut.age)
ct1 <- ctree(Survived ~ Sex * Age * Parch * SibSp, data = train)
ct2 <- ctree(Survived ~ Sex, data = train)
par(mfrow=c(2,2))
plot(ct1)
plot(ct2)


