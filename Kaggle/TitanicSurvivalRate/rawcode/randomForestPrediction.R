#Titanic prediction using Random Forrest

# This scored lower (0.77512) Compared to almost identical setup using ctree (0.78469)

rm(list=ls())

# Load libraries ---------------------------------------------
library(lattice)
library(randomForest)

# Load the method to fill in NA
source('imputeNA.R')

# read train and test data
train <- read.csv('train.csv', header = T)
test  <- read.csv('test.csv', header = T)

# converts 'Survived' to factors
train[,2] <- as.factor(train[,2])


# list rows of data that have missing values 
count.all.train <- nrow(train)
count.na.train  <- nrow(train[!complete.cases(train),])
pct.na.train    <- count.na.train * 100/ count.all.train
cat('\nThere are ', count.na.train, 'NAs ', round(pct.na.train,1) ,'% (total =', count.all.train ,') in train dataset!\n\n')

# Sample of only complete cases
train.complete <- train[complete.cases(train),]

# Crate copies of train/test data for cleaning NAs
train.est <- train
test.est  <- test

set.seed(94746)
train.est$Age    <- imputeNA(train.est$Age, fill="randomsample")
train.est$Fare   <- imputeNA(train.est$Fare, fill="randomsample")
train.est$Pclass <- imputeNA(train.est$Pclass, fill="randomsample")
train.est$Sex    <- imputeNA(train.est$Sex, fill="randomsample")
train.est$SibSp  <- imputeNA(train.est$SibSp, fill="randomsample")

test.est$Age    <- imputeNA(test.est$Age, fill="randomsample")
test.est$Fare   <- imputeNA(test.est$Fare, fill="randomsample")
test.est$Pclass <- imputeNA(test.est$Pclass, fill="randomsample")
test.est$Sex    <- imputeNA(test.est$Sex, fill="randomsample")
test.est$SibSp  <- imputeNA(test.est$SibSp, fill="randomsample")


# trainnig models
rf <- randomForest(Survived ~ Sex * Age * Pclass * SibSp, 
                   data = train.est, 
                   importance =TRUE,
                   norm.votes =TRUE,
                   ytest=train.est$Survived)
#rf <- randomForest(Survived ~ Sex * Age, data = train.est, importance =TRUE)
#str(rf)
print(importance(rf))
rf_pvalues_train = predict(rf,type="prob")    #this give the probabilities of each outcome
rf_pvalues_train
hist(rf_pvalues_train[,1])
hist(rf_pvalues_train[,2])

plot(rf)
plot(rf$err.rate)
table(predict(rf, test.est))

test.est$Survived <- predict(rf, test.est)

#print(head(test.est))
#print(test.est$Survived)

# Creating format for submission

survived   <- as.numeric(test.est$Survived) - 1
passengerId <- test.est$PassengerId
names <- c('PassengerId','Survived')
final.df <- data.frame(cbind(passengerId, survived))
names(final.df) <- names
outfile <- ('rf_v1.csv')
write.csv(final.df, file=outfile, row.names = F, quote =FALSE)
cat('\n Output written to', outfile,'\n')

# Summary of Rate of Survived predicted 
tot.survived <- sum( ifelse(survived ==1, 1, 0))
tot          <- nrow(test)
surv.pct     <- tot.survived * 100/ tot
cat ('Predicts a survival rate of = ', 
      surv.pct, ' (out of total ', tot , ' passengers)')
