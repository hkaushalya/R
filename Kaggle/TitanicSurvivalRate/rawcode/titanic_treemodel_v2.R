rm(list=ls())

#load libraries
library(lattice)
library(party)
library(fGarch)


# function to fills in NAs
impute.NA <- function(x, fill="mean"){
  if (fill=="mean")
  {
    cat ('n before = ', length(x))
    x_complete <- ifelse(is.na(x), mean(x, na.rm=TRUE), x)
    cat ('n after  = ', length(x_complete))
    
  } else if (fill=="median")
  {
    x_complete <- ifelse(is.na(x), median(x, na.rm=TRUE), x)
  } else if (fill == "randomsample")
  {
    x_complete <- ifelse(is.na(x), 
                          sample(x[!is.na(x)], size=1,replace=TRUE),
                          x)
    
  } else {
    stop('Undefined fill method called!')
  }
  
  return(x_complete)
}

#traindata$age = impute.NA(traindata$age,fill="median")
#testdata$age = impute.NA(testdata$age,fill="median")

#traindata$fare = impute.NA(traindata$fare,fill="median")
#testdata$fare = impute.NA(testdata$fare,fill="median")


# Simple random imputation ------------------------------------------------
#The simplest approach is to impute missing values of earnings based on the observed
#data for this variable. We can write this as an R function:
#random.imp <- function (a){
#    missing <- is.na(a)
#    n.missing <- sum(missing)
#    a.obs <- a[!missing]
#    imputed <- a
#    imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
#    return (imputed)
#}

# read train and test data
train <- read.csv('train.csv', header = T)
test  <- read.csv('test.csv', header = T)

# list rows of data that have missing values 
count.all.train <- nrow(train)
count.na.train  <- nrow( train[ ! complete.cases(train) , ] )
pct.na.train    <- count.na.train * 100 / count.all.train
cat('There are ', count.na.train, 'NAs ', round(pct.na.train,1) ,'% (total =', count.all.train ,') in train dataset!')

train.complete <- train[ complete.cases(train), ]

# 20% of missing data. need to model missing values
# Need to model Age to fill in the NA
# plot ages as 
plot(Age ~ Sex, data=train) #the age distribution of the two groups are similar
hist(train.complete$Age)
# hist method ignores NAs. so no need to use only 'complete' records
#r <- hist(train.complete$Age, n = 25, probability = TRUE, border = "white", col = "steelblue")
r <- hist(train$Age, n = 25, probability = TRUE, border = "white", col = "steelblue")

#plot(r)
#str(r)

# How many Age values are missing ----------------------------------------
n.missing.Age <- nrow( train[ which( is.na (train$Age) ) , ] )
n.missing.Age.in.test <- nrow( test[ which( is.na (test$Age) ) , ] )

cat('n.missing.Age (train) = ', n.missing.Age)
cat('n.missing.Age (test)  = ', n.missing.Age.in.test)


# Replace NAs in Age with sampling of Age distribution --------------------

#draw samplings for n.missing.Age
#drawn.Ages <- sample(train.complete$Age, size=n.missing.Age,replace=TRUE)
#hist(draws,n=25, probability=T)
#drawn.Ages


train.est <- train   # create new df
test.est  <- train   # create new df

set.seed(94746)

# The imputation of NA from test data does not seem to improve
# the predictions.

#train.est$Age[is.na(train.est$Age)] <- sample(train.complete$Age, size=n.missing.Age,replace=TRUE)
test.est$Age[is.na(test.est$Age)] <- sample(test$Age, size=n.missing.Age.in.test, replace=TRUE)

train.est$Age[is.na(train.est$Age)] <- impute.NA(train.est$Age, fill="randomsample")
train.est$Fare[is.na(train.est$Fare)] <- impute.NA(train.est$Fare, fill="randomsample")
train.est$Pclass[is.na(train.est$Pclass)] <- impute.NA(train.est$Pclass, fill="randomsample")
train.est$Sex[is.na(train.est$Sex)] <- impute.NA(train.est$Sex, fill="randomsample")

test.est$Age[is.na(test.est$Age)]   <- impute.NA(test.est$Age, fill="randomsample")
test.est$Fare[is.na(test.est$Fare)] <- impute.NA(test.est$Fare, fill="randomsample")
test.est$Pclass[is.na(test.est$Pclass)] <- impute.NA(test.est$Pclass, fill="randomsample")
test.est$Sex[is.na(test.est$Sex)] <- impute.NA(test.est$Sex, fill="randomsample")



#train.est$Age

par(mfrow=c(1,2))
hist(train.est$Age, n=25, probability=T)
hist(test$Age, n=25, probability=T)


# converts 'Survived' to factors -----------------------------------------
train.est[,2] <- as.factor(train.est[,2])

# Train and tune randomForest model --------------------------------------
ct <- ctree(Survived ~ Sex * Age * Pclass * SibSp * Fare, data = train.est)
treeresponse(ct)   # returns the probabilities of the outcomes for each test data item 
tapply(treeresponse(ct), where(ct), unique)

plot(ct)
confint(ct)

# Prepare Final Output for a file -----------------------------------------
test.cp <- test
test.cp$Survived <- predict(ct, test)
test.cp$Pclass <- NULL
test.cp$Name <- NULL
test.cp$Sex <- NULL
test.cp$Age <- NULL
test.cp$SibSp <- NULL
test.cp$Parch <- NULL
test.cp$Ticket <- NULL
test.cp$Fare <- NULL
test.cp$Cabin <- NULL
test.cp$Embarked <- NULL
#print(head(test))
#print(tail(test))

#print(test.cp$Survived)
write.csv(test.cp, file='ct_trainandtestAgeNAtreated.csv', row.names = F, quote =FALSE )

