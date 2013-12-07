# generating ROC curve using rpart
rm(list=ls())

require('party')
require('ROCR')
# Load the kyphosis data set.
require(rpart)

set.seed(1234)
# Read Titanic data
train <- read.csv('train.csv', header = T)
test  <- read.csv('test.csv' , header = T)
str(train)
str(test)

# This block is to test out the 'PerformanceAnalytics' library
# which produces a nice correlation plots
# train$Name <- NULL
# train$PassengerId <- NULL
# train$Cabin <- NULL
# train$Ticket <- NULL
# train$Parch <- NULL
# train$Embarked <- as.integer(train$Embarked)
# train$Sex <- as.integer(train$Sex)
# str(train)
# library(PerformanceAnalytics)
# chart.Correlation(train,col=ifelse(train$Survived,1,2))


# converts 'Survived' to factors -----------------------------------------
train[,2] <- as.factor(train[,2])
#do not do this! there is no Survived column in test data.
#test[,2]  <- as.factor(test[,2])


# list rows of data that have missing values 
count.all.train <- nrow(train)
count.na.train  <- nrow( train[ ! complete.cases(train) , ] )
pct.na.train    <- count.na.train * 100 / count.all.train
cat('There are ', count.na.train, 'NAs ', round(pct.na.train,1) ,'% (total =', count.all.train ,') in train dataset!\n')

# lets try filling in some Age NAs
source('imputeNA.R')
train$Age[is.na(train$Age)] <- imputeNA(train$Age, fill="randomsample")

train.complete   <- train[ complete.cases(train), ]
train.incomplete <- train[ ! complete.cases(train), ]
test.complete    <- test[ complete.cases(test), ]
test.incomplete  <- test[ ! complete.cases(test), ]

# Including NAs
#train.complete   <- train
#test.complete    <- test

# Split randomly # use 75% for training
x <- train.complete[sample(1:nrow(train.complete), nrow(train.complete), replace = F),]
frac <- 0.75
x.train    <- train.complete[1:floor(nrow(x) * frac), ] 
x.evaluate <- train.complete[(floor(nrow(x) * frac) + 1):nrow(x), ]
head(x)

# Create a model using "random forest and bagging ensemble algorithms
# utilizing conditional inference trees."
require(party)
x.model <- cforest(Survived ~ Age + Sex + SibSp + Pclass, data=x.train,
                   control = cforest_unbiased(mtry = 3))

# Alternatively, use "recursive partitioning [...] in a conditional
# inference framework."
# x.model <- ctree(Kyphosis ~ Age + Number + Start, data=x.train)
y.model <- ctree(Survived ~ Age + Sex + SibSp + Pclass, data=x.train,
                   control = cforest_unbiased(mtry = 3))
library(e1071)
z.model <- naiveBayes(Survived ~ Age + Sex + SibSp + Pclass, data=x.train)



# ctree plots nicely (but cforest doesn"t plot)
#plot (y.model)

# Use the model to predict the evaluation.
x.evaluate$prediction  <- predict(x.model, newdata=x.evaluate)
x.evaluate$prediction2 <- predict(y.model, newdata=x.evaluate)
x.evaluate$prediction3 <- predict(z.model, newdata=x.evaluate)

# Calculate the overall accuracy.
x.evaluate$correct  <- x.evaluate$prediction  == x.evaluate$Survived
x.evaluate$correct2 <- x.evaluate$prediction2 == x.evaluate$Survived
x.evaluate$correct3 <- x.evaluate$prediction3 == x.evaluate$Survived

print('')
print(paste("% of predicted classifications correct model.x (cforest)   :", round(mean(x.evaluate$correct), 2) ))
print(paste("% of predicted classifications correct model.y (ctree)     :", round(mean(x.evaluate$correct2), 2) ))
print(paste("% of predicted classifications correct model.z (naiveBayes):", round(mean(x.evaluate$correct3), 2) ))

# Extract the class probabilities.
x.evaluate$probabilities <- 1- unlist(treeresponse(x.model,
                                                   newdata=x.evaluate), use.names=F)[seq(1,nrow(x.evaluate)*2,2)]
x.evaluate$probabilities2 <- 1- unlist(treeresponse(y.model,
                                                   newdata=x.evaluate), use.names=F)[seq(1,nrow(x.evaluate)*2,2)]
#x.evaluate$probabilities3 <- 1- unlist(treeresponse(z.model,
#                                                    newdata=x.evaluate), use.names=F)[seq(1,nrow(x.evaluate)*2,2)]


# Plot the performance of the model applied to the evaluation set as
# an ROC curve.
require(ROCR)
pred <- prediction(x.evaluate$probabilities, x.evaluate$Survived)
perf <- performance(pred, "tpr", "fpr")
pred2 <- prediction(x.evaluate$probabilities2, x.evaluate$Survived)
perf2 <- performance(pred2, "tpr", "fpr")
#pred3 <- prediction(x.evaluate$probabilities3, x.evaluate$Survived)
#perf3 <- performance(pred3, "tpr", "fpr")


par(mfrow=c(1,2))
plot(perf, main="ROC curve: Model.x", colorize=F, col='blue')
plot(perf2, add=T, colorize=F, col='red')
#plot(perf3, add=T, colorize=F, col='green')

# And then a lift chart
perf   <- performance(pred,"lift","rpp")
perf2  <- performance(pred2,"lift","rpp")
#perf3 <- performance(pred3,"lift","rpp")

plot(perf, main="lift curve: Model.x", colorize=F, col='blue')
plot(perf2, add=T, colorize=F, col='red')
#plot(perf3, add=T, colorize=F, col='green')

#write cforest prediction to file
outname <- 'cforest_v1.csv'
cat('Summary of test now!!\n')
str(test)
head(test)
PassengerId <- test$PassengerId
Survived    <- predict(x.model, newdata = test)
test$predictionx <- predict(x.model, newdata=test)

Survived <- as.numeric(Survived) - 1
df <- data.frame(cbind(PassengerId, Survived))
str(df)
write.csv(df, file=outname, row.names = F, quote =FALSE )