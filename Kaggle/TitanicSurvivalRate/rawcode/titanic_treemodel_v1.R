#load libraries
library(lattice)
library(party)

rm(list=ls())

model_summary <- function(test, ans) 
{
  # Peform a quick summary of a model by comparing to the 'test' data.
  
  comp <- ifelse(test$Survived==ans$Survived,1,0)  
  print(comp)
  #test.sum <- sum(test$Survived)
  #print(test.sum)
}


# read train and test data
train <- read.csv('train.csv', header = T)
test  <- read.csv('test.csv', header = T)

# converts 'Survived' to factors
train[,2] <- as.factor(train[,2])

# trainnig models
#ct2 <- ctree(Survived ~ Sex * Age * Parch * SibSp, data = train)
ct3 <- ctree(Survived ~ Sex * Age * Pclass * SibSp, data = train)
# When above 2 models are combined, Parch is ignored (pruned)
#ct4 <- ctree(Survived ~ Sex * Age * Parch * Pclass * SibSp, data = train) 

treeresponse(ct3)
tapply(treeresponse(ct3), where(ct3), unique)

#str(ct2)
#quartz('ct2')
#plot(ct2)
#quartz('ct3')
plot(ct3)
#quartz('ct4')
#plot(ct4)

test$Survived <- predict(ct3, test)
test$Pclass <- NULL
test$Name <- NULL
test$Sex <- NULL
test$Age <- NULL
test$SibSp <- NULL
test$Parch <- NULL
test$Ticket <- NULL
test$Fare <- NULL
test$Cabin <- NULL
test$Embarked <- NULL
print(head(test))
print(test$Survived)
write.csv(test, file='treemodel_ct3.csv', row.names = F, quote =FALSE )


#model_summary(test, ans)
