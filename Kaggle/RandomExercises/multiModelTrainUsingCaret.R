# Training multiple models for classification using the same dataset
# http://stats.stackexchange.com/questions/11054/training-multiple-models-for-classification-using-the-same-dataset

#Setup
rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
set.seed(123)

#Pretend we only care about virginica
Data <- iris
virginica <- Data$Species=='virginica'
Data$Species <- NULL

#Look at the variable relationships
library(PerformanceAnalytics)
chart.Correlation(Data,col=ifelse(virginica,1,2))

#Create cross-validation folds to use for multiple models
#Use 10-fold CV, repeat 5 times
library(caret)
MyFolds <- createMultiFolds(virginica, k = 10, times = 5)
MyControl <- trainControl(method = "repeatedCV", index = MyFolds,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE)

#Define Equation for Models
fmla <- as.formula(paste("virginica ~ ", paste(names(Data), collapse= "+")))

#Fit some models
Data$virginica <- as.factor(ifelse(virginica,'Yes','No'))

svmModel <- train(fmla,Data,method='svmRadial',
                  tuneLength=3,metric='ROC',trControl=MyControl)

rfModel <- train(fmla,Data,method='rf',
                 tuneLength=3,metric='ROC',trControl=MyControl)

#Compare Models
resamps <- resamples(list(
  SVM = svmModel,
  RandomForest = rfModel
))
summary(resamps)
densityplot(resamps,auto.key = TRUE, metric='ROC')