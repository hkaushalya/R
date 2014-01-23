rm(list=ls())
WORKING_DIR <- '/Users/samantha/Documents/Personal/Samantha/LearningMaterials/IntroToR/RandomExercises/HandDigitRecognition/'
DATASET_DIR <- './data/'
FIGURE_DIR  <- './figures/'
COLORS <- c('white','black')
setwd(WORKING_DIR)
getwd()

library(RColorBrewer)
library(ElemStatLearn)
library(foreign)
library(tree)
library(maptree)
library(rpart)
library(RWeka)
library(FNN)
library(e1071)

## Set Color 
CUSTOM_COLORS <- colorRampPalette(colors = COLORS)
CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, 'Set3'))
## Figures Label
opts_chunk$set(fig.path = 'figures/plot-handwritten') # ????

## Load data
DATASET.train <- as.data.frame(zip.train)
DATASET.test  <- as.data.frame(zip.test)

head(DATASET.train)

sapply(DATASET.train[1, ], class)

## Transform Label as Factor (Categorical) and Change Column Names
## (TRAINING dataset)
DATASET.train[, 1] <- as.factor(DATASET.train[, 1])  # As Category
colnames(DATASET.train) <- c('Y', paste('X', 1:256, sep=""))
class(DATASET.train[, 1])
levels(DATASET.train[, 1])

sapply(DATASET.train[1, ], class)


## Transform Label as Factor (Categorical) and Change Column Names
## (TESTING dataset)
DATASET.test[, 1] <- as.factor(DATASET.test[, 1])  # As Category
colnames(DATASET.test) <- c('Y', paste('X', 1:256, sep=""))
class(DATASET.test[, 1])
levels(DATASET.test[, 1])

sapply(DATASET.test[1, ], class)

## Plot the average image of each digit
par(mfrow=c(4,3),pty ='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
images_digits_0_9 <- array(dim=c(10,16*16))
for(digit in 0:9)
{
  print(digit)
  images_digits_0_9[digit+1,]<-apply(DATASET.train[DATASET.train[,1]==digit,-1], 2, sum)
  images_digits_0_9[digit+1,]<-images_digits_0_9[digit+1,]/max(images_digits_0_9[digit+1,])*255
  
  z <- array(images_digits_0_9[digit+1,], dim=c(16,16))
  z <- z[,16:1] ##right side up
  image(1:16,1:16, z, main=digit, col=CUSTOM_COLORS(256))
}

# Classification: RPart
pc <- proc.time()
model.rpart <- rpart(DATASET.train$Y ~ ., method = 'class', data = DATASET.train)
proc.time() - pc
printcp(model.rpart)
plot(model.rpart, uniform=TRUE, main='Classification (RPART)')
text(model.rpart, all =TRUE, cex = 0.75)

draw.tree(model.rpart, cex=0.5, nodeinfo=TRUE, col=gray(0:8/8))

## Confusion Matrix (RPart)
prediction.rpart <- predict(model.rpart, newdata =DATASET.test, type = 'class')
table(`Actual Class` = DATASET.test$Y, `Predicted Class`= prediction.rpart)

error.rate.rpart <- sum(DATASET.test$Y != prediction.rpart)/nrow(DATASET.test)
print(paste0("Accuracy (Precision): ", 1 - error.rate.rpart))

write.csv(prediction.rpart,file='output/result_rpart_v1.csv')


