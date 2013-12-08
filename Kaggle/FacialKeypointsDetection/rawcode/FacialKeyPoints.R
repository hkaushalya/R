rm(list=ls())
setwd('/Users/samantha/Documents/Personal/Samantha/LearningMaterials/IntroToR/KaggleCompetitions/FacialKeypointsDetection/')
load('data.Rd')

#===============================================================
# show one or more sample image from the train data
# with several features highlighted
#
# Inputs : row.no : number of a sequence of row numbers
# Outputs: draws the image/s with some features highlighted
#===============================================================
SampleImage <- function(row.no) {
  for (r in row.no) {
    im <- matrix(data=rev(im.train[r,]), nrow=96, ncol=96)
    image(1:96, 1:96, im, col=gray((0:255)/255))
    points(96-d.train$nose_tip_x[r],         96-d.train$nose_tip_y[r],         col="red")
    points(96-d.train$left_eye_center_x[r],  96-d.train$left_eye_center_y[r],  col="blue")
    points(96-d.train$right_eye_center_x[r], 96-d.train$right_eye_center_y[r], col="green")
  }
}
#===============================================================
# This is to overlay certain feature in all images
# on top of one sample image to see how dispersed that
# feature in the train data.
#
# Inputs: r: row number
#         feature: feature to draw (nose, eye etc)
# Output: Draws a sample image(r) and overlay selected feature
#         of all other images in the train data
#===============================================================
SampleImFtOverlaid <- function(r, feature) {
  SampleImage(r)
  if (feature=='nose' || feature =='all') {
    for(i in 1:nrow(d.train)) {
      points(96-d.train$nose_tip_x[i], 96-d.train$nose_tip_y[i], col="red")
    }
  } else if (feature=='lefteye') {
    for(i in 1:nrow(d.train)) {
      points(96-d.train$left_eye_center_x[i],  96-d.train$left_eye_center_y[i],  col="blue")
    }
  } else if (feature=='righteye') {
    for(i in 1:nrow(d.train)) {
      points(96-d.train$right_eye_center_x[i], 96-d.train$right_eye_center_y[i], col="green")
    }
  }
}

ColorGradientPlot <- function(r, feature) {
  SampleImage(r)
  for(i in 1:nrow(d.train)) {
    points(96-d.train$left_eye_center_x[i],  96-d.train$left_eye_center_y[i],  col="blue")
  }
}

NACheck <-function(df) {
  print(paste("======= Missing Value Report ======"))
  ncols <- ncol(df)
  for (c in 1:ncols) {
    na.rows <- which(is.na(df[,c]))
    row.name <- names(d.train)[c]
    print(paste(row.name,' has ' , length(na.rows), '\n' ))
  }
}

# Fill in NAs with mean for that column
ImputeNAs.vec <-function(vec) {
  vec[is.na(vec)] <- mean(vec, na.rm =T)
}
# Fill in NAs in a whole data frame
# with corresponding column mean
ImputNas.df <- function(df) {
  ncols <- ncol(df)
  for (c in 1:ncols) {
    ImputeNAs.vec(df[,c])
  }  
}

## This is for study only
par(mfrow=c(2,2))
SampleImage(13)
SampleImFtOverlaid(1,'righteye')
plot(d.train$left_eye_center_x, d.train$right_eye_center_x)
plot(abs(d.train$left_eye_center_x-d.train$right_eye_center_x))
plot(abs(d.train$left_eye_center_y-d.train$right_eye_center_y))
eye.dist.x <- d.train$left_eye_center_x - d.train$right_eye_center_x
eye.dist.y <- d.train$left_eye_center_y - d.train$right_eye_center_y
eye.dist <- sqrt(eye.dist.x * eye.dist.x + eye.dist.y + eye.dist.y )
plot(eye.dist)
hist(eye.dist)

leye.nose.dist.x <- d.train$left_eye_center_x - d.train$nose_tip_x
leye.nose.dist.y <- d.train$left_eye_center_y - d.train$nose_tip_y
leye.nose.dist   <- sqrt(leye.nose.dist.x * leye.nose.dist.x + leye.nose.dist.y + leye.nose.dist.y )
plot(leye.nose.dist.x)
hist(leye.nose.dist)
leye.nose.dist.y
leye.nose.dist.x[which(is.na(leye.nose.dist.x))]

summary(abs(d.train$left_eye_center_x-d.train$right_eye_center_x))
boxplot(abs(d.train$left_eye_center_x-d.train$right_eye_center_x))

NACheck(d.train)


#split train data
frac <- 0.7
nrows <- nrow(d.train)
split.row <- floor(nrows * frac)
d.train1 <- d.train[1:split.row,]
d.train2 <- d.train[split.row+1:nrows,]





#library(randomForest)

#rf <- randomForest(left_eye_center_x ~ left_eye_inner_corner_x+ )
