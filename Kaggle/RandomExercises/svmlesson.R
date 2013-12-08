#SVM lesson from svmbasic_notes_PracticalLesson.pdf
rm(list=ls())

# These 3 functions are the solution to the problems
# from the Author's site:
# http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/svmbasic/general.R

##==================================================
plotlinearsvm2D=function(svp,xtrain)
  ## Pretty plot a linear SVM with decision boundary ##
  ## xtrain should be a 2-dimensional data.
{
  # Define the range of the plot
  # First column is plotted vertically
  yr <- c(min(xtrain[,1]), max(xtrain[,1]))
  # Second column is plotted horizontally
  xr <- c(min(xtrain[,2]), max(xtrain[,2]))
  
  # Plot the points of xtrain with different signs for positive/negative and SV/non SV
  plot(xr,yr,type='n')
  ymat <- ymatrix(svp)
  points(xtrain[-SVindex(svp),2], xtrain[-SVindex(svp),1], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
  points(xtrain[SVindex(svp),2], xtrain[SVindex(svp),1], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))
  
  # Extract w and b from the model  
  w <- colSums(coef(svp)[[1]] * xtrain[SVindex(svp),])
  b <- b(svp)
  
  # Draw the lines 
  abline(b/w[1],-w[2]/w[1])
  abline((b+1)/w[1],-w[2]/w[1],lty=2)
  abline((b-1)/w[1],-w[2]/w[1],lty=2)
}
# ==================================================
cv.folds <- function(n,nfolds=3)
  ## Randomly split the n samples into folds
  ## Returns a list of nfolds lists of indices, each corresponding to a fold
{
  return(split(sample(n),rep(1:nfolds,length=n)))
}

cvpred.ksvm <- function(x,y,folds=3,predtype="response",...)
  ## Return a vector of predictions by cross-validation
  ## 'predtype' should be one of response (by default), decision or probabilities, depending the prediction we want (SVM label, score or probability, see predict.ksvm())
  ## Additional parameters are passed to ksvm() to train the SVM
{
  n <- length(y)
  ypred <- numeric(n)
  s <- cv.folds(n,folds)
  for (i in seq(folds)) {
    m <- ksvm(x[-s[[i]],],y[-s[[i]]],...)
    ypred[s[[i]]] <- predict(m,x[s[[i]],],type=predtype)
  }
  invisible(ypred)
}


cvpred.precomp.ksvm <- function(K,y,folds=3,predtype="decision",...)
  ## Cross-validatin predictions for precomputed kernels
{
  n <- length(y)
  ypred <- numeric(n)
  s <- cv.folds(n,folds)
  for (i in seq(folds)) {
    m <- ksvm(as.kernelMatrix(K[-s[[i]],-s[[i]]]),y[-s[[i]]],...)
    ktest <- as.kernelMatrix(K[s[[i]],-s[[i]]][,SVindex(m),drop=F])
    ypred[s[[i]]] <- predict(m,ktest,type=predtype)
  }
  invisible(ypred)
}







par(mfrow=c(1,1))
n <- 150 # numberof data points
p <- 2   # dimension

sigma   <- 1   # variance of the distributions
meanpos <- 0 # center of the distribution of positive samples
meanneg <- 3 # center of the distribution of negative samples

npos <- round(n/2)  # number of positive examples
nneg <- n - npos    # number of negative examples

#Generate the positive and negative examples
xpos <- matrix( rnorm(npos*p, mean=meanpos, sd=sigma), npos, p)
xneg <- matrix( rnorm(nneg*p, mean=meanneg, sd=sigma), npos, p)
x <- rbind(xpos, xneg)

plot(x, col=ifelse(x>0,2,3))

# Generate the labels (+1 and -1)
y <- matrix(c(rep(1,npos), rep(-1,nneg)))

# Visualize
plot (x, col=ifelse(y>0,1,2))
legend("topleft",c('Positive', 'Negative'), col=seq(2),pch=1,text.col=seq(2))

# Split the data for training and testing

ntrain <- round(n * 0.8)  # number of training examples
tindex <- sample(n, ntrain)
xtrain <- x[ tindex, ]
xtest  <- x[-tindex, ]
ytrain <- y[ tindex, ]
ytest  <- y[-tindex, ]

istrain <- rep(0,n)
istrain[tindex] <- 1

# Visualize
plot (x, col=ifelse(y>0,1,2), pch=ifelse(istrain==1, 1, 2))
legend("topleft", 
       c('Positive Train', 'Positive Test', 'Negative Train', 'Negative Test'), 
       pch=c(1,2,1,2), col=c(1,1,2,2), text.col=c(1,1,2,2)
       ) 

# Train a SVM using C=100 using kernlab package -------------
library(kernlab)

# train the SVM ---------------------------------------------
svp  <- ksvm(xtrain, ytrain, type="C-svc", kernel='vanilladot', C=1000, scaled=c())
svp1 <- ksvm(xtrain, ytrain, type="C-svc", kernel='splinedot' , C=20  , scaled=c())


# General summary
svp

# Attributes that you can access
attributes(svp)
str(svp)
# For example, the support vectors 
alpha(svp) 
alphaindex(svp) 
b(svp)
# Use the built-in function to pretty-plot the classifier 
plot(svp,data=xtrain)
quartz()
plot(svp1,data=xtrain)

# Predict with SVM --------------------------------------------
# Predict labels on test 
ypred <- predict(svp,xtest) 
table(ytest,ypred)
# Compute accuracy 
sum(ypred==ytest)/length(ytest)
# Compute at the prediction scores 
ypredscore <- predict(svp,xtest,type="decision")


# Check that the predicted labels are the signs of the scores 
table(ypredscore > 0,ypred)
# Package to compute ROC curve, precision-recall etc... 
library(ROCR)
quartz()
par(mfrow=c(2,2))
pred <- prediction(ypredscore,ytest)
# Plot ROC curve 
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf)
# Plot precision/recall curve 
perf <- performance(pred, measure = "prec", x.measure = "rec") 
plot(perf)
# Plot accuracy as function of threshold 
perf <- performance(pred, measure = "acc") 
plot(perf)




plotlinearsvm2D(svp, xtrain)