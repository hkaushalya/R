data.dir   <- './data/'
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')
print(train.file)
d.train <- read.csv(train.file, stringsAsFactors=F)

im.train      <- d.train$Image
d.train$Image <- NULL

#as.integer(unlist(strsplit(im.train[1], " ")))
# im.train[1]

# to speed up conversion of string->int 
# process using multicores
# install.packages('doMC')

# Load and register the library
library(doMC)
registerDoMC()

# Implement Parallelization
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

d.test$Image <- NULL

# save these as R data files so we do not need to reprocess
save(d.train, im.train, d.test, im.test, file='data.Rd')


