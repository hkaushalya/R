#how to replace NA in columns
x <- c(1:10, NA)
x
Y <- sample(x, size=16, replace=T)
m <- matrix(Y, nrow=4 )
m
m[which(is.na(m))]
m[, 1:2][is.na(x[,1:2])] <- 0

for(col in c())
  
  
set.seed(1234)
x <- data.frame(a=sample(c(1,2,NA), 10, replace=T),
                b=sample(c(1,2,NA), 10, replace=T), 
                c=sample(c(1:5,NA), 10, replace=T))
x
x[1,][is.na(x[1,])] <- 0
x