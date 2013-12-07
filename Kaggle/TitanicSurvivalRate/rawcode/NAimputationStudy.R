#testing 

random.imp <- function (a){
  missing <- is.na(a)
  print(missing)
  n.missing <- sum(missing)
  cat('n.missin = ', n.missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}


x <- rnorm(5)
y <- 1:5

ss <- data.frame(cbind(x,y))
ss[1,2] <- NA
ss[3,2] <- NA
ss[4,1] <- NA
ss
ss2<- random.imp(ss)
ss2