# Plotting with mathematical annotation

# Using a computed value in the annotation
x <- rnorm(100)
y <- x + rnorm(100, sd = 0.5)
plot(x, y,
     xlab = substitute(bar(x) == k, list(k=mean(x),2)),
     ylab = substitute(bar(y) == k, list(k=mean(y)))
     )

# Or in a loop of plot
par(mfrow = c(2,2))
for (i in 1:4) {
  x <- rnorm(100)
  hist(x, main=substitute(theta==num, list(num=i)))  
}

