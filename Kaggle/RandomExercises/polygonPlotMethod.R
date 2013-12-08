#creating a plot method of polygon class
source('polygon.R')
setMethod("plot", "polygon",
          function(x, y, ...) {
                  plot(x@x, x@y, type = "n", ...)
                  xp <- c(x@x, x@x[1])
                  yp <- c(x@y, x@y[1])
                  lines(xp, yp)
          })


#create an object polygon and try the code out
p <- new("polygon", x = c(1,2,3,4), y = c(1,2,3,1))
#I kept getting this error wihthout this hack
#Error in plot.new() : figure margins too large
par(mar = rep(2, 4))
plot(p)