#use of lattice graphics package

library(lattice)
library(help=lattice)
package ? lattice
#example(lattice)

data(environmental)
summary(environmental)
boxplot(environmental) #from base graphic system

# lets use lattice
# Here the relationship is written as: y vs. x
# The dataset needs to specified bcos the variables, ozone amd radiation
# are not defined and will be lookup in the 'environemtal' dataframe
xyplot(ozone ~ radiation, data = environmental, main = 'Ozone vs. Radiation')

# Lets try to plot the same in different temperature ranges.
# Since Temp is contiinoues we need to break it in to several ranges.
temp.cut <- equal.count(environmental$temperature, 4)
temp.cut
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table = T, pch = 20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         fit<-lm(y ~ x)
         panel.abline(fit, lmd=2, lwd=2)
       }
       )

# Difference rendition using a smooth line
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table = T, pch = 20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x,y)
       } , xlab = 'Solar Radiation', ylab = 'Ozone (ppb)')


# include the variable wind
wind.cut <- equal.count(environmental$wind, 4)
xyplot(ozone ~ radiation | temp.cut * wind.cut, data = environmental, as.table = T, pch = 20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x,y)
       } , xlab = 'Solar Radiation', ylab = 'Ozone (ppb)')


# Use of splom varaibel
splom ( ~ environmental)  # can give the whole dataset as argument

# Use of histogram function
histogram( ~ ozone | temp.cut * wind.cut, data = environmental)