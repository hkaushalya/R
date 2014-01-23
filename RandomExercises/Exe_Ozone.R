library(lattice)
data(environmental)
head(environmental)
xyplot(ozone~radiation, data=environmental)
xyplot(ozone~radiation, data=environmental, main='Ozone vs. Radiation')
summary(environmental$temperature)
temp.cut<-equal.count(environmental$temperature, 4) #create four (roughly) equal ranges
temp.cut
temp.cut.names<-c('low','mid','warm','hot')
names(temp.cut)<-temp.cut.names
xyplot(ozone~radiation | temp.cut, data=environmental, main='Ozone vs. Radiation')
xyplot(ozone~radiation | temp.cut, data=environmental, main='Ozone vs. Radiation', layout=c(1,4))

#to add a linear regression model use 'panel' function
xyplot(ozone~radiation | temp.cut, data = environmental, pch=20,
       panel = function(x,y, ...) {
         panel.xyplot(x,y, ...)
         fit<-lm(y~x)
         panel.abline(fit,lwd=2)
       }
       )
#to add a smooth rendition
xyplot(ozone~radiation | temp.cut, data = environmental, pch=20,
       panel = function(x,y, ...) {
         panel.xyplot(x,y, ...)
         panel.loess(x,y)
       }, xlab = 'Solar Radition', ylab= 'Ozone (ppb)' , main = 'Ozone vs. Solar Radition'
)

wind.cut<-equal.count(environmental$wind,4)
wind.cut

xyplot(ozone~radiation | temp.cut * wind.cut, data = environmental, pch=20,
       panel = function(x,y, ...) {
         panel.xyplot(x,y, ...)
         panel.loess(x,y)
       }, xlab = 'Solar Radition', ylab= 'Ozone (ppb)' , main = 'Ozone vs. Solar Radition'
)

splom(~ environmental)
histogram (~temperature | wind.cut,data=environmental)
histogram (~ozone | wind.cut,data=environmental)
histogram (~ozone | wind.cut * temp.cut,data=environmental)