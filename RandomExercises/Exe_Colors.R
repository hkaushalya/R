#colorRamp
pal <- colorRamp(c('Red','blue'))
pal(0)  #[Red][Green][Blue] this the output order u see
pal(1)
pal(0.5)

#ex. 2
pal(seq(0, 1, len = 10))


#colorRampPalette

pal <- colorRampPalette(c('red','yellow'))
pal(2)
pal(10)
pie(rep(1:12))

#a test code
pal <- colorRampPalette(c('red','yellow', 'blue'))
x <- rnorm(100)
plot(x, col=pal(10))

#using RColorBrewer

library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col=pal(20))

#The smoothScatter function
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)


#scatterplot with transparency
plot(x, y, pch=19)
plot(x, y, col = rgb(0,0,0,0.1), pch=19)

