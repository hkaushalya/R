set.seed(1234)
df <- data.frame(cond=factor(rep(c('A','B'), each=200)),
				rating = c(rnorm(200),rnorm(200, mean=0.8)) )
library(ggplot2)

# Histograms and desnity plots
qplot(df$rating, binwidth=0.5)

#with black outline and white fill
ggplot(df, aes(x=rating)) + geom_histogram(binwidth=0.5, colour='blue',fill='white')

#density 
ggplot(df, aes(x=rating)) + geom_density()

# histogram overlaid with kernel density curve
ggplot(df, aes(x=rating)) +
	geom_histogram(aes(y=..density..),
					binwidth=0.5,
					colour='black', fill='white') +
	geom_density(alpha=0.2, fill='red')
	
#overlaid histogram
ggplot(df, aes(x=rating, fill=cond)) + geom_histogram(binwidth=0.5, alpha=0.5, position='identity')
#InterLeaved
ggplot(df, aes(x=rating, fill=cond)) + geom_histogram(binwidth=0.5, alpha=0.5, position='dodge')
#Density plots
ggplot(df, aes(x=rating, colour=cond)) + geom_density()
#Density plots with semi-transparent fill
ggplot (df, aes(x=rating, fill=cond)) + geom_density(alpha=0.3)


#BoxPlots
# A basic box plot
ggplot(df, aes(x=cond, y=rating, fill=cond)) + geom_boxplot()

# Add a diamond at the mean, and make it larger
ggplot(df, aes(x=cond, y=rating, fill=cond)) + geom_boxplot() +
	stat_summary(fun.y=mean, geom='point', shape=6,size=4) +
	guides(fill=FALSE)

mal7@pon4
