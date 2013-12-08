#Goal: Toss a coing N times and compute the running proportion of heads
N = 500 

#set.seed(47405)
flipsequence = sample(x=c(0,1), prob=c(0.3,0.7), size = N, replace=TRUE)

#compute the running proportion of heads:

r = cumsum(flipsequence)
n = 1:N
runprop = r/n

plot(n, runprop, type="o", log="x",
		xlim=c(1,N), ylim=c(0.0,1.0), cex.axis=1.5,
		xlab="Flip Number", ylab="Proportion Heads", cex.lab=1.5,
		main="Running Proportions of Heads", cex.main=1.5)

lines( c(1,N), c(.5,.5), lty=3)

flipsetters = paste( c("T","H")[ flipsequence[ 1:10 ] + 1 ], collapse="" )
displaystring = paste( "Flip Sequence = ", flipsetters, "...", sep="" )
text( N, .3, paste("End Proportion =", runprop[N]) , adj=c(1,0) , cex=1.3 )
dev.copy2pdf( file = "RunningProportion.pdf")