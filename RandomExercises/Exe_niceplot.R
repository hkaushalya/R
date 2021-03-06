chests <- c('gold', 'silver', 'gems', 'gold', 'gems')
types <- factor(chests)
weights <- c(300, 200, 100, 250, 150)
prices <- c(9000, 5000, 12000, 7500, 18000)
plot(weights, prices,pch=as.integer(types))
legend("topright",c("gems","gold","silver"), pch=1:length(levels(types)))

list