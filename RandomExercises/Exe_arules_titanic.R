#Association Rules
# http://www.rdatamining.com/examples/association-rules
rm(list=ls())
load('data/titanic.raw.rdata')
str(titanic.raw)

#Association Rule Mining
library(arules)
# fine association riles with default settings
rules <- apriori(titanic.raw)
inspect(rules)

# We then set rhs=c("Survived=No", "Survived=Yes") in appearance to make
# sure that only "Survived=No" and "Survived=Yes" will appear in the 
# rhs of rules.
rules <- apriori(titanic.raw,
             parameter = list(minlen=2, supp=0.005, conf=0.8),
             appearance = list(rhs=c('Survived=No', 'Survived=Yes'),
             default='lhs'),
             control = list(verbose=F)
          )

rules.sorted <- sort(rules, by='lift')
inspect(rules.sorted)


# Pruning Redundant Rules
# In the above result, rule 2 provides no extra knowledge in 
# addition to rule 1, since rules 1 tells us that all 2nd-class 
# children survived. Generally speaking, when a rule 
# (such as rule 2) is a super rule of another rule 
# (such as rule 1) and the former has the same or a lower lift, 
# the former rule (rule 2) is considered to be redundant. Below we
# prune redundant rules.

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
str(subset.matrix)
subset.matrix[lower.tri(subset.matrix, diag=T)] <-NA
subset.matrix
redundant <- colSums(subset.matrix, na.rm=T) >=1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


#Viasualizing Association Rules
library(arulesVis)
plot(rules)
plot(rules, method='graph', control=list(type='items'))

plot(rules, method='paracoord', control=list(reorder=TRUE))