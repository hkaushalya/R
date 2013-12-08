#http://www.rdatamining.com/examples/association-rules
#Association Rules
rm(list=ls())
setwd("/Users/samantha/Documents/Personal/Samantha/LearningMaterials/IntroToR/RandDataMining_ExamplesAndCaseStudies/data/")
titanicraw<-load("titanic.raw.rdata")
str(titanic.raw)

#association rule mining
library(arules)
# find association rules with default settings
rules<-apriori(titanic.raw)
inspect(rules)
# rules with rhs containing "Survived" only
rules<-apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"),control = list(verbose=F))
rules.sorted<-sort(rules,by="lift")
inspect(rules.sorted)

#find redundant rules
subset.matrix<-is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)]<-NA
redundant<-colSums(subset.matrix, na.rm=T)>=1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

