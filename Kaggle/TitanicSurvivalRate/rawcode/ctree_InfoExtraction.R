# I need to extract the probabilities used to construct the barplots
# displayed as part of the graph produced by plot("ctree").
library(party)
iris.ct <- ctree(Species ~ . , data = iris)
plot(iris.ct)

#You can compute the information from
where(iris.ct)        ## terminal node ids
treeresponse(iris.ct) ## associated predicted probabilities

#So, you could do
tapply(treeresponse(iris.ct), where(iris.ct), unique)
