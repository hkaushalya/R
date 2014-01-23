#This is to further understand the the ctree functionality. 
# I am following the party package manual.
# These are some of the examples in there.
rm(list=ls())
library(party)
library(psych)
data("treepipit", package="coin")
str(treepipit)
describe(treepipit)

ct <- ctree(counts~., data=treepipit)
plot(ct)
plot(ct, terminal_panel = node_hist(ct, breaks=0:6-0.5, ymax=65,horizontal=FALSE, freq=TRUE))
lm.model <- lm(formula=counts~., data=treepipit)
alm <- anova(formula=counts~., data=treepipit)
summary(lm.model)


#Glucoma and laser scanning images
data("GlaucomaM", package="TH.data")
describeBy(GlaucomaM,group=GlaucomaM$Class)
str(GlaucomaM)
gt <- ctree(Class ~., data=GlaucomaM, controls=ctree_control(minsplit=50))
plot(gt)