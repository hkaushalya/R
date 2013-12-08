#Hierarchical Cluster Analysis
rm(list=ls())
attach(mtcars)
d<-dist(as.matrix(mtcars))   #find distance matrix
print(d)
hc_single<-hclust(d,method="single")                #apply hierarchical clustering
hc_ward<-hclust(d,method="ward")
hc_complete<-hclust(d,method="complete")
hc_average<-hclust(d,method="average")
hc_mcquitty<-hclust(d,method="mcquitty")
hc_median<-hclust(d,method="median")
hc_centroid<-hclust(d,method="centroid")
#open a new device to do multiplot
get (getOption("device"))()
par(mfrow=c(4,2))

plot(hc_single)                     # plot the dendogram
plot(hc_ward)
plot(hc_complete)
plot(hc_average)
plot(hc_mcquitty)
plot(hc_median)
plot(hc_centroid)
#print(hc)
detach(mtcars)