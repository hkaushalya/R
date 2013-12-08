#Time vs Distance analysis
rm(list=ls())
library(lattice)
library(DAAG)
#str(worldRecords)

pltA<-xyplot(Time~Distance, 
             groups=roadORtrack,
             data=worldRecords,
             auto.key=list(column=2),
             xlab="Distance (km)",
             ylab="Time (min)")
print(pltA)

##Reduce text and point size by a factor of 0.8
print(update(pltA,cex=0.8,scales=list(tck=0.8)))

logplt<-xyplot(Time ~ Distance,
               type=c("p","r"),
               scales=list(log="e"),
               data=worldRecords)
print(logplt)
worldRecs.lm <- lm(log(Time) ~ log(Distance), data=worldRecords)
print(worldRecs.lm)

#check the fit by looking at the residuals
yexpr<-expression("Residuals (log("[e]*"scale)")
resplot<-xyplot(resid(worldRecs.lm) ~ log(Distance),
                groups=roadORtrack,
                ylab=yexpr,
                data=worldRecords)
print(resplot)