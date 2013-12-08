NumOfCountries <- function(vec) {
  return(length(unique(vec))) 
}

GradByCountry<-function(inst, recs) {
  countries <- unique(inst)
  counts    <- c(rep(0,length(countries)))
  
  for (i in 1:length(recs)) {
    srchstr <- recs[i]
    for (j in 1:length(countries)) {
      ind <- grep(countries[j], srchstr)
      print(ind)
      if (nchar(ind) ==0) next 
      if (ind>=1) break  #first hit
    }

    if (ind>=1 & ind<=length(counts)) counts[ind] <- counts[ind]+1
    print(srchstr,'->', countries[ind])
  }
  
  print(cbind(countries,counts))
}


df<-read.csv('data/preprocess1.csv', header=T)
hist(df$grad.year, xlab='Year', ylab='Number of Graduates', 
     main='Number of Graduated from CDF with a MS or PhD',col='blue')
#str(df)

df.inst <- read.csv('data/CDFInstitutions.csv',skip=2,header=T)
df.inst
print(NumOfCountries(df.inst$country))
library(ggplot2)
plot(df$)
library(lattice)
plot(df$grad.year, df$grad.month,col=rgb(100,0,0,,maxColorValue=255),pch=5)
#image(df$grad.year, df$grad.month,col=rgb(100,0,0,,maxColorValue=255),pch=5)

library(scatterplot3d)
#include number of grads per month
scatterplot3d(df$grad.year, df$grad.month,pch=16, highlight.3d=TRUE,type="h")
grad.by.country <- GradByCountry(df.inst$country, df$institution)
