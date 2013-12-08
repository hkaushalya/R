#Climb for Hill Data analysis

rm(list=ls())
library(DAAG)
attach(nihills)
#A data frame with 23 observations on the following 4 variables.
#dist  = distances in miles
#climb = amount of climb in feet
#time  = record time in hours for males
#timef = record time in hours for females
print(nihills)
plot(nihills)


detach(nihills)