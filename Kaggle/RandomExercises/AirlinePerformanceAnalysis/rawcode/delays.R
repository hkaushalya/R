#study delay times 
setwd('~/Documents/Personal/Samantha/LearningMaterials/IntroToR/RandomExercises/AirlinePerformanceAnalysis')
rm(list=ls())
load('data/2008.Rda')

airtimes$DayofMonth <-NULL
airtimes$DayOfWeek <-NULL
airtimes$DepTime <-NULL
airtimes$ArrTime <-NULL
airtimes$CRSDepTime <-NULL
airtimes$CRSArrTime <-NULL
airtimes$CRSElapsedTime <-NULL
airtimes$AirTime <-NULL
#airtimes$Origin <-NULL
airtimes$Dest <-NULL
airtimes$Distance <-NULL
airtimes$TaxiIn <-NULL
airtimes$TaxiOut <-NULL
airtimes$Cancelled <-NULL
airtimes$CancellationCode <-NULL
airtimes$Diverted <-NULL
airtimes$CarrierDelay <-NULL
airtimes$WeatherDelay <-NULL
airtimes$NASDelay <-NULL
airtimes$SecurityDelay <-NULL
airtimes$LateAircraftDelay <-NULL



library(psych)
library(ggplot2)
str(airtimes)
summary(airtimes)
describeBy(airtimes,airtimes$Month)


# Exploratory plots ==========================

# National Arrival delay  ====================
arrDelay <- airtimes$ArrDelay
depDelay <- airtimes$DepDelay
delay <- c(arrDelay, depDelay)
condition <- rep(c('Arrival','Departure'),each=length(depDelay))
df <- data.frame(delay,condition)
df$condition<- as.factor(df$condition)
df200 <- airtimes[1:500,]
str(df200)
#df2 <- subset(df, condition=='Departure' & abs(delay)<10)
#qplot(delay,data=df2)

ggplot(df200, aes(x=UniqueCarrier, y=DepDelay, fill=UniqueCarrier)) + 
  geom_boxplot()
#  geom_boxplot(aes(col=UniqueCarrier))

  

quartz()
ggplot(df, aes(x=delay, fill=condition)) +
  labs(title='National Arrival/Departure Delays') +
  labs(title=list(x='Delay (per 5 Minutes)',y='Frequency')) +
  geom_histogram(binwidth=5, alpha=.4, position="identity") +
  xlim(-45, 270)

dev.copy2pdf(file="figures/hist_delays_2008.pdf")
libray(plyr)

ggplot(airtimes, aes(x=Origin, y=DepDelay, fill=Origin)) + 
  geom_boxplot(width=0.6, position = position_dodge(width= 0.75), outlier.size=0.5
)
