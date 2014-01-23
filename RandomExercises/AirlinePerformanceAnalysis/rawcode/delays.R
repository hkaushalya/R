#study delay times 
#rm(list=ls())

setwd('~/Documents/Personal/Samantha/LearningMaterials/IntroToR/RandomExercises/AirlinePerformanceAnalysis')

#========================================
# This function produces the mean 
# arrival/departure times by month
# for the given years
#========================================
delayByMonth<-function(years) {
  
  for (yr in years) {
    print(yr)
    datafile <- paste0('data/',yr,'.Rda')
    load(datafile)
    
    #library(psych)
    #str(airtimes)
    #summary(airtimes)
    #describeBy(airtimes,airtimes$Month)
    
    #system.time(meanArrDelByMonth <- aggregate(airtimes$DepDelay, by=list(airtimes$Month), FUN=mean, na.rm=T))
    #meanDepDelByMonth <- aggregate(airtimes$ArrDelay, by=list(airtimes$Month), FUN=mean, na.rm=T)
    #This will not use multiple cores, but will be extremely fast and memory efficient.
    library(data.table)
    DT <- data.table(airtimes)
    rm(airtimes)
    setkey(DT, Month)
    meanDepDelByMonth<-DT[, list(DepDelay = mean(DepDelay, na.rm=TRUE)), by = Month]
    meanArrDelByMonth<-DT[, list(ArrDelay = mean(ArrDelay, na.rm=TRUE)), by = Month]
    
    col.arr <- 'deepskyblue2'
    col.dep <- 'tomato'
    pch.all <- 16
    lwd.all <- 2
    
    filename <- paste0('figures/delayByMonthFor_',yr,'.pdf')
    pdf(filename)
    plot(meanArrDelByMonth,type='p', col=col.arr, 
         main=paste0('Mean Arrival/Departure Delay by Month for the Year ', yr), 
         xlab='Month', ylab='Mean (Minutes)', 
         ylim=c(-2,20),pch=pch.all)
    lines(meanArrDelByMonth, col=col.arr ,lwd=lwd.all)
    lines(meanDepDelByMonth, type='p', col=col.dep,pch=pch.all)
    lines(meanDepDelByMonth, col=col.dep, lwd=lwd.all)
    legend("bottomleft", legend=list("Departure","Arrival"), 
           pch = pch.all, col=c(col.dep, col.arr),
           border="white", lwd=lwd.all,
           box.lwd = 0)
    lines(x=1:12,y=rep(mean(meanArrDelByMonth$ArrDelay),12),col=col.arr, lwd=lwd.all)
    lines(x=1:12,y=rep(mean(meanDepDelByMonth$DepDelay),12),col=col.dep, lwd=lwd.all)
    
    dev.off()
    rm(list=ls())
  }
}


#=============================================
# This function overlays the mean 
# arrival or departure times by month
# for all years
#=============================================
delayByMonthOverylaid<-function(years) {
  
  Month <- c(1:12)
  #Month <- month.name
  df.arr <- data.frame(Month)
  df.dep <- data.frame(Month)
  
  for (yr in years) {
    i <- 1
    print(yr)
    datafile <- paste0('data/',yr,'.Rda')
    load(datafile)
    
    library(data.table)
    DT <- data.table(airtimes)
    rm(airtimes)
    setkey(DT, Month)
    meanDepDelByMonth<-DT[, list(DepDelay = mean(DepDelay, na.rm=TRUE)), by = Month]
    meanArrDelByMonth<-DT[, list(ArrDelay = mean(ArrDelay, na.rm=TRUE)), by = Month]

    arr.old.names <- names(meanArrDelByMonth)
    dep.old.names <- names(meanDepDelByMonth)
    #arr.new.names <- c('Month',paste0('DepDelay.',yr), paste0('ArrDelay.', yr))
    arr.new.names <- c('Month',paste0('yr',yr))
    dep.new.names <- c('Month',paste0('yr',yr))
    setnames(meanArrDelByMonth, arr.old.names, arr.new.names)
    setnames(meanDepDelByMonth, dep.old.names, dep.new.names)
    
    df.arr <- merge(df.arr, meanArrDelByMonth, by='Month')
    df.dep <- merge(df.dep, meanDepDelByMonth, by='Month')
    rm(meanArrDelByMonth, meanDepDelByMonth)
    rm(DT, arr.old.names, dep.old.names, arr.new.names, dep.new.names)
  }
  
  df.arr
  df.dep
  
  col.arr <- 2
  col.dep <- 'tomato'
  pch.all <- 16
  lwd.all <- 2
  
  filename <- paste0('figures/ArrivalDelayByMonth_all.pdf')
  pdf(filename)
  i <- 2
  leg.text <- list()
  for (yr in years) {
    if (i==2) {
      plot(x=df.arr$Month, y=df.arr[,i], type='l', col=i, 
           main='Mean Arrival Delay by Month 1988-2008', 
           xlab='Month', ylab='Mean (Minutes)', 
           ylim=c(-2,30),pch=pch.all)
    } else {
      lines(df.arr[,i], col=i, lwd=lwd.all)
    }
    i <- i + 1
  }
 # legend("bottomleft", legend=list("Departure","Arrival"), 
#         pch=pch.all, col=i,
#         border="white", lwd=lwd.all,
#         box.lwd = 0)
  
  dev.off()
  rm(list=ls())
}

#=============================================
########################################
# Calculate mean delay for a given airport 
# arrival or departure times by month
# for all years
########################################
delayByOriginAirport<-function(years, airport) {
  
  Origin <- c(1:12)
  #Origin <- Origin.name
  df.arr <- data.frame(Origin)
  df.dep <- data.frame(Origin)
  yr <- 2008
  for (yr in years) {
    i <- 1
    print(yr)
    datafile <- paste0('data/',yr,'.Rda')
    load(datafile)
    
    library(data.table)
    DT <- data.table(airtimes)
    rm(airtimes)
    setkey(DT,Origin) 
    meanDepDelByOrigin<-DT[, list(DepDelay = mean(DepDelay, na.rm=TRUE)), by = Origin]
    meanArrDelByOrigin<-DT[, list(ArrDelay = mean(ArrDelay, na.rm=TRUE)), by = Origin]
    
    arr.old.names <- names(meanArrDelByOrigin)
    dep.old.names <- names(meanDepDelByOrigin)
    #arr.new.names <- c('Origin',paste0('DepDelay.',yr), paste0('ArrDelay.', yr))
    arr.new.names <- c('Origin',paste0('yr',yr))
    dep.new.names <- c('Origin',paste0('yr',yr))
    setnames(meanArrDelByOrigin, arr.old.names, arr.new.names)
    setnames(meanDepDelByOrigin, dep.old.names, dep.new.names)
    
    df.arr <- merge(df.arr, meanArrDelByOrigin, by='Origin')
    df.dep <- merge(df.dep, meanDepDelByOrigin, by='Origin')
    rm(meanArrDelByOrigin, meanDepDelByOrigin)
    rm(DT, arr.old.names, dep.old.names, arr.new.names, dep.new.names)
  }
  
  df.arr
  df.dep
  
  col.arr <- 2
  col.dep <- 'tomato'
  pch.all <- 16
  lwd.all <- 2
  
  filename <- paste0('figures/ArrivalDelayByOrigin_all.pdf')
  pdf(filename)
  i <- 2
  leg.text <- list()
  for (yr in years) {
    if (i==2) {
      plot(x=df.arr$Origin, y=df.arr[,i], type='l', col=i, 
           main='Mean Arrival Delay by Origin 1988-2008', 
           xlab='Origin', ylab='Mean (Minutes)', 
           ylim=c(-2,30),pch=pch.all)
    } else {
      lines(df.arr[,i], col=i, lwd=lwd.all)
    }
    i <- i + 1
  }
  # legend("bottomleft", legend=list("Departure","Arrival"), 
  #         pch=pch.all, col=i,
  #         border="white", lwd=lwd.all,
  #         box.lwd = 0)
  
  dev.off()
  rm(list=ls())
}


#x <- c(1:10)
#p <- plot(x,type="n")
#q <- lines(c(10:20))

#delayByMonth(c(1987:2008))
delayByMonthOverylaid(c(1988:2008))