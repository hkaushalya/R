# This is to reduce the memory requirement when processing
# by removing unwanted data.
rm(list=ls())
print(getwd())
GenerateRdata <- function(years)
{ 
  print(cat('GenerateRdata: Skimming all data files:'))
  yr <- 2008
  for (yr in years) {
    filename <- paste0('data/', yr, '_n999.csv')
    savefile <- paste0('data/', yr, '_v2.Rda')
    print(filename)
    if (file.exists(filename)) {
      print(cat('Processing file:', filename, '\n'))
      airtimes <- read.csv(filename)
    } else {
      print(cat('File not found:', filename, '. Skipping ...\n'))
      next
    }
    
    drops <- c(
    "FlightNum","TailNum","ActualElapsedTime"
    #,"DayofMonth","DayOfWeek"
    ,"DepTime","ArrTime"
    ,"CRSDepTime","CRSArrTime","CRSElapsedTime"
    ,"AirTime"
    #,"Origin","Dest"
    ,"Distance"
    ,"TaxiIn","TaxiOut","Cancelled"
    ,"CancellationCode","Diverted","CarrierDelay","WeatherDelay"
    ,"NASDelay","SecurityDelay","LateAircraftDelay")

    airtimes <-airtimes[,!(names(airtimes) %in% drops)]
    #for date 'chr' year/month/date
    airtimes$Dates <-as.Date(paste0(airtimes$Year,'/',airtimes$Month,'/',airtimes$DayofMonth), "%Y/%m/%d")

    print(cat('Saving data to ', savefile,'\n'))
    save(airtimes, file=savefile)
    
    rm (filename, savefile, airtimes)
  }
}

#list of years corresponding to the data available
years <- c(2008)
GenerateRdata(years)
