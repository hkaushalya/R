# This is to reduce the memory requirement when processing
# by removing unwanted data.
rm(list=ls())

GenerateRdata <- function(years)
{ 
  print(cat('GenerateRdata: Skimming all data files:'))
  for (yr in years) {
    filename <- paste0('data/',yr,'.csv')
    savefile <- paste0('data/',yr,'.Rda')
    print(filename)
    if (file.exists(filename)) {
      print(cat('Processing file:', filename, '\n'))
      airtimes <- read.csv(filename)
    } else {
      print(cat('File not found:', filename, '. Skipping ...\n'))
      next
    }
    
    airtimes$FlightNum <- NULL
    airtimes$TailNum <- NULL
    airtimes$ActualElapsedTime <- NULL

    print(cat('Saving data to ', savefile,'\n'))
    save(airtimes, file=savefile)
    
    rm (filename, savefile, airtimes)
  }
}

#list of years corresponding to the data available
years <- c(2008)
GenerateRdata(years)
