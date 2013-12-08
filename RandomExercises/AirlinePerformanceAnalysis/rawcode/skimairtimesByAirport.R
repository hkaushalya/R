# This will 1st Skim data further by airport code

rm(list=ls())

GenerateRdataByAirport <- function(origincode)
{ 
  print(cat('GenerateRdata: Skimming all data files:'))
  for (code in origincode) {
    filename <- paste0('data/2008.Rda')
    print(filename)
    if (file.exists(filename)) {
      print(cat('Processing file:', filename, '\n'))
      load(filename)
    } else {
      print(cat('File not found:', filename, '. Skipping ...\n'))
      next
    }
    
    airtimes <- subset(airtimes, Origin==code | Dest==code)
    savefile <- paste0('data/2008_',code,'.Rda')  
    print(cat('Saving data to ', savefile,'\n'))
    save(airtimes, file=savefile)
    
    rm (filename, savefile, airtimes)
  }
}

#list of years corresponding to the data available
airportcode <- c('ORD')
GenerateRdataByAirport(airportcode)
