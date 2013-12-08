#CDC Population data
rm(list=ls())
library(maps)
library(ggplot2)
#library(animation)

getPlot <- function(year) {
  print(class(year))
  all_states <- map_data("state")
  #head(all_states)
  names <- names(all_states)
  names[5] <- 'State'    # to match with what I have is data files
  names(all_states) <- names
  
  filename <- sprintf('CDCPopulationData/StatePopulations%i.csv',year)
  print(filename)
  if (file.exists(filename)) {
  #pop.data <- read.csv('CDCPopulationData/StatePopulations2000.csv')
    pop.data <- read.csv(filename) 
  } else {
    msg <- sprintf('A data file for the year %i is not found!', year)
    stop(msg)
  }
  
  #head(pop.data)
  #str(pop.data)
  
  pop.data$State <- tolower(pop.data$State)
  
  # merge the data with spatial data
  com.data <- merge(pop.data, all_states, by='State')
  #for some reason 2004 data seems to get some NAs after merge!!!!
  # need to investigate. 
  
  #print(head(com.data))
  
  maintitle <- sprintf('US Population by State in %i' , year)
  legtitle <- sprintf('Population')
  cat ('trying ot make the plot\n')

  savename <- sprintf('popdata_%i.png', year, '\n') 
  cat('printname: ', savename)
  #dev.copy(png, printname)
  #dev.off()
  #jpeg(savename)
  #dev.off()
  #jpeg(file=savename, width=40, height=20)
   
    p <- ggplot()
  # It seems have the data.frame inside 'aes' is very bad!
  # results in this error 'Error in eval(expr, envir, enclos) : object 'com.data' not found' 
  # and does not save the file
  #    p <- p + geom_polygon(data=com.data, aes(x=long, y=lat, group = group, fill=com.data$Total),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
    p <- p + geom_polygon(data=com.data, aes(x=long, y=lat, group = group, fill=Total ,xlab=''),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
    P1 <- p + theme_bw()  + labs(fill = legtitle 
                                 ,title = "Test ttitle", x="", y="")
    P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
    #plot(p)
    #print(p)
    ggsave(filename=savename, plot=p, width = 10, height = 8)
    #dev.off()
  #rm (year, maintitle, legtitle, com.data, pop.data, all_states)
}


#saveGIF(plot(years), movie.name = "animation.gif", img.name = "Rplot", convert = "convert", 
#        cmd.fun = system, clean = TRUE, ...)

generateGIF <- function(years) {
  
#  saveGIF({
    for (y in years) {
      getPlot(y)
    }
#  })

#  max <- length(years)
#  oopt = ani.options(interval = 0.2, nmax = max)  
#  for (i in 1:ani.options("nmax")) {
#    getPlot(years[i])
#    ani.pause()  ## pause for a while ('interval')
#  }
  
}

years <- c(2000, 2004,2008)
#years <- c(2000)
generateGIF(years)