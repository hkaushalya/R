rm(list=ls())
#preprocessor of FNAL Thesis data

library(gdata)  # use 'trim' to remove white spaces in strings


raw.data <- readLines('data/cdf_all_raw.txt')
head(raw.data, n=10)

#raw format
#Year (4digits)
#Author
#Institution
#Advisor
#Graduation Date
#Title

author     <- NULL
advisor    <- NULL
grad.month <- NULL   #need to strip out the month
grad.year  <- NULL   #need to strip out the month
title      <- NULL   # character string
experiment  <- NULL   # reading CDF data
institution <- NULL
foundheader <- FALSE


MonthYear <- function(vec) {
  nums1 <- gsub("[^[:digit:]]","", vec) # this strips out all the letters
  ltrs1 <- gsub("[[:digit:]]","", vec)  # this strips all the numbers
  #if (length(nums1)>1)
  #{
    print(nums1)
  #}
  nums1.clean <- nums1[nchar(nums1)!=0]  # remove 0 length
  ltrs1.clean <- ltrs1[nchar(ltrs1)!=0] 
  ltrs2.clean <- gsub('[[:punct:]]','',ltrs1.clean)  # removes ',' 
  nums <- as.numeric(trim(nums1.clean))
  ltrs <- trim(ltrs2.clean)
  #if (length(nums1)>1) print(nums)
  print(nums)
  return (list(yr=nums[length(nums)], mo=ltrs[1]))
}

i <- 1
r <- 0
indices <- grep("Author:", raw.data)
len <- length(indices)

for (i in 1:len) {
  print(cat('==============================\n'))
  r <- r + 1
  f_ind <- indices[i]
  if (i==len) { # if this is the last record
    l_ind <- length(raw.data)
  } else {
    l_ind <- indices[i+1]
    l_ind <- l_ind - 1
  }
  #print(cat('last =', l_ind,'\n'))
  data <- raw.data[f_ind:l_ind]
  #print(data)
  #if (r > 2) break
  
  exp <- "CDF"
  #print(paste(data, '->', class(data)))
  au <- grep('Author'         , data, value=TRUE)
  it <- grep('Institution'    , data, value=TRUE)
  ad <- grep('Advisor'        , data, value=TRUE)
  gd <- grep('Graduation Date', data, value=TRUE)
  ti <- grep('Title'          , data, value=TRUE)
  
  
  
  if (length(au) != 0) {
    au<-trim(sub('Author:','',au))
    #cat('au->',au,'\n')
  } else { au <- NA}
  if (length(it) != 0) {
    it<-trim(sub('Institution:','',it))
    #cat('it->',it,'\n')
  } else { au <- NA }

  if (length(ad) != 0) {
    ad<-trim(sub('Advisor:','',ad))
    #cat('ad->',ad,'\n')
  } else {
    ad <- NA
    #cat ('length of ad -> ', nchar(ad), '\n')
  }
  
  mo <- NA
  yr <- NA
  if (length(gd)!=0) {
    gd<-sub('Graduation Date:','',gd)
    dd <- MonthYear(gd)
    mo <- ifelse (nchar(dd$mo) !=0, dd$mo, NA)
    yr <- ifelse (nchar(dd$yr) !=0, dd$yr, NA)
    #print(cat('mo/yr =', dd$mo, "/", dd$yr,"\n") )
      
  } else {gd <- NA}
  
  if (length(ti)!=0) {
    ti<-trim(sub('Title:','',ti))
    #cat('ti->',ti,'\n')
  } else { ti <- NA}
  
  
  
  author[r]      <- as.character(au)
  institution[r] <- as.character(it)
  advisor[r]     <- ifelse(nchar(ad)>0,ad,NA)
  grad.month[r]  <- as.character(mo)
  grad.year[r]   <- as.numeric(yr)
  title[r]       <- as.character(ti)
  experiment[r]  <- as.character(exp)
  
  rm(data, au, it, ad, gd, ti, exp, mo, yr)
}

#df <- data.frame(rbind(grad.year, grad.year, experiment, institution, advisor, author, title))
df <- data.frame(cbind(grad.year, grad.month, experiment, institution, author, title))
head(df)
write.csv(df, 'data/preprocess1.csv')