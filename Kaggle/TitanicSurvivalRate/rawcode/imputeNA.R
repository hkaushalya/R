# =============================================================
# This is small function to fill in missing data (NA)
#   Inputs:     x: a vector of values
#            fill: a string to select how the missing
#                  data to be selected
#   Returns: x_compelete: a vector with all NAs are replaced
# =============================================================
imputeNA <- function(x, fill="mean", debug=FALSE) {
  if (debug == T) {
    cat ('impute.NA: Filling NA using method: ', fill ,'\n')
    cat ('input len = ', length(x), '\n')
  }
  if (fill=="mean")
  {
    x_complete <- ifelse(is.na(x), mean(x, na.rm=TRUE), x)    
  } else if (fill=="median")
  {
    x_complete <- ifelse(is.na(x), median(x, na.rm=TRUE), x)
  } else if (fill == "randomsample")
  {
    x_complete <- ifelse(is.na(x), 
                         sample(x[!is.na(x)], size=1,replace=TRUE), 
                         x)
  } else {
    stop('imputeNA:: Undefined fill method called!')
  }
  
  if (debug == T) {
    cat ('input len = ', length(x_complete), '\n')
  }
  return(x_complete)
}