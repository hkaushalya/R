# Bootstrapping
# http://www.statmethods.net/advstats/bootstrapping.html

# Bootstrapping a Single Statistic (k=1)  ====================
# The following example generates the bootstrapped 95% 
# confidence interval for R-squared in the linear regression
# of miles per gallon (mpg) on car weight (wt) and 
# displacement (disp). The data source is mtcars. The 
# bootstrapped confidence interval is based on 1000 replications.
# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=mtcars, statistic=rsq, 
                R=10, formula=mpg~wt+disp)

# view results
results 
str(results)
summary(results)
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")