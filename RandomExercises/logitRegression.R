#from this site: http://www.ats.ucla.edu/stat/r/dae/logit.htm

#Logistic regression, also called a logit model, is used to model dichotomous
# outcome variables. In the logit model the log odds of the outcome is 
# modeled as a linear combination of the predictor variables.


library(aod)
library(ggplot2)

# Example 2. A researcher is interested in how variables, such as GRE (Graduate 
# Record Exam scores), GPA (grade point average) and prestige of the 
# undergraduate institution, effect admission into graduate school. The response
# variable, admit/don't admit, is a binary variable.
# Description of the data
#   For our data analysis below, we are going to expand on Example 2 about
#   getting into graduate school. We have generated hypothetical data, which 
#   can be obtained from our website from within R. Note that R requires 
#   forward slashes (/) not back slashes (\) when specifying a file location
#   even if the file is on your hard drive.

rm(list=ls())

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

summary(mydata)
str(mydata)
#standard deviations
sapply(mydata, sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)


# logistic regression using 'Generalized Linear Model' (glm)
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
#str(mylogit)
summary(mylogit)
#plot(mylogit)
# Note that for logistic models, confidence intervals are based on the profiled
# log-likelihood function. We can also get CIs based on just the standard errors
# by using the default method.

## CIs using profiled log-likelihood
confint(mylogit)
confint(mylogit, 'gre')

## CIs using standard errors
confint.default(mylogit)

# We can test for an overall effect of rank using the wald.test function of the
# aod library. The order in which the coefficients are given in the table of
# coefficients is the same as the order of the terms in the model. This is
# important because the wald.test function refers to the coefficients by their
# order in the model. We use the wald.test function. b supplies the
# coefficients, while Sigma supplies the variance covariance matrix of the error
# terms, finally Terms tells R which terms in the model are to be tested, in
# this case, terms 4, 5, and 6, are the three terms for the levels of rank.

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)


# We can also test additional hypotheses about the differences in the 
# coefficients for the different levels of rank. Below we test that the 
# coefficient for rank=2 is equal to the coefficient for rank=3. The first 
# line of code below creates a vector l that defines the test we want to 
# perform. In this case, we want to test the difference (subtraction) of 
# the terms for rank=2 and rank=3 (i.e., the 4th and 5th terms in the model).
# To contrast these two terms, we multiply one of them by 1, and the other
# by -1. The other terms in the model are not involved in the test, so they
# are multiplied by 0. The second line of code below uses L=l to tell R that
# we wish to base the test on the vector l (rather than using the Terms option
# as we did above).

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)
# The chi-squared test statistic of 5.5 with 1 degree of freedom is 
# associated with a p-value of 0.019, indicating that the difference 
# between the coefficient for rank=2 and the coefficient for rank=3 
# is statistically significant.

# You can also exponentiate the coefficients and interpret them as odds-ratios. 
# R will do this computation for you. To get the exponentiated coefficients, you 
# tell R that you want to exponentiate (exp), and that the object you want to 
# exponentiate is called coefficients and it is part of mylogit (coef(mylogit)).
# We can use the same logic to get odds ratios and their confidence intervals, 
# by exponentiating the confidence intervals from before. To put it all in one
# table, we use cbind to bind the coefficients and confidence intervals
# column-wise.

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Now we can say that for a one unit increase in gpa, the odds of being 
# admitted to graduate school (versus not being admitted) increase by a 
# factor of 2.23. For more information on interpreting odds ratios see our 
# FAQ page How do I interpret odds ratios in logistic regression? . Note 
# that while R produces it, the odds ratio for the intercept is not generally 
# interpreted.

# You can also use predicted probabilities to help you understand the model. 
# Predicted probabilities can be computed for both categorical and continuous
# predictor variables. In order to create predicted probabilities we first
# need to create a new data frame with the values we want the independent 
# variables to take on to create our predictions.

# We will start by calculating the predicted probability of admission at each
# value of rank, holding gre and gpa at their means. First we create and view
# the data frame.

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
## view data frame
newdata1

# type of prediction is a predicted probability (type="response")
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response" )
#newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "terms" ) # note sure what this returns
newdata1

# In the above output we see that the predicted probability of being accepted
# into a graduate program is 0.52 for students from the highest prestige
# undergraduate institutions (rank=1), and 0.18 for students from the lowest
# ranked institutions (rank=4), holding gre and gpa at their means. We can do
# something very similar to create a table of predicted probabilities varying
# the value of gre and rank. We are going to plot these, so we will create 100
# values of gre between 200 and 800, at each value of rank (i.e., 1, 2, 3, and 4).

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

# The code to generate the predicted probabilities (the first line below) 
# is the same as before, except we are also going to ask for standard errors 
# so we can plot a confidence interval. We get the estimates on the link scale
# and back transform both the predicted values and confidence limits into 
# probabilities.

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
## view first few rows of final dataset
head(newdata2)

# It can also be helpful to use graphs of predicted probabilities to understand
# and/or present the model. We will use the ggplot2 package for graphing. 
# Below we make a plot with the predicted probabilities, and 95% confidence 
# intervals.
ggplot(newdata3, aes(x = gre, y = PredictedProb))
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL, 
            ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank), 
            size = 1)