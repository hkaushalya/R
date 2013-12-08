#Linear Mixed Models

library(MASS)
data(oats)
names(oats)   <- c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot <- oats$variety
oats$subplot  <- oats$nitrogen

summary(oats)
head(oats)

xyplot(yield ~ nitrogen | variety, data=oats, 
       panel=function(x,y,...) {
         panel.xyplot(x,y,...)
         line <- lm(y~x)
         panel.abline(line)
       }
)

xyplot(yield ~ variety | nitrogen, data=oats, 
       panel=function(x,y,...) {
         panel.xyplot(x,y,...)
         line <- lm(y~x)
         panel.abline(line)
       }
)

library(nlme)
m1.nlme = lme(yield ~ variety*nitrogen, random = ~ 1|block/mainplot, data = oats)
summary(m1.nlme)
anova(m1.nlme)


library(lme4)
m1.lme4 = lmer(yield ~ variety*nitrogen + (1|block/mainplot), data = oats)

summary(m1.lme4)
anova(m1.lme4)

# Not avail for 3.1
library(asreml)
m1.asreml = asreml(yield ~ variety*nitrogen, random = ~ block/mainplot, data = oats)
summary(m1.asreml)$varcomp

wald(m1.asreml, denDF = &quot;algebraic&quot;)
