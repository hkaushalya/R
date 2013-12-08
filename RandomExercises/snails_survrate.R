#studying the snails dataset 
# need to understand  the survival rate
#Q? The object of the exercise was to model the probability of
# survival in terms of the stimulus variables, and in particular
# to test for differences between species.

rm(list=ls())

SurvivalRate<-function(dt) {
  total<-sum(dt$N)
  total.deaths<-sum(dt$Deaths)
  total.survived = 0
  total.survived<-total-total.deaths
  surv.rate<-total.survived/total
  print("surv.rate=")
  print(surv.rate)
  head(dt,n=3)
  return(surv.rate)
}


library(MASS)
attach(snails)
expos1<-subset(snails, Exposure==1)
expos2<-subset(snails, Exposure==2)
expos3<-subset(snails, Exposure==3)
expos4<-subset(snails, Exposure==4)
#print(expos1)

expos1.total<-sum(expos1$N)
print('Total species exposed in week1=')
print(expos1.total)
expos1.survive<-subset(snails, Exposure==1 & Deaths==0)
expos2.survive<-subset(snails, Exposure==2 & Deaths==0)
expos3.survive<-subset(snails, Exposure==3 & Deaths==0)
expos4.survive<-subset(snails, Exposure==4 & Deaths==0)

expos1.surviveA<-subset(snails, Exposure==1 & Species=="A")
expos2.surviveA<-subset(snails, Exposure==2 & Species=="A")
expos3.surviveA<-subset(snails, Exposure==3 & Species=="A")
expos4.surviveA<-subset(snails, Exposure==4 & Species=="A")
#print(expos1.surviveA)
#print(expos2.surviveA)
#print(expos3.surviveA)
print(expos4.surviveA)

expos1.surviveB<-subset(snails, Exposure==1 & Species=='B')
expos2.surviveB<-subset(snails, Exposure==2 & Species=='B')
expos3.surviveB<-subset(snails, Exposure==3 & Species=='B')
expos4.surviveB<-subset(snails, Exposure==4 & Deaths==0 & Species=='B')


surv.rate<-rep(0,4)
surv.rate[1]<-SurvivalRate(expos1)
surv.rate[2]<-SurvivalRate(expos2)
surv.rate[3]<-SurvivalRate(expos3)
surv.rate[4]<-SurvivalRate(expos4)

surv.rateA<-rep(0,4)
surv.rateA[1]<-SurvivalRate(expos1.surviveA)
surv.rateA[2]<-SurvivalRate(expos2.surviveA)
surv.rateA[3]<-SurvivalRate(expos3.surviveA)
surv.rateA[4]<-SurvivalRate(expos4.surviveA)

surv.rateB<-rep(0,4)
surv.rateB[1]<-SurvivalRate(expos1.surviveB)
surv.rateB[2]<-SurvivalRate(expos2.surviveB)
surv.rateB[3]<-SurvivalRate(expos3.surviveB)
surv.rateB[4]<-SurvivalRate(expos4.surviveB)

plot(surv.rate,main='Survial rate for all species', xlab='Number of Exposures', ylab='Survival Rate',col=2,type="b")
par(new=T)
plot(surv.rateA,type="b",axes=F,col=3)
par(new=T)
plot(surv.rateB,type="b",axes=F,col=5)


detach(snails)
