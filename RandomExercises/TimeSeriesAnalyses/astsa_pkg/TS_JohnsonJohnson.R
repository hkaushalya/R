rm(list=ls())
library('astsa')
data('jj')
str(jj)
?ts
ts.jj <- ts(data=jj, start=c(1960,1),frequency=4)
ts.plot(ts.jj)
??smoothts

fit <- stl(ts.jj, s.window=4)
plot(fit)
fit2<-decompose(ts.jj)
plot(fit2)
fit2

library(forecast)
forecast(ts.jj,4)
plot(forecast(fit,h=4, method='ets'))