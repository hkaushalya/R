## snorm -
library(fGarch)
# Ranbdom Numbers:
par(mfrow = c(2, 2))
set.seed(1953)
r = rsnorm(n = 1000)
plot(r, type = "l", main = "snorm", col = "steelblue")

# Plot empirical density and compare with true density:
hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
box()
x = seq(min(r), max(r), length = 201)
lines(x, dsnorm(x), lwd = 2)

# Plot df and compare with true df:
plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
lines(x, psnorm(x), lwd = 2)

# Compute quantiles:
round(qsnorm(psnorm(q = seq(-1, 5, by = 1))), digits = 6)

## snormFit -
snormFit(r)

## Not run: 
## snormSlider -
if (require(tcltk)) {
  snormSlider("dist")
  snormSlider("rand")
}

## End(Not run)