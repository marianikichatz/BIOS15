x = rpois(200, 3)
hist(x, las=1)

# Poisson distributions are discrete, you compute the probability at each value, not over an interval
x = seq(0, 20, 1)
y = dpois(x, lambda=1) # dpois gives the PMF values, probability mass function
plot(x,y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)  # the probability that the random variable X equals the specific integer value k
points(x, dpois(x, lambda=3), type="b", pch=16, col=2)
points(x, dpois(x, lambda=10), type="b", pch=16, col=3)
legend("topright", col=1:3, pch=16,
legend=c(expression(paste(lambda, " = 1")),
expression(paste(lambda, " = 3")),
expression(paste(lambda, " = 10"))))


x = rnorm(200, 10, 3)
eta =-2 + 0.2*x  # linear predictor
y = ceiling(exp(eta + rpois(200, 0.3))) # exp(eta) converts the linear predictor into positive mean counts
# ceiling() forces y to be integers ≥ 1
# y is behaving like Poisson-distributed data depending on x
par(mfrow=c(1,2))
plot(x, eta, las=1) # Plotting x vs η shows a straight line because η is linear in x
# model expectation
plot(x, y, las=1) # Plotting x vs. y shows a cloud of count values increasing with x
# real noisy count data would look like

m = glm(y~x, family="poisson")
summary(m)

