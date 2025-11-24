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

plot(x, y, las=1, col="darkgrey", pch=16)
xx = seq(min(x), max(x), 0.01) # grid of x values
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T) # type="response" → gives predicted mean count, not log(µ)
# se.fit=T → also compute standard errors of predictions
lines(xx, y_hat$fit) # y_hat$fit, Fitted Poisson mean for each xx
# model prediction line
# y_hat$se.fit, Standard error for each fitted value
#lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
#lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)

# plotting the observed data, the model’s predicted Poisson mean, 
# and shading the 95% confidence interval around that prediction.
polygon(c(xx, rev(xx)), # 95% confidence interval ribbon
c(y_hat$fit+1.96*y_hat$se.fit,
rev(y_hat$fit-1.96*y_hat$se.fit)),
col = rgb(0,1,0,.5), border = FALSE)
# The shaded region represents where the true mean curve is likely to lie, 
# given uncertainty in the model estimates

# Pseudo r^2
r.squaredGLMM(m)
1-(m$deviance/m$null.deviance)
