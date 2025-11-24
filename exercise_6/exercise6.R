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
# Delta method → an approximation method for non-linear models
# Lognormal estimation
# Trigamma function → a special mathematical function needed to estimate variance of the log-transformed mean
1-(m$deviance/m$null.deviance) # deviance R², how much deviance is explained by the predictors
#For a simple Poisson GLM with no random effects, 1 - deviance/null.deviance is usually similar to R2m from r.squaredGLMM.
# For models with random effects or more complicated structures, r.squaredGLMM gives a more meaningful decomposition of variance.

# overdispersion

set.seed(1)
x = rnorm(200, 10, 3) # input variable — think of it as “something that might influence the outcome"
eta =-2 + 0.2*x # eta is the expected trend before we add random variation
# y is integer count data with overdispersion, suitable for negative binomial regression
y = floor(exp(eta + rnbinom(200, 1, mu=.8))) # mean count μ=exp(η), trend + randomness
# rnbinom(n, size, mu) generates negative binomial random numbers
# size = 1 → controls overdispersion (small size → more variance than Poisson)
# mu = 0.8 → mean of the Negative Bionomial noise
# linear predictor + NB noise
# exp → ensures positive counts
par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m = glm(y~x, family="poisson")
summary(m)

library(MASS)
m = glm.nb(y~x) # fits the negative bionomial regression
summary(m)

xx = seq(min(x), max(x), length.out = 100)
y_hat = predict(m, newdata = data.frame(x=xx), type="response", se.fit = TRUE)
fit = exp(y_hat$fit)
upper = exp(y_hat$fit + 1.96*y_hat$se.fit)
lower = exp(y_hat$fit - 1.96*y_hat$se.fit)
plot(x, y, col="darkgrey", pch=16, las=1, main="Negative Bionomial")
lines(xx, fit, col="blue", lwd=2)
polygon(c(xx, rev(xx)),
        c(upper, rev(lower)),
        col=rgb(0, 1, 0, 0.3), border = NA)


set.seed(1)
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y_poisson = rpois(200, lambda = exp(eta))  # Poisson counts
mu = exp(eta)  # expected mean counts
sd_y = sqrt(mu)
upper = mu + 1.96 * sd_y
lower = mu - 1.96 * sd_y
lower[lower < 0] = 0  # counts can't be negative
plot(x, y_poisson, col="darkgrey", pch=16, las=1, main="Poisson Simulation")
o = order(x)
xs = x[o]
mus = mu[o]
uppers = upper[o]
lowers = lower[o]
lines(xs, mus)
polygon(c(xs, rev(xs)), c(uppers, rev(lowers)), col=rgb(0,1,0,0.3), border=NA)  # 95% ribbon


