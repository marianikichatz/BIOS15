install.packages("MuMIn")

library(ggplot2)
library(MuMIn)

#Binomial distribution

x = seq(from=0, to=1, by=0.01)
v_b = x*(1-x) #Binomial variance
plot(x, v_b, type="l", xlab="Probability", ylab="Theoretical variance", las=1)


# logit: maps probability → real number
# invlogit: maps real number → probability

logit = function(x) log(x/(1-x)) # logit transformation, transforms probabilities from the [0,1] range to the whole real line (−∞,∞)
invlogit = function(x) 1/(1+exp(-x)) # If you have a real number x, it transforms it back to a probability

x = runif(200)
logit_x = logit(x)
par(mfrow=c(2,2))
hist(x, las=1)
hist(logit_x, las=1)

xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1,
xlab="Logit (x)",
ylab="P")
plot(x, invlogit(logit_x), las=1)

# probit link, which corresponds to the quantile distribution of the standard normal distribution (which is why we can use the pnorm function to compute the inverse)

plot(xx, invlogit(xx), type="l", las=1,
xlab="Logit/Probit (x)",
ylab="P")
lines(xx, pnorm(xx), lty=2)
legend("topleft", legend=c("Logit", "Probit"),
lty=c(1,2), bty="n")

# logit and probit are S-shaped (sigmoid) functions mapping real numbers to probabilities
# logit curve is slightly “stretched” compared to probit

# Logistic regression

# Compute a linear predictor (η) from your predictors
# Convert η → probabilities using the inverse logit
# Simulate actual 0/1 outcomes using those probabilities

x = rnorm(200, 10, 3)
eta =-2 + 0.4*x + rnorm(200, 0, 2)
p = invlogit(eta)
y = rbinom(200, 1, p)
par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)

# fit the generalized linear model

m = glm(y~x, family=binomial(link="logit"))
summary(m)


# EXERCISE: Replicate the plot below. To produce a regression line,
# we define some new x-values that spans the data range along the x-axis, 
# then obtain predicted valuesˆy (using the model coefficients), 
# and finally transform these values to the probability scale to obtain the predicted probabilitiesˆ p.

coefs = summary(m)$coef # coefs[1,1] → intercept (β0), coefs[2,1] → slope (β1)
x_pred = seq(from=min(x), to=max(x), by=0.01) # sequence covering the range of the data
y_hat = coefs[1,1] + coefs[2,1]*x_pred # η = β0 + β1χ x for each new x
p_hat = invlogit(y_hat) #et probabilities between 0 and 1

pred_df <- data.frame(x = x_pred, p_hat = p_hat)
x_50 <- -coefs[1]/coefs[2]

ggplot() +
  geom_point(aes(x = x, y = y), color = "pink") +  # actual data (y vs x)
  geom_line(data = pred_df, aes(x = x, y = p_hat), color = "plum") + # predicted probability curve from logistic regression
  geom_vline(xintercept = x_50, linetype = "dashed", color = "lightblue") + # probability = 0.5 and the corresponding x (x_50)
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "lightblue") +  # probability = 0.5 and the corresponding x (x_50)
  labs(x = "x", y = "y") +
  theme_minimal()

#Pseudo-r^2

r.squaredGLMM(m)

# coefficient of discrimination

y_hat = coefs[1,1] + coefs[2,1]*x
p_hat = invlogit(y_hat)
mean(p_hat[which(y==1)])- mean(p_hat[which(y==0)])


