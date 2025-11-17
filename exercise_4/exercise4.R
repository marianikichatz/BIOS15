# data with two correlated predictor variables and fits a multiple-regression model

set.seed(187)
x1 = rnorm(200, 10, 2)
x2 = 0.5*x1 + rnorm(200, 0, 4)
y = 0.7*x1 + 2.2*x2 + rnorm(200, 0, 4)
m = lm(y~x1+x2)
coefs = summary(m)$coef

summary(m)

# coefficient of determination (r^2)
y_hat = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2
var(y_hat)
var(y_hat)/var(y)

#To compute the predicted values associated only with x1, we keep x2 constant at its
# mean, and vice versa for the variance associated with x2.

y_hat1 = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*mean(x2)
var(y_hat1)
var(y_hat1)/var(y)

y_hat2 = coefs[1,1] + coefs[2,1]*mean(x1) + coefs[3,1]*x2
var(y_hat2)
var(y_hat2)/var(y)

# compare the sum of the variance explained by x1 and x2 to the total variance in y

var(y_hat)
var(y_hat1) + var(y_hat2)

# Var(x+ y) = Var(x) + Var(y) + 2Cov(x,y)

var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2)

# what would be the variance explained by x1 if it was uncorrelated with x2?
coefs[2,1]^2*var(x1)

# all variances and covariances between the predictors

t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1]
#coefs is a matrix of regression coefficients from your model
#[2:3,1] selects rows 2 and 3 (probably the slopes for x1 and x2) and the first column (the estimate values)
#cbind() combines vectors column-wise into a matrix
#cbind(x1,x2) creates a matrix where the first column is x1 and the second column is x2
#cov() calculates the covariance matrix of the columns in the matrix
#matrix captures both variances and correlations between x1 and x2
#t() takes the transpose of the vector of coefficients, turning it from a column into a row
#%*% is the matrix multiplication operator in R
#Multiply the row vector (t(coefs[2:3,1])) by the covariance matrix S
#Then multiply by the column vector of coefficients again (coefs[2:3,1])
#Result: a single number, the variance in Y explained by x1 and x2 together, including overlap if they are correlated

# standardize predictors to get rid of correlation

x1_z = (x1- mean(x1))/sd(x1)
x2_z = (x2- mean(x2))/sd(x2)
m = lm(y~ x1_z + x2_z)
summary(m)

#Change in Y per 100% of mean change in X

x1_m = (x1- mean(x1))/mean(x1)
x2_m = (x2- mean(x2))/mean(x2)
summary(lm(y~ x1_m + x2_m))

#Multicollinearity
#assessing the degree of multicollinearity is to compute variance inflation factors
#variance inflation factor for covariate x1

m1 = lm(x1~x2)
r2 = summary(m1)$r.squared
1/(1-r2)

# Data exercise: multiple regression and variable selection

plants = read.csv(file="exercise_4/alpineplants.csv")
