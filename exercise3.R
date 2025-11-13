#Analysis of variance (ANOVA)

# aim of our ANOVA analysis is to partition the variance in 
# our response variable into a set of additive components

set.seed(100)
groups = as.factor(rep(c("Low", "Medium", "High"), each=50))
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))

boxplot(x ~ groups, las = 1, xlab = "", ylab = "Body size (g)", main = "Body Size by Group", width = c(0.4, 0.4, 0.4))

box_pos <- 1:length(levels(groups))
stripchart(x ~ groups, vertical = TRUE, method = "jitter", pch = 21, col = "pink", add = TRUE, at = box_pos - 0.5)

m = lm(x~groups)
anova(m)

SS_T = 319.97+1200.43
SS_T/(150-1)
319.97/SS_T

summary(m)

groups = factor(groups, levels=c("Low", "Medium", "High"))
m = lm(x~groups)
summary(m)

m = lm(x~groups-1)
summary(m)$coef

confint(m)


