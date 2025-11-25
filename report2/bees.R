library(dplyr)
library(ggplot2)
library(MASS)
library(cowplot)

dat = read.csv("exercise_6/Eulaema.csv")
head(dat)

summary(dat$Eulaema_nigrita)
h_bee = ggplot(dat, aes(x = Eulaema_nigrita)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "Bee abundance", x = "Count", y = "Frequency")

summary(dat$effort)
h_effort = ggplot(dat, aes(x = effort)) +
  geom_histogram(fill = "plum", color = "black") +
  labs(title = "Sampling effort (log hours)", x = "Log hours", y = "Frequency")

plot_grid( h_bee, h_effort, ncol = 2,
          labels = c("A", "B"))

m = glm(Eulaema_nigrita ~ effort + altitude + MAT, "poisson", data=dat)
summary(m)

m_nb <- glm.nb(Eulaema_nigrita ~ offset(log(effort)) + altitude + MAT + MAP + 
                Tseason + Pseason + forest. + lu_het, data = dat) # model accounts for varying sampling effort

summary(m_nb)
coef(summary(m_nb))

m_strong <- glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + MAP + Pseason + 
                   lu_het, data = dat) # model accounts for varying sampling effort
summary(m_strong)

# Proportional change

Δx_forest = max(dat$forest.) - min(dat$forest.)
relative_change_forest= exp(Δx_forest*-0.9369490)
percent_change_forest = (relative_change_forest -1)*100

Δx_MAT = max(dat$MAT) - min(dat$MAT)
relative_change_MAT = exp(Δx_MAT*0.0031881)
percent_change_MAT = (relative_change_MAT -1)*100

Δx_MAP = max(dat$MAP) - min(dat$MAP)
relative_change_MAP = exp(Δx_MAP*-0.0012672)
percent_change_MAP = (relative_change_MAP -1)*100

Δx_Pseason = max(dat$Pseason) - min(dat$Pseason)
relative_change_Pseason = exp(Δx_Pseason*0.0164724)
percent_change_Pseason = (relative_change_Pseason -1)*100

Δx_lu_het = max(dat$lu_het) - min(dat$lu_het)
relative_change_lu_het = exp(Δx_lu_het*-0.0319785)
percent_change_lu_het = (relative_change_lu_het -1)*100


par(mfrow = c(2,3), mar=c(4,4,2,1))
x_forest <- dat$forest.
y <- dat$Eulaema_nigrita
xx_forest <- seq(min(x_forest), max(x_forest), length.out = 100)
forest_data <- data.frame(
  forest. = xx_forest,
  MAT = mean(dat$MAT),
  MAP = mean(dat$MAP),
  Pseason = mean(dat$Pseason),
  lu_het = mean(dat$lu_het),
  effort = 1  
)
y_hat_forest <- predict(m_strong, newdata = forest_data, type = "link", se.fit = TRUE)
fit <- exp(y_hat_forest$fit)
upper <- exp(y_hat_forest$fit + 1.96 * y_hat_forest$se.fit)
lower <- exp(y_hat_forest$fit - 1.96 * y_hat_forest$se.fit)
lower[lower < 0] <- 0 

pfor = plot(x_forest, y, col="darkgrey", pch=16, las=1,
     main="Effect of forest cover on Eulaema abundance",
     xlab="Forest cover", ylab="Abundance")
lines(xx_forest, fit, col="forestgreen", lwd=2)
polygon(c(xx_forest, rev(xx_forest)), c(upper, rev(lower)),
        col=rgb(0, 1, 0, 0.3), border=NA)


x_MAT <- dat$MAT
xx_MAT <- seq(min(x_MAT), max(x_MAT), length.out = 100)

data_MAT <- data.frame(
  forest. = mean(dat$forest.),
  MAT = xx_MAT,
  MAP = mean(dat$MAP),
  Pseason = mean(dat$Pseason),
  lu_het = mean(dat$lu_het),
  effort = 1
)

y_hat_MAT <- predict(m_strong, newdata = data_MAT, type = "link", se.fit = TRUE)
fit_MAT <- exp(y_hat_MAT$fit)
upper_MAT <- exp(y_hat_MAT$fit + 1.96 * y_hat_MAT$se.fit)
lower_MAT <- exp(y_hat_MAT$fit - 1.96 * y_hat_MAT$se.fit)
lower_MAT[lower_MAT < 0] <- 0

pmat = plot(x_MAT, y, col="darkgrey", pch=16, las=1,
     main="Effect of MAT on Eulaema abundance",
     xlab="Mean Annual Temperature", ylab="Abundance")
lines(xx_MAT, fit_MAT, col="red", lwd=2)
polygon(c(xx_MAT, rev(xx_MAT)), c(upper_MAT, rev(lower_MAT)),
        col=rgb(1, 0, 0, 0.3), border=NA)

x_MAP <- dat$MAP
xx_MAP <- seq(min(x_MAP), max(x_MAP), length.out = 100)

data_MAP <- data.frame(
  forest. = mean(dat$forest.),
  MAT = mean(dat$MAT),
  MAP = xx_MAP,
  Pseason = mean(dat$Pseason),
  lu_het = mean(dat$lu_het),
  effort = 1
)

y_hat_MAP <- predict(m_strong, newdata = data_MAP, type = "link", se.fit = TRUE)
fit_MAP <- exp(y_hat_MAP$fit)
upper_MAP <- exp(y_hat_MAP$fit + 1.96 * y_hat_MAP$se.fit)
lower_MAP <- exp(y_hat_MAP$fit - 1.96 * y_hat_MAP$se.fit)
lower_MAP[lower_MAP < 0] <- 0

pmap = plot(x_MAP, y, col="darkgrey", pch=16, las=1,
     main="Effect of MAP on Eulaema abundance",
     xlab="Mean Annual Precipitation", ylab="Abundance")
lines(xx_MAP, fit_MAP, col="pink", lwd=2)
polygon(c(xx_MAP, rev(xx_MAP)), c(upper_MAP, rev(lower_MAP)),
        col=rgb(1, 0, 0, 0.3), border=NA)

x_Pseason <- dat$Pseason
xx_Pseason <- seq(min(x_Pseason), max(x_Pseason), length.out = 100)

data_Pseason <- data.frame(
  forest. = mean(dat$forest.),
  MAT = mean(dat$MAT),
  MAP = mean(dat$MAP),
  Pseason = xx_Pseason,
  lu_het = mean(dat$lu_het),
  effort = 1
)

y_hat_Pseason <- predict(m_strong, newdata = data_Pseason, type = "link", se.fit = TRUE)
fit_Pseason <- exp(y_hat_Pseason$fit)
upper_Pseason <- exp(y_hat_Pseason$fit + 1.96 * y_hat_Pseason$se.fit)
lower_Pseason <- exp(y_hat_Pseason$fit - 1.96 * y_hat_Pseason$se.fit)
lower_Pseason[lower_Pseason < 0] <- 0

ppseason = plot(x_Pseason, y, col="darkgrey", pch=16, las=1,
     main="Effect of Precipitation Seasonality on Eulaema abundance",
     xlab="Precipitation Seasonality (%)", ylab="Abundance")
lines(xx_Pseason, fit_Pseason, col="blue", lwd=2)
polygon(c(xx_Pseason, rev(xx_Pseason)), c(upper_Pseason, rev(lower_Pseason)),
        col=rgb(0, 0, 1, 0.3), border=NA)

x_lu_het <- dat$lu_het
xx_lu_het <- seq(min(x_lu_het), max(x_lu_het), length.out = 100)

data_lu_het <- data.frame(
  forest. = mean(dat$forest.),
  MAT = mean(dat$MAT),
  MAP = mean(dat$MAP),
  Pseason = mean(dat$Pseason),
  lu_het = xx_lu_het,
  effort = 1
)

y_hat_lu_het <- predict(m_strong, newdata = data_lu_het, type = "link", se.fit = TRUE)
fit_lu_het <- exp(y_hat_lu_het$fit)
upper_lu_het <- exp(y_hat_lu_het$fit + 1.96 * y_hat_lu_het$se.fit)
lower_lu_het <- exp(y_hat_lu_het$fit - 1.96 * y_hat_lu_het$se.fit)
lower_lu_het[lower_lu_het < 0] <- 0

plu = plot(x_lu_het, y, col="darkgrey", pch=16, las=1,
     main="Effect of Land-use Heterogeneity on Eulaema abundance",
     xlab="Land-use Heterogeneity", ylab="Abundance")
lines(xx_lu_het, fit_lu_het, col="plum", lwd=2)
polygon(c(xx_lu_het, rev(xx_lu_het)), c(upper_lu_het, rev(lower_lu_het)),
        col=rgb(0.5, 0, 0.5, 0.3), border=NA)

par(mfrow=c(1,1))


