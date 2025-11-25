library(dplyr)
library(ggplot2)
library(MASS)
library(cowplot)
library(car)

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

ggplot(dat, aes(effort, Eulaema_nigrita)) +
  geom_point() +
  labs(title="Sampling effort vs abundance")

dat$forest_bin <- cut(dat$forest., breaks=4)

b1 <- ggplot(dat, aes(x = forest_bin, y = Eulaema_nigrita)) +
  geom_boxplot(fill="forestgreen", color="black") +
  labs(title="Abundance across Forest cover categories",
       x="Forest cover (binned)", y="Bee count")


dat$MAT_bin = cut(dat$MAT, breaks=4)

b2 = ggplot(dat, aes(x = MAT_bin, y = Eulaema_nigrita)) +
  geom_boxplot(fill="plum", color = "black") +
  labs(title="Abundance across MAT categories",
       x = "MAT (binned)", y = "Bee count")

dat$Pseason_bin <- cut(dat$Pseason, breaks=4)

b3 <- ggplot(dat, aes(x = Pseason_bin, y = Eulaema_nigrita)) +
  geom_boxplot(fill="skyblue", color="black") +
  labs(title="Abundance across Pseason categories",
       x="Precipitation seasonality (binned)", y="Bee count")

plot_grid( b1,b2,b3, ncol = 3,
          labels = c("A", "B", "C"))

m_full = lm(Eulaema_nigrita ~ altitude + MAT + MAP + Tseason + Pseason +
               forest. + lu_het + effort,
             data = dat)

vif(m_full)

m = glm(Eulaema_nigrita ~ effort + altitude + MAT, "poisson", data=dat)
summary(m)

dat_std = dat
dat$altitude = as.numeric(dat$altitude)
dat$MAT = as.numeric(dat$MAT)
dat$MAP = as.numeric(dat$MAP)
dat$Tseason = as.numeric(dat$Tseason)
dat$Pseason = as.numeric(dat$Pseason)
dat$forest. = as.numeric(dat$forest.)
dat$lu_het = as.numeric(dat$lu_het)

m_nb = glm.nb(Eulaema_nigrita ~ offset(log(effort)) + altitude + MAT + MAP + 
                Tseason + Pseason + forest. + lu_het, data = dat_std) # model accounts for varying sampling effort

summary(m_nb)
vif(m_nb)
exp(coef(m_nb))

m_strong = glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason, data = dat_std) # model accounts for varying sampling effort
summary(m_strong)

# xx_forest = seq(min(dat_std$forest.), max(dat_std$forest.), length.out = 100)
# newdata_forest = data.frame(
#   forest. = xx_forest,
#   MAT     = mean(dat_std$MAT),
#   Pseason = mean(dat_std$Pseason),
#   effort  = 1
# )

# pred_forest = predict(m_strong, newdata = newdata_forest, type = "link", se.fit = TRUE)
# fit_forest = exp(pred_forest$fit)
# upper_forest = exp(pred_forest$fit + 1.96*pred_forest$se.fit)
# lower_forest = exp(pred_forest$fit - 1.96*pred_forest$se.fit)
# lower_forest[lower_forest < 0] = 0

# df_forest = data.frame(
#   forest. = xx_forest,
#   fit = fit_forest,
#   upper = upper_forest,
#   lower = lower_forest
# )

# p1 = ggplot() +
#   geom_point(aes(x = dat_std$forest., y = dat_std$Eulaema_nigrita), color="darkgrey") +
#   geom_line(data = df_forest, aes(x = forest., y = fit), color="forestgreen", size=1.2) +
#   geom_ribbon(data = df_forest, aes(x = forest., ymin = lower, ymax = upper),
#               fill="darkgreen", alpha=0.3) +
#   labs(x = "Forest cover", y = "Abundance",
#        title = "Effect of Forest Cover on Eulaema nigrita")


# xx_MAT <- seq(min(dat_std$MAT), max(dat_std$MAT), length.out = 100)
# newdata_MAT <- data.frame(
#   forest. = mean(dat_std$forest.),
#   MAT     = xx_MAT,
#   Pseason = mean(dat_std$Pseason),
#   effort  = 1
# )

# pred_MAT <- predict(m_strong, newdata = newdata_MAT, type = "link", se.fit = TRUE)
# fit_MAT <- exp(pred_MAT$fit)
# upper_MAT <- exp(pred_MAT$fit + 1.96*pred_MAT$se.fit)
# lower_MAT <- exp(pred_MAT$fit - 1.96*pred_MAT$se.fit)
# lower_MAT[lower_MAT < 0] <- 0

# df_MAT <- data.frame(
#   MAT = xx_MAT,
#   fit = fit_MAT,
#   upper = upper_MAT,
#   lower = lower_MAT
# )

# p2 = ggplot() +
#   geom_point(aes(x = dat_std$MAT, y = dat_std$Eulaema_nigrita), color="darkgrey") +
#   geom_line(data = df_MAT, aes(x = MAT, y = fit), color="red", size=1.2) +
#   geom_ribbon(data = df_MAT, aes(x = MAT, ymin = lower, ymax = upper),
#               fill="pink", alpha=0.3) +
#   labs(x = "Mean Annual Temperature", y = "Abundance",
#        title = "Effect of MAT on Eulaema nigrita")


# xx_Pseason <- seq(min(dat_std$Pseason), max(dat_std$Pseason), length.out = 100)
# newdata_Pseason <- data.frame(
#   forest. = mean(dat_std$forest.),
#   MAT     = mean(dat_std$MAT),
#   Pseason = xx_Pseason,
#   effort  = 1
# )

# pred_Pseason <- predict(m_strong, newdata = newdata_Pseason, type = "link", se.fit = TRUE)
# fit_Pseason <- exp(pred_Pseason$fit)
# upper_Pseason <- exp(pred_Pseason$fit + 1.96*pred_Pseason$se.fit)
# lower_Pseason <- exp(pred_Pseason$fit - 1.96*pred_Pseason$se.fit)
# lower_Pseason[lower_Pseason < 0] <- 0

# df_Pseason <- data.frame(
#   Pseason = xx_Pseason,
#   fit = fit_Pseason,
#   upper = upper_Pseason,
#   lower = lower_Pseason
# )

# p3 = ggplot() +
#   geom_point(aes(x = dat_std$Pseason, y = dat_std$Eulaema_nigrita), color="darkgrey") +
#   geom_line(data = df_Pseason, aes(x = Pseason, y = fit), color="skyblue", size=1.2) +
#   geom_ribbon(data = df_Pseason, aes(x = Pseason, ymin = lower, ymax = upper),
#               fill="blue", alpha=0.3) +
#   labs(x = "Precipitation seasonality", y = "Abundance",
#        title = "Effect of Pseason on Eulaema nigrita")


# plot_grid( p1,p2,p3, ncol = 3,
#           labels = c("A", "B", "C"))


m_plot <- glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason,
                 data = dat)
summary(m_plot)

xx_forest <- seq(min(dat$forest.), max(dat$forest.), length.out = 100)

newdata_forest <- data.frame(
  forest. = xx_forest,
  MAT     = mean(dat$MAT),
  Pseason = mean(dat$Pseason),
  effort  = 1
)

pred <- predict(m_plot, newdata = newdata_forest, type = "link", se.fit = TRUE)

fit   <- exp(pred$fit)
upper <- exp(pred$fit + 1.96*pred$se.fit)
lower <- exp(pred$fit - 1.96*pred$se.fit)
lower[lower < 0] <- 0

df <- data.frame(forest. = xx_forest, fit = fit, upper = upper, lower = lower)

p1 = ggplot() +
  geom_point(aes(x = dat$forest., y = dat$Eulaema_nigrita), color="darkgrey") +
  geom_line(data = df, aes(x = forest., y = fit), color="darkgreen", size=1.2) +
  geom_ribbon(data = df, aes(x = forest., ymin = lower, ymax = upper),
              fill="lightgreen", alpha=0.3) +
  labs(x = "Forest cover", y = "Abundance",
       title = "Effect of Forest Cover on Eulaema nigrita")


xx_MAT <- seq(min(dat$MAT), max(dat$MAT), length.out = 100)

newdata_MAT <- data.frame(
  forest. = mean(dat$forest.),
  MAT     = xx_MAT,
  Pseason = mean(dat$Pseason),
  effort  = 1
)

pred <- predict(m_plot, newdata = newdata_MAT, type = "link", se.fit = TRUE)

fit   <- exp(pred$fit)
upper <- exp(pred$fit + 1.96*pred$se.fit)
lower <- exp(pred$fit - 1.96*pred$se.fit)
lower[lower < 0] <- 0

df <- data.frame(MAT = xx_MAT, fit = fit, upper = upper, lower = lower)

p2 = ggplot() +
  geom_point(aes(x = dat$MAT, y = dat$Eulaema_nigrita), color="darkgrey") +
  geom_line(data = df, aes(x = MAT, y = fit), color="red", size=1.2) +
  geom_ribbon(data = df, aes(x = MAT, ymin = lower, ymax = upper),
              fill="pink", alpha=0.3) +
  labs(x = "Mean Annual Temperature", y = "Abundance",
       title = "Effect of MAT on Eulaema nigrita")


xx_Pseason <- seq(min(dat$Pseason), max(dat$Pseason), length.out = 100)

newdata_Pseason <- data.frame(
  forest. = mean(dat$forest.),
  MAT     = mean(dat$MAT),
  Pseason = xx_Pseason,
  effort  = 1
)

pred <- predict(m_plot, newdata = newdata_Pseason, type = "link", se.fit = TRUE)

fit   <- exp(pred$fit)
upper <- exp(pred$fit + 1.96*pred$se.fit)
lower <- exp(pred$fit - 1.96*pred$se.fit)
lower[lower < 0] <- 0

df <- data.frame(Pseason = xx_Pseason, fit = fit, upper = upper, lower = lower)

p3 = ggplot() +
  geom_point(aes(x = dat$Pseason, y = dat$Eulaema_nigrita), color="darkgrey") +
  geom_line(data = df, aes(x = Pseason, y = fit), color="skyblue", size=1.2) +
  geom_ribbon(data = df, aes(x = Pseason, ymin = lower, ymax = upper),
              fill="blue", alpha=0.3) +
  labs(x = "Precipitation seasonality", y = "Abundance",
       title = "Effect of Pseason on Eulaema nigrita")

plot_grid( p1,p2,p3, ncol = 3,
          labels = c("A", "B", "C"))


