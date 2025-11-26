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
  geom_point(color = "plum") +
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

sd_f = sd(dat_std$forest.)
sd_mat = sd(dat_std$MAT)
sd_ps = sd(dat_std$Pseason)

plot_effect <- function(predictor_var, color, title_suffix) {

  xx_var <- seq(min(dat[[predictor_var]]), max(dat[[predictor_var]]), length.out = 100)

  newdata <- data.frame(
    forest. = rep(mean(dat$forest.), 100),
    MAT     = rep(mean(dat$MAT), 100),
    Pseason = rep(mean(dat$Pseason), 100),
    effort  = rep(1, 100) # Standardized Effort = 1
  )
  
  newdata[[predictor_var]] <- xx_var

  pred <- predict(m_plot, newdata = newdata, type = "link", se.fit = TRUE)

  fit   <- exp(pred$fit)
  upper <- exp(pred$fit + 1.96*pred$se.fit)
  lower <- exp(pred$fit - 1.96*pred$se.fit)
  lower[lower < 0] <- 0

  df_fit <- data.frame(xx = xx_var, fit = fit, upper = upper, lower = lower)
  names(df_fit)[1] <- predictor_var

  p <- ggplot() +
    geom_point(aes(x = dat[[predictor_var]], y = dat$Eulaema_nigrita), color="darkgrey") +
    geom_line(data = df_fit, aes(x = .data[[predictor_var]], y = fit), color = color, size = 1.2) +
    geom_ribbon(data = df_fit, aes(x = .data[[predictor_var]], ymin = lower, ymax = upper),
                fill = color, alpha = 0.3) +
    scale_y_continuous(trans = 'log1p', breaks = c(0, 10, 50, 100, 250, 500, 1000),
                       labels = c(0, 10, 50, 100, 250, 500, 1000)) +
    labs(x = title_suffix$x_label, y = "Abundance (log-transformed)",
         title = title_suffix$title)

  return(p)
}

labels_forest <- list(x_label = "Forest cover", title = "A: Effect of Forest Cover on E. nigrita")
p1_log <- plot_effect("forest.", "darkgreen", labels_forest)

labels_MAT <- list(x_label = "Mean Annual Temperature", title = "B: Effect of MAT on E. nigrita")
p2_log <- plot_effect("MAT", "pink", labels_MAT)

labels_Pseason <- list(x_label = "Precipitation seasonality", title = "C: Effect of Pseason on E. nigrita")
p3_log <- plot_effect("Pseason", "skyblue", labels_Pseason)

plot_grid( p1_log, p2_log, p3_log, ncol = 3,
           labels = c("A", "B", "C"))

plot(residuals(m_strong) ~ fitted(m_strong))
abline(h = 0, lty = 2, col = "red")


m_final <- glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason + method, data = dat)
summary(m_final)
