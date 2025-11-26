library(dplyr)
library(ggplot2)
library(MASS)
library(cowplot)
library(car)

dat = read.csv("exercise_6/Eulaema.csv")
head(dat)

vif_model_full <- lm(Eulaema_nigrita ~ altitude + MAT + MAP + Tseason + Pseason + forest. + lu_het, data = dat)
vif(vif_model_full)

vif_model_step1 <- lm(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + forest. + lu_het, data = dat)
vif(vif_model_step1)

vif_model_step2 <- lm(Eulaema_nigrita ~ MAT + MAP + Pseason + forest. + lu_het, data = dat)
vif(vif_model_step2)

vif_model_step3 <- lm(Eulaema_nigrita ~ MAT + Pseason + forest. + lu_het, data = dat)
vif(vif_model_step3)

m <- glm(Eulaema_nigrita ~ effort + forest. + MAT + Pseason + lu_het + method, "poisson", data = dat)
summary(m)

m_full <- glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason + lu_het + method, data = dat)
summary(m_full)

beta_lu_het <- 0.031036
sd_lu_het <- sd(dat$lu_het)
effect_lu_het_sd <- 100 * (exp(beta_lu_het * sd_lu_het) - 1)

beta_method_traps <- 0.167504
effect_method_traps <- 100 * (exp(beta_method_traps) - 1) # methods are categorical

beta_forest <- -1.250720
sd_forest <- sd(dat$forest.)
effect_forest <- 100 * (exp(beta_forest * sd_forest) - 1)

beta_MAT <- 0.010593
sd_MAT <- sd(dat$MAT)
effect_MAT <- 100 * (exp(beta_MAT * sd_MAT) - 1)

beta_Pseason <- 0.018179
sd_Pseason <- sd(dat$Pseason)
effect_Pseason <- 100 * (exp(beta_Pseason * sd_Pseason) - 1)

m_strong <- glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason, data = dat)

plot_effect_mean_effort <- function(predictor_var, color, title_suffix, model = m_strong, data = dat) {
  
  xx_var <- seq(min(data[[predictor_var]]), max(data[[predictor_var]]), length.out = 100)
  
  # Δημιουργία newdata, ΧΡΗΣΙΜΟΠΟΙΩΝΤΑΣ τον μέσο όρο της προσπάθειας
  newdata <- data.frame(
    forest. = rep(mean(data$forest.), 100),
    MAT     = rep(mean(data$MAT), 100),
    Pseason = rep(mean(data$Pseason), 100),
    effort  = rep(mean(dat$effort), 100) 
  )
  newdata[[predictor_var]] <- xx_var

  pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
  fit   <- exp(pred$fit)
  upper <- exp(pred$fit + 1.96*pred$se.fit)
  lower <- exp(pred$fit - 1.96*pred$se.fit)

  df_fit <- data.frame(xx = xx_var, fit = fit, upper = upper, lower = lower)
  names(df_fit)[1] <- predictor_var

  p <- ggplot() +
    geom_point(aes(x = data[[predictor_var]], y = (data$Eulaema_nigrita / data$effort) * mean(dat$effort)), 
               color="darkgrey", alpha=0.3) +
                geom_line(data = df_fit, aes(x = .data[[predictor_var]], y = fit), 
               color = color, linewidth = 1.2) +
                