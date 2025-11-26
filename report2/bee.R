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

