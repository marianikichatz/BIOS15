library(dplyr)
library(ggplot2)
library(MASS)
library(cowplot)
library(car)

dat = read.csv("exercise_6/Eulaema.csv")
head(dat)

ggplot(dat, aes(x = Eulaema_nigrita)) +
  geom_histogram( fill = "plum", color = "black") +
  labs(title = "Histogram of Eulaema_nigrita Count", x = "Count", y = "Frequency") +
  theme_minimal()

vif_model_full = lm(Eulaema_nigrita ~ altitude + MAT + MAP + Tseason + Pseason + forest. + lu_het, data = dat)
vif(vif_model_full)

vif_model_step1 = lm(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + forest. + lu_het, data = dat)
vif(vif_model_step1)

vif_model_step2 = lm(Eulaema_nigrita ~ MAT + MAP + Pseason + forest. + lu_het, data = dat)
vif(vif_model_step2)

vif_model_step3 = lm(Eulaema_nigrita ~ MAT + Pseason + forest. + lu_het, data = dat)
vif(vif_model_step3)

m = glm(Eulaema_nigrita ~ effort + forest. + MAT + Pseason + lu_het + method, "poisson", data = dat)
summary(m)

m_full = glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason + lu_het + method, data = dat)
summary(m_full)

dat$rate = dat$Eulaema_nigrita / dat$effort 

ggplot(dat, aes(x = method, y = rate, fill = method)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(y = "Observed Abundance Rate (E. nigrita / effort)", 
       x = "Sampling Method", 
       title = "Difference in E. nigrita Rate by Sampling Method") +
  theme_minimal()

beta_lu_het = 0.031036
sd_lu_het = sd(dat$lu_het)
effect_lu_het_sd = 100 * (exp(beta_lu_het * sd_lu_het) - 1)

beta_method_traps = 0.167504
effect_method_traps = 100 * (exp(beta_method_traps) - 1) # methods are categorical

beta_forest = -1.250720
sd_forest = sd(dat$forest.)
effect_forest = 100 * (exp(beta_forest * sd_forest) - 1)

beta_MAT = 0.010593
sd_MAT = sd(dat$MAT)
effect_MAT = 100 * (exp(beta_MAT * sd_MAT) - 1)

beta_Pseason = 0.018179
sd_Pseason = sd(dat$Pseason)
effect_Pseason = 100 * (exp(beta_Pseason * sd_Pseason) - 1)

m_strong = glm.nb(Eulaema_nigrita ~ offset(log(effort)) + forest. + MAT + Pseason, data = dat)
summary(m_strong)

plot_effect = function(predictor_var, color, small_titles, model = m_strong, data = dat) {
  
  xx_var = seq(min(data[[predictor_var]]), max(data[[predictor_var]]), length.out = 100)
  
  bees = data.frame(
    forest. = rep(mean(data$forest.), 100),
    MAT     = rep(mean(data$MAT), 100),
    Pseason = rep(mean(data$Pseason), 100),
    effort  = rep(mean(dat$effort), 100) 
  )
  bees[[predictor_var]] = xx_var

  pred = predict(model, newdata = bees, type = "link", se.fit = TRUE)
  fit   = exp(pred$fit)
  upper = exp(pred$fit + 1.96*pred$se.fit)
  lower = exp(pred$fit - 1.96*pred$se.fit)

  df_fit = data.frame(xx = xx_var, fit = fit, upper = upper, lower = lower)
  names(df_fit)[1] = predictor_var

  p = ggplot() +
    geom_point(aes(x = data[[predictor_var]], y = (data$Eulaema_nigrita / data$effort) * mean(data$effort)), 
               color="darkgrey", alpha=0.3) +
                geom_line(data = df_fit, aes(x = .data[[predictor_var]], y = fit), 
               color = color, linewidth = 1.2) +
                geom_ribbon(data = df_fit, aes(x = .data[[predictor_var]], ymin = lower, ymax = upper), 
               fill = color, alpha = 0.3) +
                labs(x = small_titles$x_label, 
       y = paste0("Predicted Abundance (Per ", round(mean(dat$effort), 2), " Hours of Sampling)"), 
       title = small_titles$title) + 
  theme_minimal()
  
return(p)
}

labels_forest = list(x_label = "Forest cover", title = "Effect of Forest Cover on E. nigrita")
p1 = plot_effect("forest.", "darkgreen", labels_forest)

labels_MAT = list(x_label = "Mean Annual Temperature", title = "Effect of MAT on E. nigrita")
p2 = plot_effect("MAT", "pink", labels_MAT)

labels_Pseason = list(x_label = "Precipitation seasonality", title = "Effect of Pseason on E. nigrita")
p3 = plot_effect("Pseason", "skyblue", labels_Pseason)

plot_grid( p1, p2, p3, ncol = 3,
           labels = c("A", "B", "C"))




