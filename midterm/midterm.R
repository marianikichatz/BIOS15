install.packages("glmmTMB")
install.packages("ggeffects")
install.packages("performance")
library(performance)
library(glmmTMB)
library(tidyverse)
library(ggeffects)

data = read.csv(file = "midterm/exam2023_data-2.csv")
data = na.omit(data)

head(data)
summary(data)
colnames(data) 

data$Eucalyptus_seedlings = data$euc_sdlgs50cm.2m + data$euc_sdlgs0_50cm + data$euc_sdlgs.2m

data$Season = factor(data$Season)
data$Property = factor(data$Property) # random effect
data$Aspect = factor(data$Aspect)
data$Landscape.position = factor(data$Landscape.position)

# diff types of plants

data$grass_cover = 
  data$ExoticAnnualGrass_cover +
  data$ExoticPerennialGrass_cover +
  data$NativePerennialGrass_cover 

data$graminoid_cover = data$NativePerennialGraminoid_cover

data$herb_cover = 
  data$ExoticAnnualHerb_cover +
  data$ExoticPerennialHerb_cover +
  data$NativePerennialHerb_cover

data$shrub_cover = data$ExoticShrub_cover + data$NativeShrub_cover

data$fern_cover = data$NativePerennialFern_cover

# diff life circles

data$annual_cover = data$ExoticAnnualGrass_cover + data$ExoticAnnualHerb_cover

data$perennial_cover = data$ExoticPerennialGrass_cover + data$ExoticPerennialHerb_cover +
  data$NativePerennialGrass_cover +
  data$NativePerennialGraminoid_cover +
  data$NativePerennialHerb_cover +
  data$NativePerennialFern_cover +
  data$ExoticShrub_cover +
  data$NativeShrub_cover

# diff origin

data$native_cover = data$NativePerennialGrass_cover +
  data$NativePerennialGraminoid_cover +
  data$NativePerennialHerb_cover +
  data$NativePerennialFern_cover +
  data$NativeShrub_cover

data$exotic_cover = data$ExoticAnnualGrass_cover +
  data$ExoticPerennialGrass_cover +
  data$ExoticAnnualHerb_cover +
  data$ExoticPerennialHerb_cover +
  data$ExoticShrub_cover

# diff origin and type

data$native_grass = data$NativePerennialGrass_cover + data$NativePerennialGraminoid_cover
data$native_herb  = data$NativePerennialHerb_cover
data$native_fern  = data$NativePerennialFern_cover
data$native_shrub = data$NativeShrub_cover

data$exotic_grass = data$ExoticAnnualGrass_cover + data$ExoticPerennialGrass_cover
data$exotic_herb  = data$ExoticAnnualHerb_cover + data$ExoticPerennialHerb_cover
data$exotic_shrub = data$ExoticShrub_cover

# competition between seedlings and cover from all the plants

data$competition = data$grass_cover + data$graminoid_cover + data$herb_cover + data$fern_cover + data$shrub_cover

# z-score
num_vars = c("grass_cover", "graminoid_cover", "herb_cover", "fern_cover",
              "shrub_cover", "Euc_canopy_cover", 
              "annual_cover", "perennial_cover",
              "native_cover", "exotic_cover",
              "native_grass", "native_herb", "native_fern", "native_shrub",
              "exotic_grass", "exotic_herb", "exotic_shrub",
              "competition")

data_scaled = data %>%
  mutate(across(all_of(num_vars), ~ scale(.)[,1]))

# models
# model cover
m_cover1 = glmmTMB(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover + (1 | Property),family = nbinom1, data = data_scaled)
m_cover2 = glmmTMB(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover + (1 | Property),family = nbinom2, data = data_scaled)
AIC(m_cover1,m_cover2)


vif_cover = check_collinearity(m_cover1)

summary(m_cover1)

# model life
m_life1 = glmmTMB(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover + (1 | Property), family = nbinom1, data = data_scaled)
m_life2 = glmmTMB(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover + (1 | Property), family = nbinom2, data = data_scaled)
AIC(m_life1, m_life2)
 
vif_life = check_collinearity(m_life1)

summary(m_life1)

# model where
m_where1 = glmmTMB(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover + (1 | Property),family = nbinom1, data = data_scaled)
m_where2 = glmmTMB(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover + (1 | Property),family = nbinom2, data = data_scaled)
AIC(m_where1, m_where2)

vif_where = check_collinearity(m_where1)

summary(m_where1)

# model boss 
m_boss1 = glmmTMB(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover + (1 | Property), family = nbinom1, data = data_scaled)
m_boss2 = glmmTMB(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover + (1 | Property), family = nbinom2, data = data_scaled)
AIC(m_boss1,m_boss2)

vif_boss = check_collinearity(m_boss1)
summary(m_boss1)

# model competition 
m_competition1 = glmmTMB(Eucalyptus_seedlings ~ competition + (1 | Property),family = nbinom1,data = data_scaled)
m_competition2 = glmmTMB(Eucalyptus_seedlings ~ competition + (1 | Property),family = nbinom2,data = data_scaled)
AIC(m_competition1,m_competition2)

summary(m_competition1)

# check for vif

vif_cover
vif_life
vif_where
vif_boss

# Plots

data_long_cover = data %>%
  pivot_longer(cols = c(grass_cover, herb_cover, fern_cover, shrub_cover,graminoid_cover ), names_to = "plant_type", values_to = "cover")

ggplot(data_long_cover, aes(x = plant_type, y = cover, fill = plant_type)) +
  geom_boxplot(alpha = 0.7) +       
  xlab("Plant type") +
  ylab("Cover") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot_prediction = function(model, predictor_var, data_scaled, title_text, plot_color, y_limit = NULL) {
  x_range = seq(min(data_scaled[[predictor_var]]),
                max(data_scaled[[predictor_var]]),
                length.out = 100)
  
  model_vars = names(fixef(model)$cond)[-1] 
  
  newdat = data.frame(matrix(0, nrow = 100, ncol = length(model_vars)))
  colnames(newdat) = model_vars

  newdat[[predictor_var]] = x_range

  newdat$Property = NA

  pred = predict(model, newdata = newdat, type = "response", se.fit = TRUE, re.form = NA)
  z = qnorm(0.98) 
  
  newdat$fit   = pred$fit
  newdat$lower = pred$fit - z * pred$se.fit
  newdat$upper = pred$fit + z * pred$se.fit

  p = ggplot() +
    geom_ribbon(data = newdat, aes(x = !!sym(predictor_var), ymin = lower, ymax = upper),
                alpha = 0.2, fill = plot_color) + 
    geom_line(data = newdat, aes(x = !!sym(predictor_var), y = fit), 
              color = plot_color, size = 1) +    
    geom_point(data = data_scaled, aes(x = !!sym(predictor_var), y = Eucalyptus_seedlings),
               alpha = 0.5, color = plot_color) + 
    theme_bw() +
    labs(title = title_text,
         x = paste0(stringr::str_to_title(gsub("_", " ", predictor_var)), " (scaled)"),
         y = "Predicted Eucalyptus Seedlings")

if (!is.null(y_limit)) {
    p = p + coord_cartesian(ylim = c(0, y_limit))
  }
  
  print(p)

}

plot_prediction(m_cover1, "herb_cover", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Herb Cover (Controlling for other Cover Types)", 
                plot_color = "pink", y_limit = 100)

plot_prediction(m_cover1, "grass_cover", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Grass Cover (Controlling for other Cover Types)", 
                plot_color = "darkgreen", y_limit = 100)

data_long_life = data %>%
  pivot_longer(cols = c("annual_cover", "perennial_cover"), names_to = "Life_Duration", values_to = "cover")

ggplot(data_long_life, aes(x = Life_Duration, y = cover, fill = Life_Duration)) +
  geom_boxplot(alpha = 0.7) +       
  xlab("Life Duration") +
  ylab("Cover") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_prediction(m_life1, "annual_cover", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Annual Cover (Controlling for Perennial Cover)", 
                plot_color = "plum", y_limit = 100)

plot_prediction(m_life1, "perennial_cover", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Perennial Cover (Controlling for Annual Cover)", 
                plot_color = "lightblue", y_limit = 100)

data_long_where = data %>%
  pivot_longer(cols = c("native_cover", "exotic_cover"), names_to = "origin", values_to = "cover")

ggplot(data_long_where, aes(x = origin , y = cover, fill = origin)) +
  geom_boxplot(alpha = 0.7) +       
  xlab("Origin") +
  ylab("Cover") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_prediction(m_where1, "exotic_cover", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Exotic Cover (Controlling for Native Cover)", 
                plot_color = "orange", y_limit = 100)

plot_prediction(m_where1, "native_cover", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Native Cover (Controlling for Exotic Cover)", 
                plot_color = "royalblue3", y_limit = 100)

data_long_boss = data %>%
  pivot_longer(cols = c("native_grass",  "native_herb",  "native_fern",  "native_shrub",
  "exotic_grass",  "exotic_herb", "exotic_shrub"), names_to = "origin_and_type", values_to = "cover")

ggplot(data_long_boss, aes(x = origin_and_type , y = cover, fill = origin_and_type)) +
  geom_boxplot(alpha = 0.7) +       
  xlab("Origin and Plant Type") +
  ylab("Cover") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_prediction(m_boss1, "native_herb", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Native Herb Cover (Controlling for other Cover Types)", 
                plot_color = "deeppink4", y_limit = 250)

plot_prediction(m_competition1, "competition", data_scaled, 
                "Predicted Eucalyptus Seedlings vs Competition Cover", 
                plot_color = "slateblue1", y_limit = 100)


