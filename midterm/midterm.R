install.packages("glmmTMB")
install.packages("performance")
library(performance)
library(glmmTMB)
library(car)
library(tidyverse)

data = read.csv(file = "midterm/exam2023_data-2.csv")
data <- na.omit(data)

head(data)
summary(data)
colnames(data) 

data$Eucalyptus_seedlings = 
  data$euc_sdlgs50cm.2m + data$euc_sdlgs0_50cm + data$euc_sdlgs.2m

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

data$shrub_cover = 
  data$ExoticShrub_cover + data$NativeShrub_cover

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
m_cover1 = glmmTMB(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover + (1 | Property),family = nbinom1, data = data)
m_cover2 = glmmTMB(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover + (1 | Property),family = nbinom2, data = data)
AIC(m_cover1,m_cover2)


vif_cover = check_collinearity(m_cover1)

summary(m_cover1)

# model life
m_life1 = glmmTMB(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover + (1 | Property), family = nbinom1, data = data)
m_life2 = glmmTMB(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover + (1 | Property), family = nbinom2, data = data)
AIC(m_life1, m_life2)
 
vif_life = check_collinearity(m_life1)

summary(m_life1)

# model where
m_where1 = glmmTMB(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover + (1 | Property),family = nbinom1, data = data)
m_where2 = glmmTMB(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover + (1 | Property),family = nbinom2, data = data)
AIC(m_where1, m_where2)

vif_where = check_collinearity(m_where1)

summary(m_where1)

# model boss 
m_boss1 = glmmTMB(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover + (1 | Property), family = nbinom1, data = data)
m_boss2 = glmmTMB(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover + (1 | Property), family = nbinom2, data = data)
AIC(m_boss1,m_boss2)

vif_boss = check_collinearity(m_boss1)
summary(m_boss1)

# model competition 
m_competion1 = glmmTMB(Eucalyptus_seedlings ~ competition + (1 | Property),family = nbinom1,data = data)
m_competion2 = glmmTMB(Eucalyptus_seedlings ~ competition + (1 | Property),family = nbinom2,data = data)
AIC(m_competion1,m_competion2)

summary(m_competion1)

# check for vif

vif_cover
vif_life
vif_where
vif_boss

# Plots

data_long_cover <- data %>%
  pivot_longer(cols = c(grass_cover, herb_cover, fern_cover, shrub_cover), names_to = "plant_type", values_to = "cover")

ggplot(data_long_cover, aes(x = plant_type, y = cover, color = plant_type)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +  
  ylab("Cover (%)") +
  xlab("Plant type") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_long_cover, aes(x = plant_type, y = cover, fill = plant_type)) +
  geom_boxplot(alpha = 0.7) +       
  xlab("Plant type") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_long_cover %>% filter(plant_type == "grass_cover"), 
       aes(x = cover, y = Eucalyptus_seedlings)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkgreen") +  
  ylab("Eucalyptus seedlings") +
  xlab("Grass cover") +
  ggtitle("Relationship between Grass cover and Eucalyptus seedlings") +
  theme_minimal()

