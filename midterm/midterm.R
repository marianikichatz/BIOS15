install.packages("glmmTMB")
library(glmmTMB)
library(car)
library(tidyverse)

data = read.csv(file = "midterm/exam2023_data-2.csv")

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

m_cover1 = glmmTMB(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover + (1 | Property),family = nbinom1, data = data)
m_cover2 = glmmTMB(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover + (1 | Property),family = nbinom2, data = data)
AIC(m_cover1,m_cover2)

vif_cover = lm(Eucalyptus_seedlings ~ grass_cover + graminoid_cover + herb_cover + fern_cover + shrub_cover + Euc_canopy_cover, data = data)

summary(m_cover1)

# diff life circles

data$annual_cover = data$ExoticAnnualGrass_cover + data$ExoticAnnualHerb_cover

data$perennial_cover = data$ExoticPerennialGrass_cover + data$ExoticPerennialHerb_cover +
  data$NativePerennialGrass_cover +
  data$NativePerennialGraminoid_cover +
  data$NativePerennialHerb_cover +
  data$NativePerennialFern_cover +
  data$ExoticShrub_cover +
  data$NativeShrub_cover

m_life1 = glmmTMB(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover + (1 | Property), family = nbinom1, data = data)
m_life2 = glmmTMB(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover + (1 | Property), family = nbinom2, data = data)
AIC(m_life1, m_life2)
 
vif_life = lm(Eucalyptus_seedlings ~ annual_cover + perennial_cover + Euc_canopy_cover, data = data)

summary(m_life1)

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

m_where1 = glmmTMB(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover + (1 | Property),family = nbinom1, data = data)
m_where2 = glmmTMB(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover + (1 | Property),family = nbinom2, data = data)
AIC(m_where1, m_where2)

vif_where = lm(Eucalyptus_seedlings ~ native_cover + exotic_cover + Euc_canopy_cover, data = data)

summary(m_where1)

# diff origin and type

data$native_grass = data$NativePerennialGrass_cover + data$NativePerennialGraminoid_cover
data$native_herb  = data$NativePerennialHerb_cover
data$native_fern  = data$NativePerennialFern_cover
data$native_shrub = data$NativeShrub_cover

data$exotic_grass = data$ExoticAnnualGrass_cover + data$ExoticPerennialGrass_cover
data$exotic_herb  = data$ExoticAnnualHerb_cover + data$ExoticPerennialHerb_cover
data$exotic_shrub = data$ExoticShrub_cover

m_boss1 = glmmTMB(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover + (1 | Property), family = nbinom1, data = data)
m_boss2 = glmmTMB(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover + (1 | Property), family = nbinom2, data = data)
AIC(m_boss1,m_boss2)

vif_boss = lm(Eucalyptus_seedlings ~ native_grass + native_herb + native_fern + native_shrub +
  exotic_grass + exotic_herb + exotic_shrub + Euc_canopy_cover, data = data)

summary(m_boss1)

# competition between seedlings and cover from all the plants

data$competition = data$grass_cover + data$graminoid_cover + data$herb_cover + data$fern_cover + data$shrub_cover

m_competion1 = glmmTMB(Eucalyptus_seedlings ~ competition + (1 | Property),family = nbinom1,data = data)
m_competion2 = glmmTMB(Eucalyptus_seedlings ~ competition + (1 | Property),family = nbinom2,data = data)
AIC(m_competion1,m_competion2)

summary(m_competion1)

# check for vif

vif(vif_cover)
vif(vif_life)
vif(vif_where)
vif(vif_boss)

# Plots

data_long_cover <- data %>%
  pivot_longer(cols = c(grass_cover, herb_cover, fern_cover, shrub_cover), names_to = "plant_type", values_to = "cover")

ggplot(data_long_cover, aes(x = cover, 
                      y = Eucalyptus_seedlings, 
                      color = plant_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, size = 1.1) +
  labs(
    x = "Plant cover (%)",
    y = "Eucalyptus seedlings abundance",
    ccolor = "Plant type",
    title = "Relationship between plant cover and Eucalyptus seedlings"
  ) +
  theme_minimal(base_size = 14)
