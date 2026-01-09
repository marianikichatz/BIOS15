install.packages("lme4")
install.packages("lmerTest")

library(lme4)  
library(lmerTest)
library(tidyverse)
library(ggeffects)
library(car)
library(MuMIn)
library(cowplot)

# load the data
data = read.table(file = "exams/penstemon-1.txt", header = TRUE) 

data = na.omit(data) # remove missing values

# convert categorical variables to factors
data$Pop <- as.factor(data$Pop)

head(data) # view the first few rows of the data
summary(data) # view a summary of the data

# histogram of fitness values
ggplot(data, aes(x = fitness)) +
  geom_histogram(bins = 30, fill = "pink", color = "black") + 
  theme_minimal() +
  labs(title = " Histogram of fitness values", x = "Fitness", y = "Count")

data$log_fitness <- log(data$fitness + 1) # log transform fitness values

# scale continuous predictor variables
data_scaled <- data %>%
  mutate(
    height_sc = scale(height),
    flwsize_sc = scale(flwsize),
    InflorLen_sc = scale(InflorLen), 
    tscent_sc = scale(tscent),       
    openflws_sc = scale(openflws),   
    FlwDate_sc = scale(FlwDate)     
  )

# fit the global linear mixed effects model
m_global <- lmer(log_fitness ~ Pop + height_sc + flwsize_sc + 
                 InflorLen_sc + tscent_sc + openflws_sc + FlwDate_sc + 
                 (1|Block), data = data_scaled)

summary(m_global) # view the summary of the model

# morphology hypothesis
# fit the morphology model
m_morph <- lmer(log_fitness ~ Pop + height_sc + flwsize_sc + InflorLen_sc  + (1|Block), 
                data = data_scaled)

# attraction hypothesis
# fit the attraction model
m_attr <- lmer(log_fitness ~ Pop + tscent_sc + openflws_sc + FlwDate_sc + (1|Block), 
               data = data_scaled)

# compare models using AICc
model_list <- list(Global = m_global, Morphology = m_morph, Attraction = m_attr)
model.sel(model_list)

# final model 
m_final <- lmer(log_fitness ~ Pop + openflws_sc + height_sc + flwsize_sc + (1|Block), 
                data = data_scaled)
summary(m_final)

# compare final model to other models
model.sel(m_final, m_global, m_morph, m_attr) # AICc comparison
anova(m_final, m_global, m_morph, m_attr) # anova comparison

# interaction model

# fit the interaction model
m_inter <- lmer(log_fitness ~ Pop * openflws_sc + Pop * height_sc + Pop * flwsize_sc + (1|Block), 
                data = data_scaled)

summary(m_inter) 
anova(m_final, m_inter) # anova comparison
model.sel(m_final, m_inter) # AICc comparison
model.sel(m_final, m_inter,m_attr)

# check for multicollinearity
vif(m_global)
vif(m_morph)
vif(m_attr)
vif(m_final) 

 # r2 for final model
r.squaredGLMM(m_final)

# Plots

# function to create prediction plots
plot_prediction <- function(model, predictor_var, data_scaled, title_text, plot_color) {
  pred_data <- ggpredict(model, terms = predictor_var, ci.lvl = 0.98) # get predictions
  
  ggplot(pred_data, aes(x = x, y = predicted)) + # base plot
    geom_point(data = data_scaled, # add raw data points
               aes(x = .data[[predictor_var]], y = log_fitness),
               inherit.aes = FALSE, alpha = 0.15, color = "gray50") +
    geom_line(color = plot_color, size = 1) + # add prediction line
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = plot_color) + # add confidence interval
    theme_minimal() +
    labs(title = title_text, x = paste0(predictor_var, " (scaled)"), y = "Log(Fitness)") +
    theme(text = element_text(size = 14))
}

# attraction model plots
# plot for openflws_sc
plot_attr_openflws <- plot_prediction(m_attr, "openflws_sc", data_scaled, 
                                      "Effect of Number of Open Flowers on Fitness", "darkgreen")

# plot for tscent_sc
plot_attr_tscent <- plot_prediction(m_attr, "tscent_sc", data_scaled,
                                   "Effect of Flowering Scent on Fitness", "deeppink4")

# plot for FlwDate_sc
plot_attr_FlwDate <- plot_prediction(m_attr, "FlwDate_sc", data_scaled,
                                    "Effect of Flowering Date on Fitness", "mediumpurple4")


# Bar plot of effect sizes from attraction model
plot_data_attr <- data.frame(
  Factor = c("Floral Scent", "Open Flowers", "Flowering Date"),
  Power  = c(fixef(m_attr)["tscent_sc"], 
             fixef(m_attr)["openflws_sc"], 
             fixef(m_attr)["FlwDate_sc"]))

bars_attr <- ggplot(plot_data_attr, aes(x = Factor, y = Power, fill = Factor)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "Effect Sizes from Attraction Model", 
       x = "Predictor", 
       y = "Effect Size (Estimate)") +
  theme(text = element_text(size = 14)) +
  scale_fill_brewer(palette = "PuRd")

plot_grid( bars_attr ,plot_attr_openflws, plot_attr_tscent, plot_attr_FlwDate, ncol = 2,
           labels = c("A", "B", "C", "D"))

# final model plots
# plot for openflws_sc
plot_openflws <- plot_prediction(m_final, "openflws_sc", data_scaled, 
                                  "Effect of Number of Open Flowers on Fitness", "skyblue")

# plot for height_sc
plot_height <- plot_prediction(m_final, "height_sc", data_scaled,
                              "Effect of Plant Height on Fitness", "pink")

# plot for flwsize_sc
plot_flwsize <- plot_prediction(m_final, "flwsize_sc", data_scaled,
                               "Effect of Flower Size on Fitness", "plum")

# Bar plot of effect sizes from final model

# create data frame for plotting
plot_data_final <- data.frame(
  Factor = c("Open Flowers", "Plant Height", "Flower Size"), # predictor names
  Power  = c(fixef(m_final)["openflws_sc"], # effect sizes
             fixef(m_final)["height_sc"], 
             fixef(m_final)["flwsize_sc"]))

bars_final <- ggplot(plot_data_final, aes(x = Factor, y = Power, fill = Factor)) + 
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal() +
  labs(title = "Effect Sizes from Final Model", x = "Predictor", y = "Effect Size (Estimate)") +
  theme(text = element_text(size = 14)) +
  scale_fill_brewer(palette = "Set3") 

plot_grid( bars_final, plot_openflws, plot_flwsize, plot_height, ncol = 2,
           labels = c("A", "B", "C", "D"))

# fitness per population

# predictions for populations
pred_pop <- ggpredict(m_final, terms = "Pop")

ggplot(pred_pop, aes(x = x, y = predicted, color = x)) +
  geom_point(size = 4) +                       
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1) + 
  theme_minimal() +
  labs(title = "Predicted Fitness by Population",
       x = "Population",
       y = "Predicted log(Fitness)") +
  theme(text = element_text(size = 14), legend.position = "none") +
  scale_color_brewer(palette = "PuRd")

# for tables

print(summary(m_final)$coefficients) # coefficients

betas_f <- fixef(m_final) # fixed effects coefficients

percent_change_f <- (exp(betas_f) - 1) * 100 # percent change in fitness

# results table
results_table_f <- data.frame(
  Predictor = names(betas_f),
  Estimate = round(betas_f, 4),           
  Percent_Change = round(percent_change_f, 2) 
)

print(results_table_f)


print(summary(m_inter)$coefficients) # coefficients

betas_int <- fixef(m_inter) # fixed effects coefficients

percent_change_int <- (exp(betas_int) - 1) * 100 # percent change in fitness

# results table
results_table_int <- data.frame(
  Predictor = names(betas_int),
  Estimate = round(betas_int, 4),           
  Percent_Change = round(percent_change_int, 2) 
)

print(results_table_int)

