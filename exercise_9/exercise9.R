plants = read.csv(file="exercise_4/alpineplants.csv")

plants = na.omit(plants) # Αφαιρούμε κενές τιμές (NAs) για να είναι τα μοντέλα συγκρίσιμα
plants = as.data.frame(scale(plants)) # Κάνουμε z-transformation για να έχουν οι μεταβλητές μέση τιμή 0 και τυπική απόκλιση 1
# όλες οι μεταβλητές έχουν mean = 0 και sd = 1

round(colMeans(plants), 2) # Υπολογίζει τη μέση τιμή κάθε στήλης του plants
round(apply(plants, 2, sd), 2)

m1 = lm(Carex.bigelowii~ snow + min_T_winter + soil_moist, data=plants) #Μοντέλο 1 (απλή πολλαπλή παλινδρόμηση)
m2a = lm(min_T_winter~ snow, data=plants) # πόσο το χιόνι επηρεάζει τη θερμοκρασία εδάφους
m2b = lm(soil_moist~ snow, data=plants) # πόσο το χιόνι επηρεάζει την υγρασία του εδάφους
m2c = lm(Carex.bigelowii~ min_T_winter + soil_moist, data=plants) # επίδραση των ενδιάμεσων μεταβλητών στο φυτό

summary(m1)

summary(m1)$coef[2,1] +
summary(m1)$coef[3,1]*cor(plants$snow, plants$min_T_winter, "pairwise") +
summary(m1)$coef[4,1]*cor(plants$snow, plants$soil_moist, "pairwise")

cor(plants$snow, plants$Carex.bigelowii, "pairwise")

summary(m2a)$coef
summary(m2b)$coef
summary(m2c)$coef

install.packages("piecewiseSEM")
library(piecewiseSEM)
m2 = psem(lm(soil_moist~snow, data=plants), # Υπολογίζει πώς το χιόνι επηρεάζει την υγρασία
lm(min_T_winter~snow, data=plants), # Υπολογίζει πώς το χιόνι επηρεάζει τη θερμοκρασία χειμώνα
lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants), # Υπολογίζει πώς η θερμοκρασία και η υγρασία επηρεάζουν το φυτό
data=plants)
# Το psem() παίρνει αυτά τα μικρά μοντέλα και τα συνδυάζει σε ένα δομημένο μοντέλο σχέσεων

summary(m2)
plot(m2)

m2b = psem(lm(soil_moist~snow, data=plants),
lm(min_T_winter~snow, data=plants),
lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants),
min_T_winter %~~% soil_moist, # Ας πούμε ότι η θερμοκρασία και η υγρασία είναι συσχετισμένες, αλλά δεν ξέρουμε ποιος επηρεάζει ποιον
data=plants)
summary(m2b)

# R-squared λέει πόσο καλά εξηγείται η κάθε μεταβλητή από το μοντέλο
# Τα coefficients λένε πόσο ισχυρή είναι κάθε σχέση, και τα stars (*) αν είναι σημαντική

m3 = psem(lm(soil_moist~snow, data=plants),
lm(min_T_winter~snow, data=plants),
lm(Carex.bigelowii~min_T_winter, data=plants),
min_T_winter %~~% soil_moist,
data=plants)

summary(m3)

# Φτιάχνουμε πρώτα τα component models
lm1 <- lm(soil_moist ~ snow, data = plants)
lm2 <- lm(min_T_winter ~ snow, data = plants)
lm3 <- lm(Carex.bigelowii ~ min_T_winter, data = plants)

# Τώρα φτιάχνουμε το SEM
m3 <- psem(
  lm1,
  lm2,
  lm3,
  min_T_winter %~~% soil_moist
)

# Τα επιμέρους AIC
AIC(lm1)
AIC(lm2)
AIC(lm3)

# Το συνολικό AIC
summary(m3)$AIC

# EXERCISE: Repeat the analyses above for Thalictrum alpinum instead of Carex

m1_thal <- lm(Thalictrum.alpinum ~ snow + min_T_winter + soil_moist, data = plants)
summary(m1_thal)

m2_thal <- psem(
  lm(soil_moist ~ snow, data = plants),
  lm(min_T_winter ~ snow, data = plants),
  lm(Thalictrum.alpinum ~ min_T_winter + soil_moist, data = plants),
  data = plants
)
summary(m2_thal)

m2b_thal <- psem(
  lm(soil_moist ~ snow, data = plants),
  lm(min_T_winter ~ snow, data = plants),
  lm(Thalictrum.alpinum ~ min_T_winter + soil_moist, data = plants),
  min_T_winter %~~% soil_moist,
  data = plants
)
summary(m2b_thal)

lm1 <- lm(soil_moist ~ snow, data = plants)
lm2 <- lm(min_T_winter ~ snow, data = plants)
lm3 <- lm(Thalictrum.alpinum ~ min_T_winter, data = plants)

m3_thal <- psem(
  lm1,
  lm2,
  lm3,
  min_T_winter %~~% soil_moist
)
summary(m3_thal)

AIC(m2b_thal, m3_thal)

# EXERCISE: Can you think of other potential models that can be tested? Are there other important environ-
#mental variables? Start by drawing the competing models as directed graphs (on paper). Fit and compare
#the models, and interpret the results

# Παράδειγμα για νέο μοντέλο
m_new <- psem(
  lm(soil_moist ~ snow + light, data = plants),
  lm(min_T_winter ~ snow + altitude, data = plants),
  lm(Carex.bigelowii ~ min_T_winter + soil_moist + light, data = plants),
  min_T_winter %~~% soil_moist,
  data = plants
)

summary(m_new)
AIC(m2b, m3, m_new)
