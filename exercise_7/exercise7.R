set.seed(12)

# Δημιουργία 200 τυχαίων τιμών για x1 από κανονική κατανομή (mean=10, sd=3)
x1 = rnorm(200, 10, 3)

# Δημιουργία κατηγορικής μεταβλητής group με δύο επίπεδα "A" και "B"
group = as.factor(sample(c("A", "B"), 200, replace=T))

# Δημιουργία εξαρτημένης μεταβλητής y ως γραμμικός συνδυασμός x1 + θόρυβος
y = 0.5*x1 + rnorm(200, 0, 4)

# Προσθήκη επίδρασης του group "A" στο y (+ περίπου 2 με τυχαίο θόρυβο)
y[group=="A"] = y[group=="A"] + rnorm(length(y[group=="A"]), 2, 1)

# Δημιουργία διαφόρων γραμμικών μοντέλων με διαφορετικούς συνδυασμούς predictors
m1 = lm(y ~ x1 * group)  # main effects + interaction
m2 = lm(y ~ x1 + group)  # μόνο main effects
m3 = lm(y ~ x1)          # μόνο x1
m4 = lm(y ~ group)       # μόνο group
m5 = lm(y ~ 1)           # null model (μόνο intercept)

# Βάζουμε όλα τα μοντέλα σε λίστα για εύκολη επεξεργασία
mlist = list(m1, m2, m3, m4, m5)

# Υπολογισμός AIC για κάθε μοντέλο και αποθήκευση σε πίνακα
AICTab = AIC(m1, m2, m3, m4, m5)

# Προσθήκη log-likelihood για κάθε μοντέλο στον πίνακα
AICTab$logLik = unlist(lapply(mlist, logLik))

# Ταξινόμηση των μοντέλων στον πίνακα με βάση το μικρότερο AIC (καλύτερο μοντέλο)
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]

# Υπολογισμός διαφοράς AIC (ΔAIC) από το καλύτερο μοντέλο
AICTab$delta = round(AICTab$AIC- min(AICTab$AIC), 2)

# Υπολογισμός των unnormalized weights (σχετική πιθανότητα για κάθε μοντέλο)
lh = exp(-0.5*AICTab$delta)

# Κανονικοποίηση weights ώστε να αθροίζουν σε 1 και αποθήκευση στον πίνακα
AICTab$w = round(lh/sum(lh), 2)
AICTab
