#Analysis of variance (ANOVA)

# aim of our ANOVA analysis is to partition the variance in 
# our response variable into a set of additive components

set.seed(100)
groups = as.factor(rep(c("Low", "Medium", "High"), each=50))
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))

boxplot(x ~ groups, las = 1, xlab = "", ylab = "Body size (g)", main = "Body Size by Group", width = c(0.4, 0.4, 0.4))

box_pos <- 1:length(levels(groups))
stripchart(x ~ groups, vertical = TRUE, method = "jitter", pch = 21, col = "pink", add = TRUE, at = box_pos - 0.5)

m = lm(x~groups)
anova(m)

SS_T = 319.97+1200.43
SS_T/(150-1)
319.97/SS_T

summary(m)

groups = factor(groups, levels=c("Low", "Medium", "High"))
m = lm(x~groups)
summary(m)

m = lm(x~groups-1)
summary(m)$coef

confint(m)


# Two-way ANOVA
install.packages("dplyr")
install.packages("broom")
install.packages("gt")

library(dplyr)
library(ggplot2)
library(broom)
library(gt)



dat = read.csv("exercise_3/butterflies.csv")
names(dat)

dat = dat %>%
  mutate(
    MaternalHost = factor(MaternalHost),
    LarvalHost = factor(LarvalHost)
  )
head(dat)

summary_stats = dat %>%
  group_by(MaternalHost, LarvalHost) %>%
  summarise(
    mean_development_time = mean(DevelopmentTime),
    se_development_time = sd(DevelopmentTime)/sqrt(n()),
    .groups = "drop"
  )

summary_stats

summary_stats_table <- summary_stats %>%
  rename(`Maternal Host` = MaternalHost,
    `Larval Host` = LarvalHost,
    `Mean Development Time (days)` = mean_development_time,
    `SE (days)` = se_development_time) %>%
  gt() %>%
  tab_header(title = "Table 1: Effects of Host Plants on Larval Development Time",
    subtitle = "Mean Values and Standard Errors by Maternal and Larval Host Plants") %>%
  fmt_number(
    columns = c(`Mean Development Time (days)`, `SE (days)`),)

summary_stats_table
gtsave(summary_stats_table, "summary_stats_table.html")


p = ggplot(summary_stats, aes(x = LarvalHost, y = mean_development_time, color = MaternalHost, group = MaternalHost)) +
  geom_point(size = 3) +  
  geom_line() +           
  geom_errorbar(aes(ymin = mean_development_time - se_development_time, ymax = mean_development_time + se_development_time), width = 0.2) +  # error bars
  scale_color_manual(values = c("Barbarea" = "pink", "Berteroa" = "plum")) +
  ylab("Development Time (days)") +
  xlab("Larval Host Plant") +
  theme_minimal() +
  ggtitle("Larval Development Time by Maternal and Larval Host Plant")

ggsave("butterflies_plot.png", plot = p, width = 6, height = 4, dpi = 300)

anova_model = aov(DevelopmentTime ~ MaternalHost * LarvalHost, data = dat)

anova_table = broom::tidy(anova_model) %>%
  mutate(Cases = case_when(
      term == "MaternalHost" ~ "Maternal Host",
      term == "LarvalHost" ~ "Larval Host",
      term == "MaternalHost:LarvalHost" ~ "Interaction (Maternal Host : Larval Host)",
      term == "Residuals" ~ "Residuals",
      TRUE ~ term)) %>%
  select(Cases, Df = df, `Sum of Squares` = sumsq, `Mean Square` = meansq, `F value` = statistic, `p value` = p.value)

anova_table_gt = anova_table %>%
  gt() %>%
  tab_header( title = "Table 2: Two-Way ANOVA Results",
    subtitle = "Effects of Maternal and Larval Host on Development Time") 

anova_table_gt

gtsave(anova_table_gt, "anova_results_table.html")


