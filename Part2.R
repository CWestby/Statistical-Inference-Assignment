pmtooth <- data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

ggplot(ToothGrowth, aes(x = len)) +
  geom_histogram(binwidth = 2)
ggplot(ToothGrowth, aes(x = supp, y = len, col = dose)) +
  geom_point() + facet_grid(~ dose)
ggplot(ToothGrowth, aes(x = dose, y = len, col = supp)) +
  geom_point() + facet_grid(~ supp)

t.test(len ~ supp, ToothGrowth)

#Can say with 95% confidence that -0.1710156 to 7.5710156 units of length 
#in cells is a result of the different supplement. If hypthesis test was run
#could not reject the null hypthesis that there is no difference in means 
#between supplements. p-value not below 0.05.

ToothGrowth0.5 <- subset (ToothGrowth, dose %in% c(0.5, 1.0)) 
t.test(len ~ dose, ToothGrowth0.5)
ToothGrowth1.0 <- subset (ToothGrowth, dose %in% c(0.5, 2.0)) 
t.test(len ~ dose, ToothGrowth1.0)
ToothGrowth2.0 <- subset (ToothGrowth, dose %in% c(1.0, 2.0)) 
t.test(len ~ dose, ToothGrowth2.0)

#Can reject the null hypothesis that there is no difference between cell
#growth in guinea pigs. All p-values well below 0.05. Variability in cell
#growth not due to random chance but due to doses.

#Relying on Central Limit Theorem and normal distributions