# There are two sets of patients, in pt ID, ones 
# prefix "I" and one "d". That just represents 
# different sources of the patients, 
# demographically they should be identical, 
# so they should be treated as a single group.

# I am interested, in terms of my headers, in 
# the following: Does the measurement OB have an 
# association with U6-NF as it relates to U1-NF. 
# The clinical indications are that as U6-NF is 
# increased, especially without similar increases 
# in U1-NF, OB should decrease.

# Additionally, the final two numbers, OP-SN and OP-FH, 
# do they have any association with the individual 
# measurements of ANB, Wits or AF-BF.

library(tidyverse)
library(regclass)
ceph <- read.csv("ceph_study.csv")

boxplot(ceph$OB)
boxplot(ceph$U1.NF)
boxplot(ceph$U6.NF)
ceph[139,]

# Why in the world would you do it like this
ceph[ceph$U1.NF > 100, c("OB","ANB","Wits","Jacobi","U1.NF","U6.NF","OP.FH","OP.SN")] <- c(3, 5.8, 1.4, 1.8, 31.4, 27.4, 0.5, 16.1)

mod_full <- glm(
  OB ~ U1.NF * U6.NF,
  ceph,
  family = gaussian()
)

mod_red <- glm(
  OB ~ U1.NF + U6.NF,
  ceph,
  family = gaussian()
)

#Testing for interaction 
anova(mod_red,mod_full)

plot(mod_full)
VIF(mod_full) # Not ideal but

summary(mod_full)

coef(mod_full)["U6.NF"]
confint(mod_full, "U6.NF")

heat_grid <- expand.grid(
  U1.NF = seq(min(ceph$U1.NF), max(ceph$U1.NF), length.out = 15),
  U6.NF = seq(min(ceph$U6.NF), max(ceph$U6.NF), length.out = 15)
)

heat_grid$pred_OB <- predict(mod_full, newdata = heat_grid, type = "response")

heat_grid |>
  ggplot(aes(x = U6.NF, y = U1.NF, z = pred_OB)) +
  geom_tile(aes(fill = pred_OB)) +
  geom_contour(color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Predicted OB",
       x = "U6.NF", y = "U1.NF", fill = "Predicted OB") +
  theme_minimal()

library(emmeans)
emtrends(mod_full, ~ U1.NF, var = "U6.NF")

ceph |>
  mutate(`U1-NF Group` = cut(U1.NF, breaks = quantile(U1.NF, probs = seq(0, 1, 0.25)), include.lowest = TRUE)) %>%
  ggplot(aes(x = U6.NF, y = OB, color = `U1-NF Group`)) +
  geom_smooth(span = 1, se = F) +
  geom_point() + 
  labs(title = "Effect of U6-NF on OB across U1-NF Levels")

library(interactions)
interact_plot(mod_full, pred = U6.NF, modx = U1.NF)


# Now Correlations

p1_cor_value <- cor(ceph$OP.SN, ceph$ANB)
p2_cor_value <- cor(ceph$OP.SN, ceph$Wits)
p3_cor_value <- cor(ceph$OP.SN, ceph$Jacobi)
p4_cor_value <- cor(ceph$OP.FH, ceph$ANB)
p5_cor_value <- cor(ceph$OP.FH, ceph$Wits)
p6_cor_value <- cor(ceph$OP.FH, ceph$Jacobi)

p1 <- ggplot(ceph, aes(x = OP.SN, y = ANB)) + 
  geom_point() + 
  geom_text(x = min(ceph$OP.SN), y = max(ceph$ANB), 
            label = paste("r =", round(p1_cor_value, 2)), 
            hjust = 0, vjust = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OP-SN vs ANB")

p2 <- ggplot(ceph, aes(x = OP.SN, y = Wits)) + 
  geom_point() + 
  geom_text(x = min(ceph$OP.SN), y = max(ceph$Wits), 
            label = paste("r =", round(p2_cor_value, 2)), 
            hjust = 0, vjust = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OP-SN vs Wits")

p3 <- ggplot(ceph, aes(x = OP.SN, y = Jacobi)) + 
  geom_point() + 
  geom_text(x = min(ceph$OP.SN), y = max(ceph$Jacobi), 
            label = paste("r =", round(p3_cor_value, 2)), 
            hjust = 0, vjust = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OP-SN vs Jacobi")

p4 <- ggplot(ceph, aes(x = OP.FH, y = ANB)) + 
  geom_point() + 
  geom_text(x = min(ceph$OP.FH), y = max(ceph$ANB), 
            label = paste("r =", round(p4_cor_value, 2)), 
            hjust = 0, vjust = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OP-FH vs ANB")

p5 <- ggplot(ceph, aes(x = OP.FH, y = Wits)) + 
  geom_point() + 
  geom_text(x = min(ceph$OP.FH), y = max(ceph$Wits), 
            label = paste("r =", round(p5_cor_value, 2)), 
            hjust = 0, vjust = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OP-FH vs Wits")

p6 <- ggplot(ceph, aes(x = OP.FH, y = Jacobi)) + 
  geom_point() + 
  geom_text(x = min(ceph$OP.FH), y = max(ceph$Jacobi), 
            label = paste("r =", round(p6_cor_value, 2)), 
            hjust = 0, vjust = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OP-FH vs Jacobi")

library(patchwork)
(p1 + p2 + p3) / (p4 + p5 + p6)


# We observed a negative association between U6-NF and OB across the patient study population. When modeling OB based on both U1-NF and U6-NF measurements, we found that, for all values of U1-NF, an increase in U6-NF is associated with a decrease in OB. Specifically, for the average U1-NF value, a 1 mm increase in U6-NF corresponds to a 0.45 mm reduction in OB. As U1-NF increased, the decrease in OB associated with higher U6-NF values became more pronounced. To assess this further, we examined the slopes at the four quantiles of U1-NF, as well as +/- 1 standard deviation from the mean. In all cases, the relationship between OB and U6-NF remained negative, indicating a consistent pattern.

# Regarding the relationships between OP measures (OP.SN and OP-FH) and the other measurements, we found that ANB showed the weakest correlation with both OP measures. On the other hand, Wits had a moderate negative correlation (around -0.3) with both OP.SN and OP-FH. In contrast, Jacobi exhibited a stronger, positive correlation with both OP measures. For both Wits and Jacobi, the correlations were stronger with OP-FH than with OP.SN.