#There are two sets of patients, in pt ID, ones 
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

#Additionally, the final two numbers, OP-SN and OP-FH, 
#do they have any association with the individual 
#measurements of ANB, Wits or AF-BF.

#That's it basically. Let me know what else I can tell 
#you or if we should have a phone chat or whatever.

library(regclass)
ceph <- read.csv("/Users/gregorymatthews/Downloads/ceph_study.csv")

ceph[ceph$U1.NF > 100,c("OB","ANB","Wits","Jacobi","U1.NF","U6.NF","OP.FH","OP.SN")] <- c(3, 5.8, 1.4, 1.8, 31.4, 27.4, 0.5, 16.1)
# 
# OB = 3.0
# ANB = 5.8
# Wits = 1.4
# Jacobi = 1.8
# U1-NF = 31.4
# U6-NF = 27.4
# OP-FH = 0.5
# OP-SN = 16.1

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

summary(mod)
ceph[139,]
boxplot(ceph$OB)
boxplot(ceph$U1.NF)
boxplot(ceph$U6.NF)
summary(mod)
VIF(mod)

coef(mod)["U1.NF"]
confint(mod, "U1.NF")

plot(mod)
