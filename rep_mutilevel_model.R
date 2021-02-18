# Jame Fanjul
# Multilevel Model
# Nested hierarchies

# Clean memory
rm(list=ls())
dev.off()

library(ggplot2)

# Working directory
#\""~
setwd("C:/YOUR PATH")

## F Distribution
# Quantiles
q_95=qf(0.05,2,2,lower.tail=F)
q_95

#Percentiles
pf(q_95,df1 = 2,df2 = 2)

# Figure
# When the degrees of freedom tend to infinity 
# the distribution function of f becomes 1
x = seq(0,5,0.001)
plot(x,df(x,df1=20,df2=20),xlim=c(0,5),ylim=c(0,3),"l",
     col="green",xlab = "Valor F")

# You want to evaluate how the reputation of a guild affects a random sample
# of 10 companies in this sector. 6 families were interviewed for each company
# and the level of perception of reputation of the union and the selected company was measured.

# Does the guild reputation score affect the reputation of the company?
# Is any company more affected than another by the reputation of its union?
# At the lowest level of reputation of the guild, which company has the level
# higher reputation?

rep=read.csv("rep.csv",sep=";")
head(rep)

ggplot(data = rep, aes(x = rep_gre, y = rep_emp)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Fitting a linear model
summary( lm(rep_emp ~ rep_gre , data =  rep))

rep$femp=factor(rep$emp)

# Adjustment of different models for each product (categorical)
ggplot(data = rep, aes(x = rep_gre, y = rep_emp, color = femp)) + 
  geom_point() + stat_smooth(method = 'lm')


# Fitting a model with only intercepts, without covariate
# It is used to later compare it with a model with covariates and see if there is a contribution
# significant of the covariate
mn1=lmer(rep_emp~1+(1|emp), REML = FALSE, data = rep)
summary(mn1)
coe1=coef(mn1)
coe1

# Fitting a mixed model: fixed effect of slope and reputation intercept
# union (rep_gre) where the company nests the family factor, for whose regression lines
# a random factor is assumed

mn2=lmer(rep_emp~ rep_gre + (rep_gre|emp), REML = FALSE, data = rep)

# Variance-covariance matrix (covariance)
as.data.frame(VarCorr(mn2))

# Extract fixed coefficients
fixef(mn2)


# Does the guild reputation score affect the reputation of the company?

anova(mn1,mn2)

# which
# Is any company more affected than another by the reputation of its union?
# At the lowest level of reputation of the guild, which company has the level
# higher reputation?

# Extract deviations from a - alpha and - beta
re=as.data.frame(ranef(mn2))
re$vt=re$condval/re$condsd
re