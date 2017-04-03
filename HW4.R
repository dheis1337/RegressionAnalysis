library(faraway)
library(ggplot2)


# 3)  Using the teengambdata, fit a model with gambleas the response and the other
# variables as predictors.
data("teengamb")
fit <- lm(gamble ~ ., data = teengamb)

# (a) Which variables are statistically significant at the 5% level?
summary(fit)

# (b) What interpretation should be given to the coefficient for sex?


# (c) Fit a model with just income as a predictor and use an F-test to compare it to
# the full model
lil.fit <- lm(gamble ~ income, data = teengamb)
anova(lil.fit, fit)


# Using the sat data:
data(sat)
#   (a) Fit a model with total sat score as the response and expend, ratio and
# salary as predictors. Test the hypothesis that ?? salary = 0. 
sat.fit <- lm(total ~ expend + ratio + salary, data = sat)
lil.sat <- lm(total ~ expend + ratio, data = sat)
anova(lil.sat, sat.fit)

# Test the hypothesis that ?? salary = ?? ratio = ?? expend = 0.
nullmod <- lm(total ~ 1, data = sat)
anova(nullmod, sat.fit)

# Do any of these predictors have an effect on the response?
summary(sat.fit)


# (b) Now add takers to the model. Test the hypothesis that ?? takers = 0. Compare
# this model to the previous one using an F-test. 
sat.fit2 <- lm(total ~ expend + ratio + salary + takers, data = sat)
anova(sat.fit, sat.fit2)

# Demonstrate that the F-test and t-test here are equivalent
names(summary(sat.fit))



sat.fit$coefficients / sat.fit$residuals

summary(sat.fit2)
(summary(sat.fit2)$coefficients[, 1] / summary(sat.fit2)$coefficients[, 2])^2







