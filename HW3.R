# Homework 3 
library(faraway)
library(ggplot2)
data("teengamb")

# # 1) The dataset teengamb concerns a study of teenage gambling in Britain. Fit a regression
# model with the expenditure on gambling as the response and the sex, status, income
# and verbal score as predictors. Present the output. 
fit <- lm(gamble ~ sex + status + income + verbal, data = teengamb)

# (a) What percentage of variation in the response is explained by these predictors?
summary(fit)$r.squared

# (b) Which observation has the largest (positive) residual? Give the case number.
max(residuals(fit))
# (c) Compute the mean and median of the residuals.
median(residuals(fit))
mean(residuals(fit))
# (d) Compute the correlation of the residuals with the fitted values.
cor(x = residuals(fit), y = fitted(fit))
# (e) Compute the correlation of the residuals with the income.
cor(x = residuals(fit), y = teengamb$income)
# (f) For all other predictors held constant, what would be the difference in predicted
# expenditure on gambling for a male compared to a female?
summary(fit)

# Fit a polynomial in x for predicting y. Compute in two ways-by lm ( ) and by
# using the direct calculation described in the chapter. At what degree of polynomial
# does the direct calculation method fail? (Note the need for the I ( ) function in fitting
# the polynomial, that is, lm(y~x+I(x^2))
x <- 1:20
y <- x + rnorm(20)

lm(y ~ x)

test <- cbind(x, y )

xtxi <- solve(t(x) %*% x)
xtxi %*% t(x) %*% y

mat <- as.matrix(x)
mat <- cbind(mat, x^2)

xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

mat <- as.matrix(x)
mat <- cbind(mat, x^3)

xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

mat <- as.matrix(x)
mat <- cbind(mat, x^4)

xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

mat <- as.matrix(x)
mat <- cbind(mat, x^5)

xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

mat <- as.matrix(x)
mat <- cbind(mat, x^6)

xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

lm(y ~ x + I(x^2)) 
lm(y ~ x + I(x^2) + I(x^3)) 
lm(y ~ x + I(x^2) + I(x^3) + I(x^4)) 
lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)) 
lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6)) 
lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7)) 



# The dataset prostate comes from a study on 97 men with prostate cancer who were due
# to receive a radical prostatectomy. Fit a model with lpsa as the response and l cavol as
# the predictor. Record the residual standard error and the R 2 . Now add lweight, svi,
# lpph, age, l cp, pgg45 and gleason to the model one at a time. For each model record
# the residual standard error and the R 2 . Plot the trends in these two statistics
data("prostate")
pro.fit <- lm(lpsa ~ lcavol, data = prostate)
trend <- data.frame(RSE = summary(pro.fit)$sigma, R2 = summary(pro.fit)$r.squared)

pro.fit <- lm(lpsa ~ lcavol + lweight, data = prostate)
trend <- rbind(c(summary(pro.fit)$sigma, summary(pro.fit)$r.squared), trend)

pro.fit <- lm(lpsa ~ lcavol + lweight + svi, data = prostate)
trend <- rbind(c(summary(pro.fit)$sigma, summary(pro.fit)$r.squared), trend)

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph, data = prostate)
trend <- rbind(c(summary(pro.fit)$sigma, summary(pro.fit)$r.squared), trend)

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph + age, data = prostate)
trend <- rbind(c(summary(pro.fit)$sigma, summary(pro.fit)$r.squared), trend)

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp, data = prostate)
trend <- rbind(c(summary(pro.fit)$sigma, summary(pro.fit)$r.squared), trend)

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45, data = prostate)
trend <- rbind(c(summary(pro.fit)$sigma, summary(pro.fit)$r.squared), trend)

ggplot(trend, aes(x = RSE, y = R2)) + 
  geom_point()

# 6.a) Fit a regression model with a taste as the response and the three chemical 
# contents as predictors. Report the values of the coefficients. 
data("cheddar")
cheese <- lm(taste ~ ., data = cheddar)
cheese$coefficients

# b) Compute the correlation between the fitted values and the response. Square it.
# Identify where this value appears in the regression output. 
correl <- cor(x = fitted(cheese), y = cheddar$taste)
correl^2

# c) Fit the same regression model but without an intercept term. What is the value
# of the R-squared reported in the output. Compute a more reasonable measure of 
# the goodness of fit for this example. 
no.int <- lm(taste ~ . - 1, data = cheddar)
cor(fitted(no.int), cheddar$taste)^2

