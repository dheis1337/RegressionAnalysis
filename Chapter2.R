library(faraway)
library(ggplot2)

# 1) The dataset teengamb concerns a study of teenage gambling in Britain.
# Fit a regression model with the expenditure on gambling as the response and d
# the sex, status, income, and verbal score as predictors. Present the output. 
data("teengamb")

# a) What percentage of variation in the response is explained by these predictors?
gamb.fit <- lm(gamble ~ sex + status + income + verbal, data = teengamb)

summary(gamb.fit)$r.squared

# b) Which observation has the largest (positive) residual? Give the case number
which(gamb.fit$residuals == max(gamb.fit$residuals))

# c) Compute the mean and median of the residuals 
mean(gamb.fit$residuals)
median(gamb.fit$residuals)

# d) Compute the correlation of the residuals with the fitted values
cor(residuals(gamb.fit), fitted(gamb.fit))

# e) Compute the correlation of the residuals with the income
cor(residuals(gamb.fit), teengamb$income)

# f) For all other predictors held constant, what would the difference in predicted
# expenditure on gambling for a male compared to a female?
gamb.fit$coefficients[2]


# 2) The dataset uswages is drawn as a sample from the Current Population Survey in 
# 1988. Fit a model with weekly wages as the response and years of education and 
# experience as predictors. Report and give a simple interpretation to the regression
# coefficient for years of education. Now fit the same model but with logged weekly 
# wages. Give an interpretation to the regression coefficient for years of education. 
# Which interpretation is more natural?
data("uswages")
wage.fit <- lm(wage ~ educ + exper, data = uswages)

wage.fit$coefficients

# For every incremental year of education, you make an additional 51 dollars per week. 
log.wage.fit <- lm(log10(wage) ~ educ + exper, data = uswages)


# 3) In this question, we investigate the relative merits of methods for computing
# the coefficients. Generate some artifical data.
x <- 1:20
y <- x + rnorm(20)

# Fit a polynomial in x for predicting y. Compute B-hat in two ways - by lm() and
# by using the direct calculation method described in the chapter. At what degree
# of polynomial does the direct calculation method fail? Note the need for the 
# I function in fitting the polynomial, that is, lm(y ~ x + I(x^2)). 
mat <- as.matrix(x)

# Degree 2
mat <- cbind(x, x^2)
xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

# Degree 3
mat <- cbind(mat, x^3)
xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

# Degree 4
mat <- cbind(mat, x^4)
xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

# Degree 5
mat <- cbind(mat, x^5)
xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

# Degree 6
mat <- cbind(mat, x^6)
xtxi <- solve(t(mat) %*% mat)
xtxi %*% t(mat) %*% y

# Lm() method
poly.fit <- lm(y ~ poly(x, degree = 21, raw = TRUE))


# The dataset prostate comes from a study on 97 men with prostate cancer who 
# were due to receive radical prostatectomy. Fit a model with lpsa as the response
# and lcavol as the predictor. Record the residual standard error and the R-squared. 
# Now add lweight, svi, lbph, age, pgg45, and gleason to the model one at a time. 
# For each model record the residual standard error and the R-squared. Plot the
# trends in the two statistics. 
data("prostate")

pro.fit <- lm(lpsa ~ lcavol, data = prostate)
pro.df <- data.frame(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared)

pro.fit <- lm(lpsa ~ lcavol + lweight, data = prostate)
pro.df <- rbind(pro.df, c(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared))

pro.fit <- lm(lpsa ~ lcavol + lweight + svi, data = prostate)
pro.df <- rbind(pro.df, c(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared))

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph, data = prostate)
pro.df <- rbind(pro.df, c(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared))

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph + age, data = prostate)
pro.df <- rbind(pro.df, c(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared))

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + pgg45, data = prostate)
pro.df <- rbind(pro.df, c(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared))

pro.fit <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + pgg45 + gleason, data = prostate)
pro.df <- rbind(pro.df, c(RSE = summary(pro.fit)$sigma, Rsquared = summary(pro.fit)$r.squared))


ggplot(pro.df, aes(x = Rsquared, y = RSE)) + 
  geom_point()

# As Rsquared increase, the standard error of the residuals decreases. 

# Thirty samples of cheddar cheese were analyzed for their content of acetic acid, 
# hydrogen sulfide and lactic acid. Each sample was tasted and scored by a panel of 
# judges and the average taste score produced. Use the cheddar data to answer
# the following: 
data("cheddar")

# a) Fit a regression model with taste as the response and the three
# chemical contents as predictors. Report the values of the regression coefficients. 
cheese <- lm(taste ~ Acetic + H2S + Lactic, data = cheddar)

cheese$coefficients

# b) Compute the correlation between the fitted values and the response. Square it. 
# Identify where this value appears in the regression output. 
(cor(fitted(cheese), cheddar$taste))^2

summary(cheese)$r.squared

# c) Fit the same regression model but without an intercept term. What is the value 
# of R-squared reported in the output? Compute a more reasonable measure of the
# goodness of fit for this example
no.int.cheese <- lm(taste ~ . - 1, data = cheddar)

summary(no.int.cheese)$r.squared
(cor(fitted(no.int.cheese), cheddar$taste))^2

# 7) An experiement was conducted to determine the effect of four factors on the 
# resistivity of a semiconductor wafer. The data is found in the wafer dataset where 
# each of the four factors is coded as - or + depending on whether the low or high 
# setting for that factor was used. Fit the linear model resist ~ x1 + x2 + x3 + x4
data("wafer")
waf.fit <- lm(resist ~ x1 + x2 + x3 + x4, data = wafer)

# a) Extract the X matrix using the model.matrix function. Examine this to determine 
# how the low and high levels have been coded in the model. 
waf.mat <- model.matrix(~ x1 + x2 + x3 + x4, data = wafer)

# b) Compute the correlation in the X matrix. Why are there some missing values in
# the matrix. 
cor(waf.mat)

# c) What difference in resistance is expected when moving from the low to the high
# level of x1? 
waf.fit$coefficients[2]

# d) Refit the model without x4 and examine the regression coefficients and standard
# errors? What stayed the same as the original fit and what changed?
no.4 <- lm(resist ~ x1 + x2 + x3, data = wafer)
