library(faraway)
library(ggplot2)
library(car)

# 3.1) For the prostatedata, fit a model with lpsa as the response and the other vari-
# ables as predictors:
data("prostate")
fit <- lm(lpsa ~ ., data = prostate)

# (a) Compute 90 and 95% CIs for the parameter associated with age. Using just
# these intervals, what could we have deduced about the p-value for age in the
# regression summary?
confint(fit)
confint(fit, level = .9)


# (b) Compute and display a 95% joint confidence region for the parameters asso-
#   ciated with age and lbph. Plot the origin on this display. The location of the
# origin on the display tells us the outcome of a certain hypothesis test. State that
# test and its outcome.
confidenceEllipse(fit, which.coef = c(4, 5))
points(x = 0, y = 0)


# (c) In the text, we made a permutation test corresponding to the F-test for the
# significance of all the predictors. Execute the permutation test corresponding
# to the t-test for age in this model. (Hint: {summary(g)$coef[4,3] gets you
# the t-statistic you need if the model is called g.)
obs <- summary(fit)$coef[4, 3]
n <- 1000
samp <- vector("numeric", length = n)

for (i in 1:n) {
  mod <- lm(lpsa ~ . - age + sample(age), data = prostate)
  samp[i] <- summary(mod)$coef[9, 3]
}

mean(abs(samp) >= abs(obs))

# (d) Remove all the predictors that are not significant at the 5% level. Test this
# model against the original model. Which model is preferred?
summary(fit)
lil.fit <- lm(lpsa ~ lcavol + lweight + svi, data = prostate)
anova(lil.fit, fit)

# 5. Find a formula relating R 2 and the F-test for the regression.

# 6. Thirty-nine MBA students were asked about happiness and how this related to
# their income and social life. The data are found in happy. Fit a regression model
# with happy as the response and the other four variables as predictors.
data("happy")
mba.fit <- lm(happy ~ ., data = happy)
# (a) Which predictors were statistically significant at the 1% level?
summary(mba.fit)

# (b) Use the table function to produce a numerical summary of the response. What
# assumption used to perform the t-tests seems questionable in light of this sum-
# mary?
table(happy$happy)

# (c) Use the permutation procedure described in Section 3.3 to test the significance
# of the money predictor.
mba.obs <- summary(mba.fit)$coef[2, 3]

ts <- vector("numeric", length = n)

for (i in 1:n) {
  t.mba <- lm(happy ~ sample(money) + sex + love + work, data = happy)
  ts[i] <- summary(t.mba)$coef[2, 3]
}

mean(abs(ts) >= abs(mba.obs))
# (d) Plota histgram of the permutation t-statistics. Make sure you use the the prob-
# ability rather than frequency version of the histogram.
hist(ts, freq = FALSE)


# (e) Overlay an appropriate t-density over the histogram. Hint: Use grid <-
# seq(-3, 3, length = 300)to create a grid of values, then use the dtfunc-
# tion to compute the t-density on this grid and the lines function to superim-
# pose the result.
grid <- seq(-3, 3, length.out = 300)
t34 <- dt(grid, 34)
lines(grid, t34)

# (f) Use the bootstrap procedure from Section 3.6 to compute 90% and 95% con-
# fidence intervals for ?? money . Does zero fall within these confidence intervals?
# Are these results consistent with previous tests?
nb <- 1000
coefmat <- matrix(0, nb, 5)
resids <- residuals(mba.fit)
preds <- fitted(mba.fit)

for (i in 1:nb) {
  booty <- preds + sample(resids, replace = TRUE)
  bmod <- lm(booty ~ money + sex + love + work, data = happy)
  coefmat[i, ] <- coef(bmod)
}

colnames(coefmat) <- c("Intercept", "money", "sex", "love", "work")
coefmat <- data.frame(coefmat)

apply(coefmat, 2, function (x) {quantile(x, c(.05, .95))})
