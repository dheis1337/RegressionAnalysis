library(faraway)


# # Using the teengamb data, fit a model with gamble as the response and the other
# variables as predictors.
data("teengamb")
fit <- lm(gamble ~ ., data = teengamb)


# (a) Predict the amount that a male with average (given these data) status, income
# and verbal score would gamble along with an appropriate 95% CI.
cols <- model.matrix(fit)
x0 <- apply(cols, 2, mean)
x0[2] <- 0
predict(fit, new = data.frame(t(x0)), interval = "prediction")

# (b) Repeat the prediction for a male with maximal values (for this data) of status,
# income and verbal score. Which CI is wider and why is this result expected?
cols <- model.matrix(fit)
x1 <- apply(cols, 2, max)
x1[2] <- 0
predict(fit, new = data.frame(t(x1)), interval = "prediction")


# (c) Fit a model with sqrt(gamble) as the response but with the same predictors.
# Now predict the response and give a 95% prediction interval for the individual
# in (a). Take care to give your answer in the original units of the response.
sqrt.fit <- lm(sqrt(gamble) ~ ., data = teengamb)
cols <- model.matrix(sqrt.fit)
x0 <- apply(cols, 2, mean)
x0[2] <- 0
predict(sqrt.fit, new = data.frame(t(x0)), interval = "prediction")

# (d) Repeat the prediction for the model in (c) for a female with status=20,
# income=1, verbal = 10. Comment on the credibility of the result.
x3 <- x0
x3[1:5] <- c(1, 1, 20, 1, 10)
predict(sqrt.fit, new = data.frame(t(x3)), interval = "prediction")


# 4.5 For the fat data used in this chapter, a smaller model using only age, weight,
# height and abdom was proposed on the grounds that these predictors are either
# known by the individual or easily measured.
data("fat")

#(a) Compare this model to the full thirteen-predictor model used earlier in the
# chapter. Is it justifiable to use the smaller model?
big.fit <- lm(brozek ~ age + weight + height + neck + chest + abdom +
                hip + thigh + knee + ankle + biceps + forearm + wrist, data=fat)
lil.fit <- lm(brozek ~ age + weight + height + abdom, data = fat)

anova(lil.fit, big.fit)

# (b) Compute a 95% prediction interval for median predictor values and compare
# to the results to the interval for the full model. Do the intervals differ by a
# practically important amount?
lil.mat <- model.matrix(lil.fit)
lil.x0 <- apply(lil.mat, 2, median)

big.mat <- model.matrix(big.fit)
big.x0 <- apply(big.mat, 2, median)

predict(lil.fit, new = data.frame(t(lil.x0)), interval = "prediction")
predict(big.fit, new = data.frame(t(big.x0)), interval = "prediction")


# (c) For the smaller model, examine all the observations from case numbers 25 to
# 50. Which two observations seem particularly anomalous?
lil.mat[25:50, ] # Entries 39 and 42

# (d) Recompute the 95% prediction interval for median predictor values after these
# two anomalous cases have been excluded from the data. Did this make much
# difference to the outcome?
fat <- fat[-c(39, 42), ]

clean.fit <- lm(brozek ~ age + weight + height + abdom, data = fat)
clean.mat <- model.matrix(clean.fit)
clean.x0 <- apply(clean.mat, 2, median)

predict(clean.fit, new = data.frame(t(clean.x0)), interval = "prediction")
