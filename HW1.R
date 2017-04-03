library(faraway) # Load in faraway package
library(ggplot2)
data(teengamb) # Load data set

?teengamb # Find information of data set

# 1) Clean the data. (Show your work).
# It appears that sex is represented as a 0, 1 binary. 0 is male, 1 is female,
# so let's change that. 
teengamb$sex[teengamb$sex == 0] <- "male"
teengamb$sex[teengamb$sex == 1] <- "female"

# Let's also make this a factor variable, since it is categorical
teengamb$sex <- as.factor(teengamb$sex)

# Just to ensure it is a factor
is.factor(teengamb$sex) # Result is two
levels(teengamb$sex) # Print the levels

# Now to income. Income is a number represented as pounds earned per week. Since
# our gambing variable is in pounds per year, let's scale income by 52, so they're 
# units match
teengamb$income <- teengamb$income * 52
 
# This may be unecessary, but the verbal variable is a score on a verbal test out
# of 12. Let's just divide this column by 12 so we can see a percentage representation
# each person's score. 
teengamb$verbal <- teengamb$verbal / 12


# 2) Calculate and provide numerical summaries for all variables. Comment on the 
# numerical summary for each variable. 
# First, let's provide a summary on the sex variable. 
summary(teengamb$sex)

# Now, the status variable
summary(teengamb$status)

# Now, the income variable
summary(teengamb$income)

# Now, the verbal variable
summary(teengamb$verbal)

# Finally, the gamble variable
summary(teengamb$gamble)

# 3) Construct and provide at least one univariate graphic for each variable. Comment
#  on interesting features of each plot. 
# First, sex 
ggplot(teengamb, aes(x = sex)) + geom_bar(fill = "burlywood2", color = "black")

# Now, status
ggplot(teengamb, aes(x = status)) + geom_histogram(binwidth = 5)
  ggplot(teengamb, aes(x = status)) + geom_density(adjust = 1/3)
boxplot(teengamb$status)

# Now, income
ggplot(teengamb, aes(x = income)) + geom_histogram(binwidth = 50)
ggplot(teengamb, aes(x = income)) + geom_density(adjust = 1)

# Now, verbal
ggplot(teengamb, aes(x = verbal)) + geom_histogram(binwidth = 1/10)
ggplot(teengamb, aes(x = verbal)) + geom_density(adjust = 1/2)

# Finally, gambe
ggplot(teengamb, aes(x = gamble)) + geom_histogram(binwidth = 15)
ggplot(teengamb, aes(x = gamble)) + geom_density(adjust = 1)

# 4) Construct bivariate plots for every combination of variables. Comment on any 
# relationships you notice. 
# Gamble ~ status
ggplot(teengamb, aes(x = status, y = gamble)) + geom_point()

# Gamble ~ income
ggplot(teengamb, aes(x = income, y = gamble)) + geom_point()

# Gamble ~ verbal
ggplot(teengamb, aes(x = verbal, y = gamble)) + geom_point()

# Gamble ~ sex
boxplot(gamble ~ sex, teengamb)

# Status ~ income
ggplot(teengamb, aes(x = income, y = status)) + geom_point()

# Status ~ verbal
ggplot(teengamb, aes(x = verbal, y = status)) + geom_point()

# Status ~ sex
boxplot(status ~ sex, teengamb)

# Income ~ Verbal
ggplot(teengamb, aes(x = verbal, y = income)) + geom_point()

# Verbal ~ sex
boxplot(verbal ~ sex, teengamb)

# Income ~ sex
boxplot(income ~ sex, teengamb)

# Construct and provide densities for all quantitative variables split by sex. 
# What do you notice about the distribution of the variables for the different sexes?
# Income
ggplot(teengamb, aes(x = income)) + 
  geom_density() +
  facet_wrap(~ sex)

# Status
ggplot(teengamb, aes(x = status)) + 
  geom_density() +
  facet_wrap(~ sex)

# Verbal
ggplot(teengamb, aes(x = verbal)) + 
  geom_density() +
  facet_wrap(~ sex)


# Gamble
ggplot(teengamb, aes(x = gamble)) + 
  geom_density() +
  facet_wrap(~ sex)

