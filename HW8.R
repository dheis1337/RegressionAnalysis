library(alr4)

# 1. Fit a	simple	linear	regression	of log(fertility) on	pctUrban using	the	UN11
# data	set	in	the	alr4 package.	
data("UN11")
fit <- lm(log(fertility) ~ pctUrban, data = UN11)
# a. Provide the	equation	for	the	fitted	model.
summary(fit)

# b. Interpret	the	estimated	coefficient	for	pctUrban in	a	straightforward	
# sentence in	the	context	of	the	problem.

# 2. Using	the	same	data	set	as	the	previous	problem,	fit	the	regression	of	
# log(fertility) on log(ppgdp) and	lifeExpR.
loglog.fit <- lm(log(fertility) ~ log(ppgdp) + lifeExpF, data = UN11)

# a. What	is	the	equation	for	the	fitted	model?
summary(fit)

# b. If	we	increase	ppgdp by	25%,	what	is	the	expected	decrease	in	fertility?		
# (You	will	need	to	do	some	computing).


# c. Interpret	the	estimated	coefficient	for	ppgdp in	a	straightforward	sentence
# in	the	context	of	the	problem.