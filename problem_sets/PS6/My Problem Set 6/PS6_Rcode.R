setwd("~/GitHub/QTM200Spring2020/problem_sets/PS6/My Problem Set 6")
cholesterol <- read.csv("~/GitHub/QTM200Spring2020/problem_sets/PS6/cholesterol.csv")
cholesterol
library(ggplot2)
###Part 1 Question 1###
#Part A#

fit <- glm(cholCat ~ sex+fat, data = cholesterol, family=gaussian())
summary(fit) #has a p-value of .02
plot(fit)

table(cholesterol$sex, cholesterol$cholCat)

###PArt 1 Question 2###
#Part c#
-.1303597 + (.1894160*0) + (0.0082466*100)

###Part 2 Question 1###
library(nnet)
mydata = gdpChange
mydata$GDPWdiff2 = relevel(mydata$GDPWdiff, ref = "no change")

multil = multinom(GDPWdiff2 ~ REG + OIL, data=mydata)
summary(multil)
coef(summary(multil))

###Part 2 Question 2###
library(MASS)
ordinal = polr(GDPWdiff ~ REG + OIL, data=mydata)
summary(ordinal)
coef(summary(ordinal))
