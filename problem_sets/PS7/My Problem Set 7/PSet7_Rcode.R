setwd("~/GitHub/QTM200Spring2020/problem_sets/PS7/My Problem Set 7")

###Part 1 Question 1###
PANdata = MexicoMuniData
summary(PANdata)
PANdata$competitive.district

###Part 2 Question 1###
library(lme4)
str(sleepstudy)

model1 <- lm(Reaction ~ Days, data = sleepstudy)
summary(model1)
ggplot(data = sleepstudy, aes(Days, Reaction))+
  geom_point() + 
  geom_smooth(method = "lm")

plot(model1) #checking residuals and variance. Seems like there are a few points that are view more as extreme such as point 57, 99, and 10.

###Part 2 Question 2###
model2 <- lmList(Reaction ~ Days | Subject, data = sleepstudy)
model2coef <- coef(model2)
names(model2coef) <- c("Intercept", "Days.Slope")
summary(model2coef)

ggplot(data = model2coef, aes(Intercept, Days.Slope))+
  geom_point()+
  geom_smooth()

###Part 2 Question 3###
model3 <- lmList(Days.Slope ~ Intercept, data = model2coef)
model3coef <- coef(model3)

###Part 2 Question 4###
model1_mix = lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(model1_mix)
