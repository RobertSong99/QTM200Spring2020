setwd("~/GitHub/QTM200Spring2020/problem_sets/PS7/My Problem Set 7")
MexicoMuniData <- read.csv("~/GitHub/QTM200Spring2020/problem_sets/PS7/MexicoMuniData.csv")
View(MexicoMuniData)

###Part 1 Question 1###
PANdata = MexicoMuniData
summary(PANdata)

pan_model2 <- lm(PAN.visits.06 ~ competitive.district + PAN.governor.06 + marginality.06, data = PANdata, family=poisson(link = "log"))
summary(pan_model2)

###Part 1 Question 3####
0.10076 - 0.04827*1 - 0.03918*1 - 0.12046*0

###Part 2 Question 1###
library(lme4)
str(sleepstudy)

model1 <- lm(Reaction ~ Days, data = sleepstudy)
summary(model1)
ggplot(data = sleepstudy, aes(Days, Reaction))+
  geom_point() + 
  geom_smooth(method = "lm")

sleepstudy$PooledPredictions <- fitted(model1)
plot(model1) #checking residuals and variance. Seems like there are a few points that are view more as extreme such as point 57, 99, and 10.

###Part 2 Question 2###
model2 <- lm(Reaction ~ Days + Subject, data = sleepstudy)

sleepstudy$WithSubject <- fitted(model2)

###Part 2 Question 3###
model3 <- lm(Reaction ~ Days:Subject, data = sleepstudy)

sleepstudy$Varying <- fitted(model3)

###Part 2 Question 4###
interaction_model <- lm(Reaction ~ Days + Subject + Days:Subject, data=sleepstudy)
sleepstudy$Interaction <- fitted(interaction_model)

###Part 2 Question 5 ###

plot_sleep <- ggplot(data=sleepstudy, aes(x=Days, y = Reaction, group = Subject))+
  geom_line(aes(y = PooledPredictions), color = "red")+
  geom_line(aes(y=WithSubject), color = "blue")+
  #geom_line(aes(y=Varying), color = "green") +
  #geom_line(aes(y = Interaction), color = "orange")+
  geom_point(alpha = 0.2, size = 2) + 
  facet_wrap(~Subject)
plot_sleep

plot_sleep2 <- ggplot(data=sleepstudy, aes(x=Days, y = Reaction, group = Subject))+
  geom_line(aes(y = PooledPredictions), color = "red")+
  #geom_line(aes(y=WithSubject), color = "blue")+
  geom_line(aes(y=Varying), color = "green") +
  #geom_line(aes(y = Interaction), color = "orange")+
  geom_point(alpha = 0.2, size = 2) + 
  facet_wrap(~Subject)
plot_sleep2

plot_sleep3 <- ggplot(data=sleepstudy, aes(x=Days, y = Reaction, group = Subject))+
  geom_line(aes(y = PooledPredictions), color = "red")+
  #geom_line(aes(y=WithSubject), color = "blue")+
  #geom_line(aes(y=Varying), color = "green") +
  geom_line(aes(y = Interaction), color = "orange")+
  geom_point(alpha = 0.2, size = 2) + 
  facet_wrap(~Subject)
plot_sleep3