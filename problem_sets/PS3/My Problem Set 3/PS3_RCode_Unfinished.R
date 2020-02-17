#PS5


setwd("~/GitHub/QTM200Spring2020/problem_sets/PS3/My Problem Set 3")

library(tidyverse)
incumbents <- read.csv("~/GitHub/QTM200Spring2020/problem_sets/PS3/My Problem Set 3/incumbents_subset.csv")
View(incumbents)
####### Question 1 #########

### Part 1 ###
difflog <- select(incumbents, difflog)
voteshare <- select(incumbents, voteshare)

diff_vote <- lm(voteshare ~ difflog, data = incumbents)
diff_vote

plot(diff_vote) 
abline(plot(diff_vote))

incumbents_predicted <- predict(diff_vote)
incumbents_resid <- residuals(diff_vote)

pdf("plot_Q1_1.pdf")
plot_resid <- ggplot(data = incumbents, aes(difflog, voteshare)) + 
  geom_point(alpha=I(.3)) +
  geom_point(aes(y=incumbents_predicted), alpha = I(.1), color = "red") +
  geom_segment(aes(xend=difflog, yend = incumbents_predicted), alpha = I(.1)) +
  geom_smooth(method = "lm", se = F)
plot_resid
dev.off()

### Part 2 ###
pdf("plot_Q1_2.pdf")
plot(incumbents$difflog, incumbents$voteshare, main="Voteshare Vs. DiffLog",
     xlab="Difflog", ylab="Voteshare")
abline(diff_vote)
dev.off()

###Part 3###
pdf("plot_Q1_3.pdf")
plot_residual <- ggplot(data = incumbents, aes(difflog, voteshare)) + 
  geom_point(alpha=I(.3)) +
  geom_point(aes(y=incumbents_resid), alpha = I(.1), color = "red") +
  geom_segment(aes(xend=difflog, yend = incumbents_resid), alpha = I(.1)) +
  geom_smooth(method = "lm", se = F)
plot_residual
dev.off()


###Part 4###
summary(diff_vote)
#voteshare = .579 + .04(difflog) with p-value almost being 0 at 2.2 e-16, meaning our results are significant. For every 1 change in difflog, the difference in campaign spending between incumbent and challenger, there is a .04 increase in voteshare, or specifically the incumbent's vote share .

###########Quesiton 2 ##################

###Part 1###
presvote <- select(incumbents, presvote)
presvote

presvote_diff <- lm(presvote ~ difflog, data = incumbents)
presvote_diff

plot(presvote_diff)
abline(plot(presvote_diff))

incumbents_predicted_presvote <- predict(presvote_diff)
incumbents_presvote_resid <- residuals(presvote_diff)

pdf("plot_Q2_1.pdf")
plot_resid_presvote <- ggplot(data = incumbents, aes(difflog, presvote)) + 
  geom_point(alpha=I(.3)) +
  geom_point(aes(y=incumbents_predicted_presvote), alpha = I(.1), color = "red") +
  geom_segment(aes(xend=difflog, yend=incumbents_predicted_presvote), alpha = I(.1)) +
  geom_smooth(method = "lm", se = F)
plot_resid_presvote
dev.off()

###Part 2###
pdf("plot_Q2_2.pdf")
plot(incumbents$difflog, incumbents$presvote, main="Presvote Vs. DiffLog",
     xlab="Difflog", ylab="Presvote")
abline(presvote_diff)
dev.off()

###Part 3###
pdf("plot_Q3_3.pdf")
plot_presvote <- ggplot(data = incumbents, aes(difflog, presvote)) + 
  geom_point(alpha=I(.3)) +
  geom_point(aes(y=incumbents_presvote_resid), alpha = I(.1), color = "red") +
  geom_segment(aes(xend=difflog, yend = incumbents_presvote_resid), alpha = I(.1)) +
  geom_smooth(method = "lm", se = F)
plot_presvote
dev.off()

###Part 4####
summary(presvote_diff)
#prevote = .508 + .02(difflog) with p-value almost being 0 at 2.2 e-16, meaning our results are significant. For every 1 change in difflog, or the difference between incumbent and challenger's spending, there is a .02 increase in presvote, the vote share of the presidential candidate of the incumbent's party.

###########Quesiton 3 ##################

presvote <- select(incumbents, presvote)
voteshare <- select(incumbents, voteshare)

presvote_voteshare <- lm(voteshare ~ presvote, data = incumbents)
presvote_voteshare

plot(presvote_voteshare)
abline(plot(presvote_voteshare))

incumbents_predicted_presvote_voteshare <- predict(presvote_voteshare)
incumbents_resid_presvote_voteshare <- residuals(presvote_voteshare)

pdf("plot_Q3_1.pdf")
plot_presvote_voteshare <- ggplot(data = incumbents, aes(presvote, voteshare)) + 
  geom_point(alpha=I(.3)) +
  geom_point(aes(y=incumbents_predicted_presvote_voteshare), alpha = I(.1), color = "red") +
  geom_segment(aes(xend=presvote, yend=incumbents_predicted_presvote_voteshare), alpha = I(.1)) +
  geom_smooth(method = "lm", se = F)
plot_presvote_voteshare
dev.off()

###Part 2###
pdf("plot_Q3_2.pdf")
plot(incumbents$presvote, incumbents$voteshare, main="Voteshare vs Presvote",
     xlab="Presvote", ylab="Voteshare")
abline(presvote_voteshare)
dev.off()

### Part 3 ###
summary(presvote_voteshare)
#voteshare = .441 + .388(presvote), with a p-value almost at 0 again, meaning our results are significant. For every 1 change in presvote, the vote share of the presidential candidate of the incumbent's party , there is a .388 increase in voteshare, or the the incumbent's vote share.

###########Question 4 #########################3

explained_plot4 <- lm(incumbents_predicted ~ incumbents_predicted_presvote, data = incumbents)

summary(explained_plot4)
#the residuals from voteshare = 1.748 + -3.082e-01(presvote residual). The p-value is almost 0 at less than 2e-16, meaning our data is significant, and that we can reject that null - these two residuals create variant results

pdf("plot_Q4")
explained_graph4 <- ggplot(data = incumbents, aes(incumbents_predicted_presvote, incumbents_predicted))+
  geom_point(alpha=I(.3)) +
  geom_smooth(method = "lm", se = F)
explained_graph4
dev.off()

##################### Question 5 ####################################

x_explained5 <- incumbents$presvote + incumbents$difflog
explained_plot5<- lm(voteshare ~ x_explained5, data = incumbents)

pdf("plot_Q5")
ggplot_explained <- ggplot(data = incumbents, aes(x_explained5, voteshare))+
  geom_point(alpha=I(.3)) +
  geom_smooth(method = "lm", se = F)
ggplot_explained
dev.off()

summary(explained_plot5)
#the residual from voteshare = .555 + .04(x_explained5). x_explained5 is accounting the presvote and difflog variables, to account for the conclusion of voteshare. 
#the similarities exist through their explained results. In question 4, utilizing the residuals allows us to take the variation from presvote which was run again diff log, to be observed. Likewise to that difference, plotting both presvote and difflog together should produce similar results for their combined effect and accountablity towards the results of voteshare.