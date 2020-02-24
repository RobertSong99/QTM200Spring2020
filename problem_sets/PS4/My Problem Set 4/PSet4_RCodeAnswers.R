setwd("~/GitHub/QTM200Spring2020/problem_sets/PS4/My Problem Set 4")
install.packages("car")
library("car")
data(Prestige)
help(Prestige)

library('tidyverse')
view(Prestige)

############# Question 1 ##################

### Part A ###

#changing the variable to professional from type
summary(Prestige)
prestige_data<- Prestige %>% rename(professional = type)
view(prestige_data)

#creating binary/dummy variable based on prof versus bc and wc which get 0.
ifelse(prestige_data$professional =="prof", 1, 0)


### Part B ###
#plotting prestige as outcome, income, professional, and interaction of the two as predictors
lm(prestige ~ income + professional + professional:income, data = prestige_data)


### Part F ###
13.904517 + (.004023 * 1000) + (45.019022 *1) + (18.980739 * 0) - (.003178 *1000 *1)
#59.769 when professional with income of 1000

13.904517 + (.004023 * 0) + (45.019022 *1) + (18.980739 * 0) - (.003178 *1000 * 0)
#55.754 when income is 0
59.769 - 58.92354

### Part G ###
13.904517 + (.004023 * 6000) + (45.019022 *1) + (18.980739 * 0) - (.003178 *1000 *1) - (.002171 * 1000 *0)
#79.88354
13.904517 + (.004023 * 6000) + (45.019022 *0) + (18.980739 * 1) - (.003178 *1000 *0) - (.002171 * 1000 *1)
#54.85226
79.88354-54.85226



############## Question 2 #########################3

### Part A ####

impact_matrix = matrix(c(.042, .042, .302), nrow = 3, ncol = 1) #Creating matrix from problem

colnames(impact_matrix) <- c("Impact of lawn signs on vote share")
rownames(impact_matrix) <- c("Precinct assigned lawn signs (n=30)", "Precinct adjacent to lawn signs (n=76)", "Constant")
impact_matrix #Creation of Matrix

impact_se = matrix(c(.016, .013, .011), nrow = 3, ncol = 1) #Creating matrix from problem

colnames(impact_se) <- c("Impact of lawn signs on vote share")
rownames(impact_se) <- c("Precinct assigned lawn signs (n=30)", "Precinct adjacent to lawn signs (n=76)", "Constant")
impact_se #Creation of Matrix

xbar = .042 # sample mean 
mu0 = .302 # hypothesized value 
#std = se * sqrt(n)
sigma = .016*(sqrt(30)) #standard error converted to standard deviation = .088
n = 131 # sample size 
z = (xbar-mu0)/(sigma/sqrt(n)) #-16.25 is the z score

alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) #from -1.96 to 1.96 for 95% CI


### Part B ###
xbar2 = .042 # sample mean 
mu02 = .302 # hypothesized value 
sigma2 = .013*(sqrt(76)) #standard error converted to standard deviation = 113
n2 = 131 # sample size 
z2 = (xbar2-mu02)/(sigma2/sqrt(n2)) #-20 is the z score

alpha2 = .05 
z.half.alpha2 = qnorm(1-alpha2/2) 
c(-z.half.alpha2, z.half.alpha2) #from -1.96 to 1.96 for 95% CI


### Part D ###
summary(impact_matrix)
plot(impact_matrix)
