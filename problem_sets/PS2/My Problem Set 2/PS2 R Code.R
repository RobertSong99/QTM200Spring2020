#Problem Set 2
#Question 1A:
#Problem Set 2

##Question 1A:
X = matrix(c(14, 7, 6, 7, 7, 1), nrow = 2, ncol = 3) #Creating matrix from problem

colnames(X) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(X) <- c("Upper Class", "Lower Class")
X #Creation of Matrix

no_stop_sum <- sum(X[,1]) 
no_stop_sum #21

bribe_sum <- sum(X[,2])
bribe_sum #13

stopped_sum <- sum(X[,3])
stopped_sum #8

#getting total for each column

upper_sum <- sum(X[1,])
upper_sum #27

lower_sum <- sum(X[2,])
lower_sum #15

#Getting total for each row
sum(upper_sum, lower_sum) #Matrix total is 42

#now remaking matrix, to have column and row totals for observed values
new_x = matrix(c(14, 7, 21, 6, 7, 13, 7, 1, 8, 27, 15, 42), nrow = 3, ncol = 4)

colnames(new_x) <- c("Not Stopped", "Bribe requested", "Stopped/given warning", "Column Total")
rownames(new_x) <- c("Upper Class", "Lower Class", "Row Total")
new_x #Creation of Matrix

#to calculate expected values for chi squared test = (row total/grand sum) * column total
(matrix(new_x[3,1]) / matrix(new_x[3,4])) * matrix(new_x[1,4]) #13.5
(matrix(new_x[3,1]) / matrix(new_x[3,4])) * matrix(new_x[2,4]) #7.5
(matrix(new_x[3,2]) / matrix(new_x[3,4])) * matrix(new_x[1,4]) #8.36
(matrix(new_x[3,2]) / matrix(new_x[3,4])) * matrix(new_x[2,4]) #4.64
(matrix(new_x[3,3]) / matrix(new_x[3,4])) * matrix(new_x[1,4]) #5.14
(matrix(new_x[3,3]) / matrix(new_x[3,4])) * matrix(new_x[2,4]) #2.86

#creating matrix of expected values
expected_values = matrix(c(13.5, 7.5, 8.36, 4.64, 5.14, 2.86), nrow = 2, ncol = 3)
expected_values

#calculating chi squared directly
sum(((X - expected_values)^2) / expected_values) #Chi squared values is 3.801141

#Using chi squared function in R to test if values are close, which they are with the function outputting 3.7912
chisq.test(X)

##1B

#calculating p-value with chi squared value = 3.801141, df= (2-1)(3-1), and lower tail = false, as we only want the upper end due to distribution type
pchisq(3.801141, 2, lower.tail = F)
#p-value = 0.1495 meaning we fail to reject the null, that officers would not solicit a bribe depending on the class

##1C
#calculating standardized residuals and want to see the matrixes again
new_x
expected_values

A_one = ((X[1,1] - expected_values[1,1])/ (sqrt(expected_values[1,1])*(1-(27/42))* (1-(21/42))))
A_one
A_two = ((X[2,1] - expected_values[2,1])/ (sqrt(expected_values[2,1])*(1-(15/42))* (1-(21/42))))
A_two

B_one = ((X[1,2] - expected_values[1,2])/ (sqrt(expected_values[1,2])*(1-(27/42))* (1-(13/42))))
B_one
B_two = ((X[2,2] - expected_values[2,2])/ (sqrt(expected_values[2,2])*(1-(15/42))* (1-(13/42))))
B_two

C_one = ((X[1,3] - expected_values[1,3])/ (sqrt(expected_values[1,3])*(1-(27/42))* (1-(8/42))))
C_one
C_two = ((X[2,3] - expected_values[2,3])/ (sqrt(expected_values[2,3])*(1-(15/42))* (1-(8/42))))
C_two

#creating matrix for standardized residuals
standardized_matrix = matrix(c(0.762, -0.568, -3.310, 2.468, 2.838, -2.113), nrow=2, ncol=3)

colnames(standardized_matrix) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(standardized_matrix) <- c("Upper Class", "Lower Class")
standardized_matrix #Creation of Matrix

##1D
#The standard residual data may help with the interpretation of the results because they show how far off each data point is from a normal projected value. Simply, a the standaraized residuals illustrates the difference between the observed and expected results, and show which values are contributing most in either more/less bias or no bias ways.

#2 Economics
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS2/My Problem Set 2")
economics_ps <- read.csv("~/GitHub/QTM200Spring2020/problem_sets/PS2/My Problem Set 2/Economics PS.txt")


##2A:
#Null Hypothesis: There is no effect from the reservation policy on the number of new or repaired drinking water facilities in the villages
#Alternative Hypothesis: There is an effect from the reservation policy on the number of new or repaired drinking water facilities in the villages


##2B
ggplot(aes(water, irrigation), data = economics_ps) + 
  geom_point() +
  geom_smooth(method = 'lm')

#can also plot against the identifier BP to see in each location how many water sites there are
ggplot(aes(GP, water), data = economics_ps) + 
  geom_point() +
  geom_smooth(method = 'lm')

##2C
c(round(mean(economics_ps$water), 2), round(sd(economics_ps$water),2)) #x bar which is the calculated mean is 17.84, while the standard deviation is 33.68.

standardized_water <- (((economics_ps$water - (mean(economics_ps$water)))/sd(economics_ps$water)))
round(standardized_water, 2) #calculated standardized difference from the observed valued of lifespans

(1/(322-1)) * (sum(round(standardized_water, 2)))  

#3: Fruit Flies

fruitfly <- read.csv("~/GitHub/QTM200Spring2020/problem_sets/PS2/fruitfly.csv")
summary(fruitfly)

##3-1

library(tidyverse)
library(dplyr) #inputing library for filter function
fruitfly_type1 <- filter(fruitfly, type == 1)
fruitfly_type1 #isolating within the fruitfly dataset, each type

fruitfly_type2 <- filter(fruitfly, type == 2)
fruitfly_type2

fruitfly_type3 <- filter(fruitfly, type == 3)
fruitfly_type3

fruitfly_type4 <- filter(fruitfly, type == 4)
fruitfly_type4

fruitfly_type5 <- filter(fruitfly, type == 5)
fruitfly_type5

boxplot(fruitfly_type1$lifespan, fruitfly_type2$lifespan, fruitfly_type3$lifespan, fruitfly_type4$lifespan, fruitfly_type5$lifespan, names = c("1", "2","3", "4", "5"), col = 'light blue', main="Fruitfly Lifespan by Type",
        xlab="Type", ylab="Lifespan (Days)")
#making a boxplot that isolates by type, and shows the lifespan overall. It seems that type 2 and 3 have the highest mean length for lifespan, whereas type 5 has the lowest on average. The spread of the lifespan per type also varies quite drastically, so that must be taken into account when analyzing which type has higher lifespan as well. 


##3-2
ggplot(aes(lifespan, thorax), data = fruitfly) +
  geom_point(aes(color = type))+
  geom_smooth(method = 'lm')
#There does seem to be a positive, linear, relationship between lifespan and thorax length. 


fruitfly
c(round(mean(fruitfly$lifespan), 2), round(sd(fruitfly$lifespan),2)) #x bar which is the calculated mean is 57.44, while the standard deviation is 17.56.

standardized_fly <- (((fruitfly$lifespan - (mean(fruitfly$lifespan)))/sd(fruitfly$lifespan)))
round(standardized_fly, 2) #calculated standardized difference from the observed valued of lifespans

standardized_thorax <- (((fruitfly$thorax - (mean(fruitfly$thorax)))/sd(fruitfly$thorax)))
round(standardized_thorax, 2) #calculated standardized difference from the observed valued of thorax length

standardized_fly_thorax <- standardized_fly * standardized_thorax #multiplying each standardarized values to then calculate correlation coefficient

(1/(125-1))*sum(round(standardized_fly_thorax, 2)) #the correlation coefficient is .636 which is a decently strong, and positive relationship that is explained between lifespan and thorax length. 

##3-3

summary(lm(fruitfly$thorax ~ fruitfly$lifespan))

y <- .6597 + 0.0028*(fruitfly$lifespan) #creating the basic slope-intercept line with the gained intercept and slope from the summary.
plot(fruitfly$lifespan, y , type = "l") #creation of basic line graph to illustrate the intercept and slope of the relationship between lifespan and thorax length

##3-4
t.test(fruitfly$lifespan, fruitfly$thorax, alternative = "two.sided", var.equal = FALSE)
summary(lm(fruitfly$thorax ~ fruitfly$lifespan))

thorax_lifespan <- lm(fruitfly$thorax ~ fruitfly$lifespan) #creation of one variable under the line relationship of thorax and lifespan
sigma(thorax_lifespan) #finding sigma of this relationship to be 0.06

beta_se <- sigma(thorax_lifespan)/sqrt(sum((fruitfly$lifespan - mean(fruitfly$lifespan))^2)) 
beta_se #finding the standard error of the lifespan quantity to be .0003

2*pt((.0028 - 0)/beta_se,dim(fruitfly)[1]-2, lower.tail = F) #p-value for beta-1 is 1.69e-15, which is very similar to checking through summary function
summary(thorax_lifespan)

##3-5
summary(lm(fruitfly$thorax ~ fruitfly$lifespan))

b1 <- .0028 #identification of the slope beta1
se <- .0003 #standard error of the graph between the relationship of thorax and lifespan
n <- 125 #sample size

t_test <- abs(qt(.1/2, df= n-2)) #t test that is two tailed
t_test

left <- b1 - se*t_test #obtaining the values of left and right
left
right <- b1 + se*t_test
right

thorax_lifespan <- lm(fruitfly$thorax ~ fruitfly$lifespan)
confint(thorax_lifespan, level = .9) #double checking the confidence interval which we obtain to be .0023 to .0033

##3-6
summary(lm(fruitfly$thorax ~ fruitfly$lifespan))
ggplot(fruitfly, aes(lifespan, thorax)) +
  geom_point() +
  stat_smooth(method = lm) #creating variable for graph with the grey color indicating the 95% confidence interval range from the regression line, whereas the red lines are the prediction lines, all under 95%.

new_fruitfly <- fruitfly
confidence <- as.data.frame(predict(thorax_lifespan, newdata = new_fruitfly, interval = "confidence")) #illustrates that on average per day in lifespan, the average thorax length ranges
mean(confidence$upr) #mean upper value in confidence interval is .8354 for thorax length
mean(confidence$lwr) #mean lower value in confidence interval is .8065 for thorax length

predict_data <- as.data.frame(predict(thorax_lifespan, newdata = new_fruitfly, interval = "prediction")) #illustrates that on average per day in lifespan, the average thorax length ranges
mean(predict_data$fit) #mean fitted value for predicted thorax length is .821

##3-7
new_graph <- ggplot(aes(lifespan, thorax), data = fruitfly) +
  geom_point(aes(color = type))+
  geom_smooth(method = 'lm')

new_graph + geom_line(aes(y = predict_data $lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = predict_data$upr), color = "red", linetype = "dashed")