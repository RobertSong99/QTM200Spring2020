#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
library(tidyverse) #loading package for graphical usage

lapply(c(),  pkgTest)

#Setting work directory 
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS1/My probem set")


#####################
# Problem 1
#####################

#Taking Data set and pasting
problem_1 <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

mean(problem_1) #the sample mean for IQ score is 98.44
length(problem_1) # three are 25 observations by counselor on student IQ scores
sd(problem_1) # the standard deviation of the observations in IQ scores is 13.09
std_error <- sd(problem_1) / sqrt(length(problem_1)) #accounting standard deviation based on our sample size to obtain sample error
std_error # standard error is 2.62

t.test(problem_1, alternative = "two.sided", conf.level= 0.90) #Running a t test because we have less than 30 observations, with a desired CI of 90 percent
#the average student IQ scores is between 93.96 and 102.92 90 percent of the time
#in other words, we are 90 percent confident that the average student IQ score would be between 93.96 and 102.92

#####################
# Problem 2
#####################

problem_2 <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

t.test(problem_2, mu = 100, alternative = "greater", conf.level= 0.95) #making a t test with 'greater than' function to observe if the scores on average will be greater than 100 with a 0.05 significance level
# we fail to reject the null hypothesis due to a p-value of .7215, and we do not suggest that the average IQ scores is not higher  than the average IQ 100 score.  
#####################
# Problem 3
#####################

##Bullet 1

expenditure <- read.delim("~/GitHub/QTM200Spring2020/problem_sets/PS1/expenditure.txt")
expenditure

### Per capita personal income vs. Per capita expenditure on public education relationship

ggplot(aes(Y, X1), data=expenditure) + #graphing each axis using variables within the expenditure data set
  geom_point() + #adding points for each relationship
  geom_smooth() #adding a trendline
#As per capita expenditure on public education increases, so does the relationship of per capita personal income. This is illustrated by the positive relationship drawn by the trendline. The relationship could be deemed correlational because having more people with more money, leads to more money going back into the system through taxes for example, to be spent on public education. Though one must note, ss per capita expenditure on public education passes approximately 100, the relationship becomes more neutral.

###Number of residents per thousand under 18 years of age vs. per capita personal income

ggplot(aes(X1, X2), data=expenditure) + #graphing each axis using variables within the expenditure data set
  geom_point() + #adding points for each relationship
  geom_smooth() #adding a trendline

#As per capita personal income increases, the number of residents per thousand under 18 years of age seem to decrease illustrated by the negative relationship indicated by the trendline. This makes sense, as one is more likely to earn more money later on in their life due to amass amounts of variables such as more educational experience, leading to higher paying jobs. 

### Number of people per thousand residing in urban areas vs. per capita personal income
ggplot(aes(X1, X3), data=expenditure) + #graphing each axis using variables within the expenditure data set
  geom_point() + #adding points for each relationship
  geom_smooth() #adding a trendline

#As per capita personal income increases, there is a general positive trend of the number of people per thousand residing in urban areas. This could be due to the fact that as personal income increases, one can afford to live in presumably better located residencies in urban areas, closer to work. That relationship because to decrease with the few observations around 2500 on 'X1' possibly because those who gain more money can afford even more luxurious locations outside of the urban areas. 

##Bullet 2
### Per capita expenditure on public education Vs. Region
##### 1 = Northeast | 2 = North Central | 3 = South | 4 = West|

expenditure$Region <- as.factor(expenditure$Region)
ggplot(aes(Region, Y), data=expenditure) + #graphing each axis using variables within the expenditure data set
  geom_point(aes(color = Region)) #adding points for each relationship

#For each region, there are differences between the per capita expenditure on public education as indicated by the spread within the graph. On average, the South (Region 3), has the lowest expenditure; whereas the West (Region 4) has the highest expenditure on public education. The North Central region (Region 2) slightly edges out the Northeast (Region 1), but both exist in between the South and West's expenditures

##Bullet 3
###Per capita personal income vs. Per capita expenditure on public education relationship Based on Region

ggplot(aes(X1, Y), data=expenditure) + #graphing each axis using variables within the expenditure data set
  geom_point(aes(color = Region, shape = Region, size=2)) #adding points for each relationship

#Even when distributing the data by region (as indicated by the different shapes), there is a general positive trend for an increase in per capita expenditure on public education and per capita personal income. This arguments makes sense due to the fact that having more money held by people allows them to give back more money to fund public education through taxes, or even other means.
  
