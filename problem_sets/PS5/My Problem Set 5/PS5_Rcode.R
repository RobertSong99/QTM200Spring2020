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

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
lapply(c("faraway"),  pkgTest)

setwd("~/GitHub/QTM200Spring2020/problem_sets/PS5/My Problem Set 5")


gamble <- (data=teengamb)
View(gamble)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex+status + income + verbal, gamble)
model1
plot(model1) #obtained residual vs fitted plot. 

cooks.distance(model1)
plot(cooks.distance(model1))

#### Part D ####
outlierTest(model1)

#### Part E ####

plot(hatvalues(model1), rstudent(model1), type = "n")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1),
       cex=10*cook/max(cook))
abline(h=c(-2,0,2), lty = 2)
abline(v=c(2,3) * 3/45, lty = 2)
identify(hatvalues(model1), rstudent(model1), row.names(gamble))
