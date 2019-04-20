###
# Chapter 2 Exercise Solutions
# April 20, 2019
###

# Conceptual
###
# 1.
### a. Flexible. There is enough data to capture signal over noise
### b. Inflexible. You will overfit with small n and large p
### c. Flexible. It will allow you to find y_hat similar to y
### d. Inflexible. Lots of noise. Flexible models will model noise

# 2.
### a. Regression. Inference problem. n = 500, p = 3
### b. Classification. Prediction problem. n = 20, p = 13
### c. Regression. Prediction problem. n = 51, p = 3

# 8
college = read.csv("C:/Users/Owner/Desktop/Programming_Education/Stanford - ISLR/Ch 2/College.csv",
                   header = T)
rownames(college) <- college[,1]
fix(college)
college = college [,-1]
fix(college)
summary(college)
pairs(college[,1:10])
plot(college$Private, college$Outstate)
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(college$Elite, college$Outstate)
par(mfrow = c(2,2))
hist(college$Enroll, breaks = 10)
hist(college$Enroll, breaks = 15)
hist(college$Enroll, breaks = 20)
hist(college$Enroll, breaks = 25)

# 9
auto = read.csv ("C:/Users/Owner/Desktop/Programming_Education/Stanford - ISLR/Ch 2/Auto.csv", header = T,
                 na.strings="?")
nrow(auto)
auto1 = na.omit(auto)
nrow(auto1)
auto1$name = as.factor(auto1$name)
auto1$origin = as.factor(auto1$origin)
range(auto1[,1])
range(auto1[,2])
range(auto1[,3])
summary(auto1)
summary(auto1[-c(10:85),])
pairs(auto1)
y = lm (auto1$mpg~auto1$displacement + auto1$horsepower + auto1$weight + auto1$acceleration + auto1$year)
summary(y)

# 10
library(MASS)
# Has 506 rows, 14 columns
summary(Boston)
Boston$chas = as.factor(Boston$chas)
pairs(Boston)
summary(lm(Boston$crim ~Boston$lstat))
y=lm(Boston$crim ~Boston$age)
plot(Boston$black, Boston$crim)
par(mfrow = c(1,1))
abline(y)
plot(Boston$rad, Boston$crim)
sum(Boston$chas==1)/nrow(Boston)
