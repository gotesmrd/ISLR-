library (ISLR)
head(Default)
#############
#### 5
############
# a
log.fit <- glm(default~balance + income, data = Default, 
               family = "binomial")
summary(log.fit)
# b (i)
set.seed(1)
indices <- sample(10000, 5000)
log.fit2 <- glm(default~balance + income, data = Default, 
                family = "binomial", subset = indices)
# (ii)
probs <- predict (log.fit2, type = "response",
                  newdata = Default[-indices, 
                                              c("student","balance","income")])
classification <- ifelse (probs >0.5, "Yes" , "No")
# (iv) 
table(classification, Default$default[-indices])
### classification   No  Yes
#                    No  4828  104
#                    Yes   14   54
# Error = (104+14)/5000 = 0.0236

# c
# Try 1 
set.seed(2)
indices <- sample(10000, 5000)
log.fit2 <- glm(default~balance + income, data = Default, 
                family = "binomial", subset = indices)
probs <- predict (log.fit2, type = "response",
                  newdata = Default[-indices, 
                                    c("student","balance","income")])
classification <- ifelse (probs >0.5, "Yes" , "No")
table(classification, Default$default[-indices])
(table(classification, Default$default[-indices])[1,2] + table(classification, Default$default[-indices])[2,1])/5000
## Error = 0.0276
set.seed(3)
indices <- sample(10000, 5000)
log.fit2 <- glm(default~balance + income, data = Default, 
                family = "binomial", subset = indices)
probs <- predict (log.fit2, type = "response",
                  newdata = Default[-indices, 
                                    c("student","balance","income")])
classification <- ifelse (probs >0.5, "Yes" , "No")
table(classification, Default$default[-indices])
(table(classification, Default$default[-indices])[1,2] + table(classification, Default$default[-indices])[2,1])/5000
## Error = 0.0248
set.seed(4)
indices <- sample(10000, 5000)
log.fit2 <- glm(default~balance + income, data = Default, 
                family = "binomial", subset = indices)
probs <- predict (log.fit2, type = "response",
                  newdata = Default[-indices, 
                                    c("student","balance","income")])
classification <- ifelse (probs >0.5, "Yes" , "No")
table(classification, Default$default[-indices])
(table(classification, Default$default[-indices])[1,2] + table(classification, Default$default[-indices])[2,1])/5000
## Error = 0.0262
## The error changes slightly but not by much

# d
set.seed(4)
indices <- sample(10000, 5000)
log.fit2 <- glm(default~balance + income + student, data = Default, 
                family = "binomial", subset = indices)
probs <- predict (log.fit2, type = "response",
                  newdata = Default[-indices, 
                                    c("student","balance","income")])
classification <- ifelse (probs >0.5, "Yes" , "No")
table(classification, Default$default[-indices])
(table(classification, Default$default[-indices])[1,2] + table(classification, Default$default[-indices])[2,1])/5000
# Error becomes 0.274 vs 0.0262...DOesnt seem to improve accuracy

#############
#### 6
############
# a
set.seed (10)
log.fit3 <- glm(default~balance + income , data = Default, 
                family = "binomial")
summary(log.fit3)
# std error (balance) = 2.274*10-4
# std error (income) = 4.985*10-6

# b
library(boot)
boot.fn <- function (data, index)
{
  model <- glm(data$default ~ data$balance + data$income,
               family = "binomial", subset = index)
  return (coef(model))
}

# c
boot(Default, boot.fn, 100)
# std error (balance) = 2.132*10-4
# std error (income) = 4.586*10-6

boot(Default, boot.fn, 1000)
# std error (balance) = 2.248*10-4
# std error (income) = 4.968*10-6
# d The estimates are pretty close.

#######
## 7
#######
# a
head(Smarket)
#Smarket$Dir <- factor(Smarket$Direction, levels = c("Up","Down"), 
#                     labels = c(1,0) )
log.fit4 <- glm (Direction ~ Lag1 + Lag2 , data = Smarket,
                 family = "binomial")
# b
log.fit5 <- glm (Direction ~ Lag1 + Lag2 , data = Smarket,
                 family = "binomial", subset = -1)
# c
#Considers response variable alphabetically. First word is baseline.
predict(log.fit5, Smarket[1, c("Lag1", "Lag2")], type = c("response")) # P(Up) = 0.513 
# this was classified correctly

# d
sum = 0
for (i in 1:nrow(Smarket))
{
  log.fit <- glm (Direction ~ Lag1 + Lag2 , data = Smarket,
                   family = "binomial", subset = -i)
  predictions <- ifelse(predict (log.fit, Smarket [i, c("Lag1", 
                                                        "Lag2")])>0.5, "Up","Down")
  sum <- sum + (predictions != Smarket[i,"Direction"])
}
sum/nrow(Smarket) # Error is 0.52

#######
## 8
#######

# a
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
# n is 100, p is 2
# y = x-2x^2 + error

#b
plot(x,y)

#c
###########################
data_of_interest <- data.frame(cbind(x,y))
names(data_of_interest)<-c("X","Y")
sum<- 0
for (j in 1:100)
{
  model.poly <- lm(Y ~ poly(X,degree = 1, raw=T), data= data_of_interest, subset=-j)
  val <- X[j]
  pred.poly <- predict(model.poly, data.frame(X = val), type="response", data=data_of_interest)
  sum <- sum + (pred.poly - data_of_interest$Y[j])^2  
}
(sum/100) # Error for 1 degree is 7.50
##############
sum<- 0
for (j in 1:100)
{
  model.poly <- lm(Y ~ poly(X,degree = 2, raw=T), data= data_of_interest, subset=-j)
  val <- X[j]
  pred.poly <- predict(model.poly, data.frame(X = val), type="response", data=data_of_interest)
  sum <- sum + (pred.poly - y[j])^2  
}
(sum/100) # Error for 2 degree is 16.02
###################
sum<- 0
for (j in 1:100)
{
  model.poly <- lm(Y ~ poly(X,degree = 3, raw=T), data= data_of_interest, subset=-j)
  val <- X[j]
  pred.poly <- predict(model.poly, data.frame(X = val), type="response", data=data_of_interest)
  sum <- sum + (pred.poly - y[j])^2  
}
(sum/100) # Error for 3 degree is 15.99
###################
sum<- 0
for (j in 1:100)
{
  model.poly <- lm(Y ~ poly(X,degree = 4, raw=T), data= data_of_interest, subset=-j)
  val <- X[j]
  pred.poly <- predict(model.poly, data.frame(X = val), type="response", data=data_of_interest)
  sum <- sum + (pred.poly - y[j])^2  
}
(sum/100) # Error for 4 degree is 15.83
##################################
#################
##### Faster way to do c
#################
##################################
library(boot)
d <- data.frame(x,y)
set.seed(1)
## degree 1
model <- glm(y~x)
cv.glm(d, model)$delta
### degree 2
model <- glm(y~poly(x,2))
cv.glm(d, model)$ delta
### degree 3
model <- glm(y~poly(x,3))
cv.glm(d, model)$ delta
### degree 3
model <- glm(y~poly(x,4))
cv.glm(d, model)$ delta

# e
# Quadratic fits the best

# f
# In all fitted models the quadratic term is the significant one

####
## 9
####
# a)
library(MASS)
head(Boston)
attach (Boston)
mu <- mean(medv) ## 22.53281
# b)
s.e <- sqrt(var(medv))/sqrt(nrow(Boston)) ## 0.409

#c)
library(boot)
bootstrap <- function(x, index)
{
  return (mean(x[index]))
}
boot(medv, bootstrap, 1000) # std error = 0.413

#d)
c(mu-2*0.413, mu+2*0.413) # 21.70681 23.35881
t.test(medv) # 95 percent confidence interval: 21.72953 23.33608

#e)
med <- median(medv) # median = 21.2
med

#f) 
bootstrap.med <-function(x, index)
{
  return (median(x[index]))
}
boot(medv, bootstrap.med, 1000) # std. error = 0.387

#g)
q10<-quantile(medv, 0.1)
q10 # 12.75

#h)
bootstrap.q10 <-function(x, index)
{
  return (quantile(x[index], 0.1))
}
boot(medv, bootstrap.q10, 1000) # std. error = 0.511
