# 6.8 Exercises
### Conceptual
# 1 
# (a) The best subset selection method has lowest training RSS
# (b) Impossible to say
# (c) i. True
#     ii. True
#     iii. False
#     iv. False
#     v. False
# 2
# (a) i. Incorrect
#     ii. Incorrect
#     iii. Correct
#     iv. Incorrect
# (b) i. Incorrect
#     ii. Incorrect
#     iii. Correct
#     iv. Incorrect
# (c) i. Incorrect
#     ii. Correct
#     iii. Incorrect
#     iv. Incorrect
# 3. This is an example of lasso regression
# (a) iv
# (b) ii
# (c) iii
# (d) iv
# (e) v
# 4. This is an example of ridge regression
# (a) iii
# (b) ii
# (c) iv
# (d) iii
# (e) v
# 5. 
# (a) Minimize: J = Sigma {j=1 .. n} (y_j - b0 - Sigma {i=1..p} bi*xji)^2 + lambda*Sigma {i=1..p} bi^2
# (y1- 0 - b1*x11 - b2*x12)^2 + (y2 - 0 -b1*x21-b2*x22)^2 + lambda * b1^2 + lambda * b2^2
# (y1-b1*x11-b2x11)^2 + (y2-b1*x21-b2*x22)^2 + lambda * b1^2 + lambda *b2^2
# (y1-b1*x11-b2*x11)^2 + (y2+b1*x11+b2*x12)^2 + lambda * b1^2 + lambda*b2^2
# (y1-b1*x11-b2*x11)^2 + (y1-b1*x11-b2*x11)^2 + lambda * b1^2 + lambda*b2^2
# J = 2(y1-b1*x11-b2*x11)^2 + lambda * b1^2 + lambda*b2^2   ===>
# dJ/db1 = 2(y1-b1x11-b2x11)(-x11) + lambda*b1 = 0 ===>
# 2x11*y1-2b1*x11^2-2b2*x11^2 = lambda * b1 ===>
# b1 = (2x11*y1 - 2b2*x11^2)/(lambda + 2*x11^2)
####### ^^ Couldn't figure it out
# 8
# (a) 
set.seed(1)
X = rnorm(100)
epsilon = rnorm (100) 
# (b)
beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1*X + beta2*X^2 + beta3*X^3 + epsilon
# (c)
library(leaps)
tables<-data.frame(X,Y)
pred <- regsubsets(Y~poly(X, 10, raw=T), data=tables, nvmax=10)
pred.sum <- summary(pred)
which.min(pred.sum$cp)
which.min(pred.sum$bic)
which.max(pred.sum$adjr2)
par(mfrow=c(2,2))
plot(pred.sum$cp, type='b')
plot(pred.sum$bic, type='b')
plot(pred.sum$rss, type='b')
plot(pred.sum$adjr2, type='b')
## Looks like around 3-4 variabls is ideal ... 3 is pretty equivalent to 4 but simpler
coef(pred, id=3)
# b0 =3.08, b1=2.36, b2=-3.17, b7=0.01
## (d)
## Forward stepwise selection
pred <- regsubsets(Y~poly(X,10, raw=T), data=tables, nvmax=10, method = "forward")
pred.sum <- summary(pred)
which.min(pred.sum$cp)
which.min(pred.sum$bic)
which.max(pred.sum$adjr2)
par(mfrow=c(2,2))
plot(pred.sum$cp, type='b')
plot(pred.sum$bic, type='b')
plot(pred.sum$rss, type='b')
## We'll go with a 3 variable model again
coef(pred, id=3)
# Same variables make it in. b0, b1, b2 and b3 the same
### Backwards selection
pred <- regsubsets(Y~poly(X,10,raw=T), data=tables, y=Y, nvmax=10, method = "backward")
pred.sum <- summary(pred)
which.min(pred.sum$cp)
which.min(pred.sum$bic)
which.max(pred.sum$adjr2)
par(mfrow=c(1,1))
plot(pred.sum$cp, type='b')
plot(pred.sum$bic, type='b')
plot(pred.sum$rss, type='b')
coef(pred, id=3)
# Again 3 model is best but instead of b7 the b9 coefficient is used
## (e)
library(glmnet)
y_var <-tables$Y
x_var <-tables$X
lass_pred <- glmnet (x = poly(x_var, 10, raw=T), y = y_var, alpha=1)
names(lass_pred)
cv.pred <- cv.glmnet(x = poly(x_var, 10, raw=T), y = y_var, alpha=1)
plot(cv.pred)
coef(cv.pred)
summary(cv.pred)
cv.pred$lambda.min
## (f)
## The lasso fit
beta7 = 7
Y=beta0 + beta7*X^7 + epsilon
cv.pred <- cv.glmnet(x=poly(X, 10, raw=T), y=Y, alpha=1)
plot(cv.pred)
cv.pred$lambda.min
cv.final <- glmnet(x=poly(X, 10, raw=T), y=Y, alpha = 1,
                   lambda = cv.pred$lambda.min)
predict(cv.final, type="coefficients")
predict(cv.final, s=cv.pred$lambda.min, type="coefficients")
## The best subset fit
pred.subset <- regsubsets(Y~poly(X,10,raw=T),data=tables, nvmax=10)
names(summary(pred.subset))
which.min(summary(pred.subset)$bic)
coef(pred.subset, id=3)
## 9.
# (a)
library(ISLR)
set.seed(11)
locs <- sample (1:nrow(College), nrow(College)/2)
training_set <- College[locs,]
test_set <- College[-locs,]
# (b)
head(test_set[,-2])
training.pred <- lm (Apps ~., data=training_set)
mean((test_set$Apps - predict (training.pred, newdata=test_set[,-2]))^2)
# (c)
library(glmnet)
grid = 10 ^ seq(4, -2, length=100)
mat <- model.matrix(Apps ~., data=training_set)
ridge.pred <- cv.glmnet (y=training_set$Apps, x=mat, alpha=0, lambda = grid, thresh=1e-12)
plot(ridge.pred)
ridge.pred$lambda.min
mean((test_set$Apps - predict(ridge.pred, newx=model.matrix(Apps ~., data=test_set), s="lambda.min"))^2)
## (d)
lasso.pred <- cv.glmnet (y=training_set$Apps, x=mat, alpha=1, lambda = grid, thresh = 1e-12)
plot(lasso.pred)
lasso.pred$lambda.min ## We pick the model with 5 parameters
mean((test_set$Apps - predict(lasso.pred, newx =model.matrix(Apps ~., data=test_set), s="lambda.min"))^2)
## (e)
library(pls)
pcr.mode <- pcr (Apps~., data=training_set, scale=T, validation="CV")
names(pcr.mode)
validationplot(pcr.mode, "MSEP") # Looks like u get the lowest around 5
pcr.pred <- predict(pcr.mode, test_set[,-2], ncomp=10)
mean((pcr.pred - test_set[,2])^2)
## (f)
plsr.mod <- plsr (Apps ~., data=training_set, scale=T, validation="CV")
validationplot(plsr.mod, "MSEP")
summary(plsr.mod) ## looks like 9 is ideal
mean((test_set[,2] - predict (plsr.mod, test_set[,-2], ncomp = 9))^2)
##########
## 10
####
## a
set.seed(34)
data_frame <-c()
for (i in 1:20){
data_frame <- cbind(data_frame,rnorm(1000))
}
err <- rnorm(1000)
beta <- c( 1,0, 0, 4,5,6,7,0,9, 10,
           4,3, 2.3, 5.4,1.5,2.6,1.7,0,9.1, 1.30)
Y = as.matrix(data_frame) %*% beta + err
## b
s <- sample (1:1000, 100)
## c
library(leaps)
data_frame <-data.frame(data_frame)
data_frame2 <-cbind(data_frame, Y)
model <- regsubsets(Y ~ ., data = data_frame2[s,],
                    nvmax=20)
names(summary(model))
plot(summary(model)$rss/100, type='b')
which.min(summary(model)$rss) ## 20 ... biggest model
## d
errors <-c()
for (i in 1:20)
{
  coefficient <- coef(model, i)
  print(i)
  print(coefficient)
  print(names(coefficient)) #names(coefficient)[2:length(names(coefficient))]
  print(head(cbind(rep(1,900), data_frame[-s,names(coefficient)[-1]])))
  #temp_error <- 
  #  mean((Y[-s] - 
  #     (as.matrix(cbind(rep(1,900), data_frame[-s,names(coefficient)[-1]]))%*%coefficient))^2)
  temp_error <- 
    mean((Y[-s] - 
            (as.matrix(data_frame[-s,names(coefficient)[-1]])%*%coefficient[-1]))^2)
  
  errors <- rbind (errors, temp_error)
}
plot(errors, type='b')
which.min(errors)
## e
## 16 parameters is the best! 
coef(model, 16)
## f
## It doesnt have any of the 0 terms
## g
weird_sum<- c()
for (r in 1:20)
{
  beta_r <- (coef(model, r)[-1])
  add_them <- beta [names(data_frame) %in% names(beta_r)] - beta_r
  print(add_them)
  print(t(add_them))
  length(beta_r)
  length(add_them)
  weird_sum <- rbind (weird_sum, sqrt(sum((add_them)^2)))
}
plot(weird_sum)
which.min(weird_sum) ## the model with 15 (16 with intercept)
                     ## minimized error of paramters, but 16 (with intercept) is best for 
                     ## minimizing test error ... so they are not the same
### 11
# (a)
library(ISLR)
library(MASS)
library(leaps)
dim(Boston)
sa <- sample(506, 200)
## best subset selection
model.train <- regsubsets(crim ~., data=Boston[s,], nvmax = 13)
plot(summary(model.train)$rss)
errors <- c()
for (i in 1:13)
{
  coefficient <- coef(model.train, i)
  #print(names(coefficient)[-1])
  MSE <- mean((Boston$crim[-s] - as.matrix(cbind(rep (1, 306), Boston [-s, names(Boston) %in%
                                                      names(coefficient)[-1]])) %*% coefficient)^2)
  errors <- rbind (errors, MSE)
}
plot(errors, type='b') ## 7 gives the best model
# Included variables are: zn, nox, rm, dis, rad, lstat, medv
print(coef(model.train, 7))
### now let's try the lasso
library(glmnet)
X <- model.matrix (crim ~., data=Boston)
Y <- Boston$crim
model.lasso.train <- cv.glmnet(x= X[sa,], y=Y[sa], alpha = 1) # lasso
plot(model.lasso.train)
min_lambda <- model.lasso.train$lambda.min
## the MSE is 35.95
mean((Y[-sa] - predict (model.lasso.train, newx = X[-sa,],s= min_lambda))^2)
### YOU NEED TO FIT THE ENTIRE MODEL NOW
grid = 10^seq(10,-2,length=100)
out <- glmnet(X,Y, alpha=1, lambda = min_lambda)
predict (out, type = "coefficients", s=min_lambda)
## For the best model incldude variables were:
# zn, indus, chas, rm, dis, rad, ptratio, black, lstat, medv
coef (out, s=min_lambda)
predict()
### Now let's try ridge regression
model.ridge.train <- cv.glmnet(x= X[sa,], y=Y[sa], alpha =0) # lasso
plot(model.ridge.train)
best_lambda <- model.ridge.train$lambda.min
# The MSE is 39.50
mean((Y[-sa] - predict (model.ridge.train, newx = X[-sa,], s = best_lambda))^2)
model.ridge.fin <- glmnet (x =X, y = Y, alpha=0, lambda = grid )
# all variables are used in the model
predict (model.ridge.fin, type = "coefficients", s = best_lambda)
### Let's try PCR
library(pls)
pcr.train <- pcr(Y~., data=Boston, subset = sa)
validationplot(pcr.train, val.type = "MSEP")
## The MSE is 56.92
mean((Y[-sa] - predict(pcr.train, newdata = X[-sa,],ncomp = 6))^2)
summary(pcr.train)
## Looks like the best model is lasso which includes only some, not all parameters
