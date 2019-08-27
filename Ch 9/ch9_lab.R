#install.packages("e1071")
## 9.6.1 Support ector Classifier
library(e1071)
set.seed(1)
x = matrix (rnorm(20*2), ncol=2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,] + 1
plot(x, col=(3-y))
dat = data.frame (x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel = "linear", cost=10, scale=F)
plot(svmfit, dat)
svmfit$index
summary(svmfit)
svmfit = svm(y~., data=dat, kernel = "linear", cost=0.1, scale=F)
plot(svmfit, dat)
svmfit$index
set.seed(1)
tune.out <- tune (svm, y~., data=dat, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
xtest = matrix (rnorm(20*2), ncol=2)
ytest = sample (c(-1, 1), 20, rep=T)
xtest[ytest ==1,] = xtest[ytest==1,] + 1
testdat = data.frame (x=xtest, y=as.factor(ytest))
ypred = predict(bestmod, testdat)
table (predict = ypred, truth = testdat$y)
svmfit = svm (y~., data=dat, kernel = "linear", cost=0.01, scale=F)
ypred = predict (svmfit, testdat)
table (predict = ypred, truth = testdat$y)
x[y==1,] = x[y==1,] + 0.5
plot (x, col = (y+5)/2, pch=19)
dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel = "linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit = svm (y~., data=dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)
## 9.6.2 Support Vector Machine
set.seed(1)
x=matrix(rnorm (200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,]-2
y = c(rep(1,150), rep(2,50))
dat = data.frame(x=x, y=as.factor(y))
plot (x, col=y)
train = sample(200,100)
svmfit = svm (y~., data=dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit = svm (y~., data=dat[train,], kernel = "radial", gamma=1, cost=1e5)
plot(svmfit, dat[train,])
set.seed(1)
tune.out = tune (svm, y~., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))
summary(tune.out)
table (true = dat[-train, "y"], pred = predict (tune.out$best.model,
                                                newdata=dat[-train,]))
## 9.6.3 ROC Curves
#install.packages("ROCR")
library(ROCR)
rocplot = function(pred, truth, ...)
{
  predob = prediction (pred, truth)
  perf = performance (predob, "tpr", "fpr")
  plot(perf, ...)
}
svmfit.opt = svm (y~., data=dat[train,], kernel = "radial",
                  gamma = 2, cost =1, decision.values = T)
fitted = attributes (predict(svmfit.opt, dat[train,], decision.values =T))$decision.values

par(mfrow = c(1,2))
rocplot (fitted, dat[train, "y"], main="Training Data")
svmfit.flex = svm (y~., data=dat[train,], kernel = "radial", gamma=50, cost=1,
                   decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train,], decision.values=T))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")
fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"],add=T, col="red")
## 9.6.4 SVM with Multiple Classes
set.seed(1)
x = rbind (x, matrix (rnorm(50*2), ncol=2))
y = c(y, rep(0,50))
x [y==0, 2] = x[y==0, 2] + 2
dat = data.frame (x=x, y=as.factor(y))
par(mfrow = c(1,1))
plot(x, col= (y+1))
svmfit = svm (y~., data=dat, kernel = "radial", cost = 10, gamma = 1)
plot (svmfit, dat)
## 9.6.5 Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table (Khan$ytrain)
table (Khan$ytest)
dat = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out = svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)
dat.te = data.frame (x=Khan$xtest, y=as.factor (Khan$ytest))
pred.te = predict (out, newdata = dat.te)
table (pred.te, dat.te$y)
## online course problem 9.R SVMs in R
library (MASS)
set.seed(1)
## code for radial and linear kernels
errors <- c()
for (i in 1:200)
{
  s1 <- mvrnorm (50, mu = rep (0, 10), Sigma = diag(1, 10,10) )
  y1 <- rep (0, 50)
  s2 <- mvrnorm (50, mu = rep( c(1, 0), c(5,5)), Sigma = diag(1, 10,10) )
  y2 <- rep (1, 50)
  dat <- cbind(rbind (s1, s2), c(y1, y2))
  dat <- data.frame (x = dat[,1:10], y= as.factor (dat[,11]))
  svm.fit <- svm (y~., data=dat, kernel = "linear")
  s1 <- mvrnorm (5000, mu = rep (0, 10), Sigma = diag(1, 10,10) )
  y1 <- rep (0, 5000)
  s2 <- mvrnorm (5000, mu = rep( c(1, 0), c(5,5)), Sigma = diag(1, 10,10) )
  y2 <- rep (1, 5000)
  dat <- cbind(rbind (s1, s2), c(y1, y2))
  dat <- data.frame (x = dat[,1:10], y= as.factor (dat[,11]))
  y_pred <- predict (svm.fit, newdata = dat)
  tt <- table (dat[,"y"], y_pred)
  errors <- c(errors, (tt[2,1] + tt[1,2])/10000)
}
mean(errors) # when n=10, mu = 0.16; n=100, mu = 0.161; n=200, mu=0.162
## code for logistic regression
errors <- c()
for (i in 1:500)
{
  s1 <- mvrnorm (50, mu = rep (0, 10), Sigma = diag(1, 10,10) )
  y1 <- rep (0, 50)
  s2 <- mvrnorm (50, mu = rep( c(1, 0), c(5,5)), Sigma = diag(1, 10,10) )
  y2 <- rep (1, 50)
  dat <- cbind(rbind (s1, s2), c(y1, y2))
  dat <- data.frame (x = dat[,1:10], y= as.factor (dat[,11]))
  log.fit <- glm (y~., data=dat, family = "binomial")
  s1 <- mvrnorm (5000, mu = rep (0, 10), Sigma = diag(1, 10,10) )
  y1 <- rep (0, 5000)
  s2 <- mvrnorm (5000, mu = rep( c(1, 0), c(5,5)), Sigma = diag(1, 10,10) )
  y2 <- rep (1, 5000)
  dat <- cbind(rbind (s1, s2), c(y1, y2))
  dat <- data.frame (x = dat[,1:10], y= as.factor (dat[,11]))
  y_pred <- predict (log.fit, newdata = dat, type = "response")
  y_pred <- ifelse (y_pred >=0.5, 1, 0)
  tt <- table (dat[,"y"], y_pred)
  errors <- c(errors, (tt[2,1] + tt[1,2])/10000)
}
mean(errors) # when n=10, mu = 0.1587; n=100, mu = 0.1587; n=200, mu=0.157
            # when n=500, mu = 0.160
