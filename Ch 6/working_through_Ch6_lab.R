## ^.5 Lab
# 6.5.1
library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(leaps)
regfit.full = regsubsets(Salary~., Hitters)
regfit.summary <-summary(regfit.full)
names(regfit.summary)
regfit.summary$adjr2
par(mfrow=c(1,2))
plot(regfit.summary$rss, type='l')
plot(regfit.summary$adjr2, type='l')
which.max(regfit.summary$adjr2)
points(8, regfit.summary$adjr2[8], col='red',
       cex=2, pch=20)
par(mfrow=c(2,2))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
## 6.5.2 Forward and Backward Stepwise Selection
regfit.fwd <-regsubsets (Salary~., data=Hitters, nvmax=19, method = "forward")
summary(regfit.fwd)
regfit.bwd <-regsubsets (Salary~., data=Hitters, nvmax=19, method = "backward")
summary(regfit.bwd)
coef (regfit.fwd,7)
coef (regfit.full, 7)
coef (regfit.bwd, 7)
## 6.5.3 Choosing among models using the validation set apprach and Cross-Validation
set.seed(1)
train = sample(c(T,F), nrow(Hitters), rep=T)
test = (!train)
regfit.best = regsubsets(Salary~., data = Hitters[train,], nvmax=19)
test.mat = model.matrix(Salary~., data=Hitters[test,])
val.errors = rep(NA, 19)
for (i in 1:19){
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 10)
predict.regsubsets = function (object, newdata, id, ...)
{
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef (object, id=id)
  xvars = names (coefi)
  mat[,xvars] %*%coefi
}
regfit.best = regsubsets (Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)
######### Validation set ^^^^^^^
######### K-fold cross validation VVVVVVVVVVVVV
k=10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace =T)
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))
for (j in 1:k)
  {
    best.fit = regsubsets (Salary~., data=Hitters[folds!=j,],
                           nvmax=19)
    for(i in 1:19){
      pred = predict(best.fit, Hitters[folds==j,], id=i)
      cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
    }
}
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
reg.best = regsubsets(Salary~., Hitters, nvmax=19)
coef(reg.best, id=11)
### 6.6 Lab 2: Ridge Regression and the Lasso
x = model.matrix (Salary~., Hitters)[,-1] ## The -1 gets rid of the Intercept
y = Hitters$Salary
library(glmnet)
grid = 10^seq(10,-2,length=100)
# alpha => (1-??)/2||??||_2^2+??||??||_1 so alpha =0 is rige regression, 1 is lasso
ridge.mod = glmnet(x,y,alpha=0, lambda = grid)
dim (coef(ridge.mod)) # each column is a model, # rows corresponds to number of alphas
names(ridge.mod)
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
predict(ridge.mod, s=50, type="coefficients")[1:20,]
## creating a validation set
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh=1e-12)
ridge.pred = predict (ridge.mod, s=4, newx = x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train]) - y.test)^2)
ridge.pred = predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred = predict (ridge.mod,  s=0, newx = x[test,], x = x[train,], y = y[train],exact = T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, x=x[train,], y=y[train], s=0, exact=T, type = "coefficients")[1:20,]
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)
out = glmnet(x,y,alpha=0)
predict (out, type="coefficients", s=bestlam)[1:20,]
## 6.6.2 The Lasso
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet (x,y,alpha=1)
predict(out, type = "coefficients", s = bestlam)[1:20,]
## 6.7.1 Principal Component Regression
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=T, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
set.seed(1)
pcr.fit = pcr(Salary~.,data=Hitters, subset=train,
              scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit = pcr(y~x, scale=T, ncomp=7)
summary(pcr.fit)
## 6.7.2 Partial Least Squares
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train,
               scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit)
pls.pred = predict(pls.fit, x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~.,data=Hitters, scale=T, ncomp=2)
summary(pls.fit)
