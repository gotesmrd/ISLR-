## Ch 9 Exercises
## 2
## (a)
x1 <- seq(-4,4,0.1)
## x2 <= 2 +- sqrt (4-(1+x1)^2)
y1 <- 2 + sqrt (4-(1+x1)^2)
y2 <- 2 - sqrt (4-(1+x1)^2)
plot (y2 ~ x1, type='b',xlim = c(-4,4), ylim = c(-4,4))
lines (y1 ~ x1)
## 4
library(e1071)
set.seed(1)
x <- matrix (rnorm(100*2), ncol=2)
y <- c(rep(0,25), rep(1,50), rep(0,25))
x [1:25,] = x[1:25,] + 2.5
x [76:100,] = x[76:100,] - 2.5
plot (x, col = (3-y))
train = sample(1:100, 80)
test = -train
dat = data.frame(x=x, y= as.factor(y))
## Linear
svm.linear <- svm (y~., dat, subset = train, kernel="linear")
plot(svm.linear, dat[train,])
preds <- predict (svm.linear, newdata = dat[test,])
table (dat[test, "y" ], preds)
## Error is 7/20 = 35%
## Radial
svm.radial <- svm (y~., dat, subset = train, kernel="radial", gamma=0.1 )
plot(svm.radial, dat[train,])
preds <- predict (svm.radial, newdata = dat[test,])
table (dat[test, "y" ], preds)
## Error is 1/20 = 5%
## Polynomial
svm.poly <- svm (y~., dat, subset = train, kernel="polynomial", degree=2 )
plot(svm.poly, dat[train,])
preds <- predict (svm.poly, newdata = dat[test,])
table (dat[test, "y" ], preds)
## Error is 1/20 = 5%
##
## 5
##
## (a)
x1 <- runif (500) - 0.5
x2 <- runif (500) -0.5
y <- 1* (x1^2 - x2^2 > 0)
## (b)
plot (x1, x2, col = (3-y))
## (C)
tab <- data.frame (x1, x2, y)
log.fit <- glm (y ~ x1+x2, family = "binomial", data = tab)
## (d)
set.seed(1)
train <- sample (1:500, 300)
test <- -train
log.fit1 <- glm (y~x1+x2, family = "binomial", data = tab, subset = train)
y_pred <- predict (log.fit1, type = "response")
y_pred <- ifelse (y_pred >0.5, 1, 0)
plot (x1[train], x2 [train], col = (3-y_pred)) ## it's a linear decision boundary
## (e-f)
log.fit2 <- glm (y~x1+x2+ I(x1^2)+ I(x2^2), family = "binomial", data = tab, subset = train)
y_pred <- predict (log.fit2, type = "response")
y_pred <- ifelse (y_pred >0.5, 1, 0)
plot (x1[train], x2 [train], col = (3-y_pred)) ## Decision boundary is quadratic
## (g)
library (e1071)
dat <- data.frame (x1 = x1, x2 = x2, y = as.factor(y))
svm.fit <- svm (y~., data = dat, subset = train, kernel = "linear")
plot (svm.fit, dat [train,])
summary(svm.fit)
y_pred <- predict (svm.fit, data = dat[train,])
table (dat[train, "y"], y_pred) ## Error = 136/300 = 45%
## (h)
svm.fit <- svm (y~., data = dat, subset = train)
plot (svm.fit, dat [train,])
y_pred <- predict (svm.fit, data = dat[train,])
table (dat[train, "y"], y_pred)  ## Error = 11/300 = 3.66% => Much better
## 6
## (a)
set.seed(2)
x <- matrix( rnorm (100*2), ncol = 2 )
y <- c(rep (0,50), rep (1, 50))
x[y==1,] = x[y==1,] + 3.3
plot (x,col = (3-y))
## (b-c)
train <- sample (1:100, 70)
test <- -train
dat <- data.frame (x=x, y=as.factor(y))
models <- tune (svm , y~., data = dat[train,], 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 2, 5, 10, 100)))
y_pred <- predict (models$best.model, newdata = dat[test,])
table (dat[test,"y"], y_pred)
models$performances[,2]*70 ## how many were misclassified
## 7
library(ISLR)
library(e1071)
m <- median (Auto$mpg)
Auto$milage <- as.factor(ifelse (Auto$mpg >=m, 1, 0))
head(Auto)
## (b)
train <- sample (1:392, 250)
cc <- c(0.001,0.01,0.1,1,5,10,100)
models <- tune (svm, milage ~.-mpg, data=Auto[train, ], kernel="linear",
                ranges = list (cost = cc))
summary(models) ## cost of 0.01 gives lowest error = 0.108
## (c)
gamma <- c(0.001, 0.01, 0.1, 1, 5, 10, 100)
models <- tune (svm, milage ~.-mpg, data=Auto[train, ], kernel="radial",
                ranges = list (cost = cc, gamma = gamma))
summary (models) # cost = 5, gamma = 0.1, error = 0.104
deg = c(1, 2,3,4,5,6,7)
models <- tune (svm, milage ~.-mpg, data=Auto[train, ], kernel="polynomial",
                ranges = list (cost = cc, degree = deg))
summary (models) # cost 0.1, d = 1, error = 0.108
## (d)
model <- svm (milage~.-mpg, data = Auto [train,] , kernel="radial",
              cost=5, gamma = 0.1)
plot (model, Auto[train,], cylinders~displacement, slice = 
        list(mpg = 5,  horsepower=5, weight=5,acceleration=5, year=70, origin=1,
             name = "ford torino"))## weird error happening here i cant figure...moving on
summary(Auto$cylinders)
table(is.na(Auto$cylinders))
data(iris)
names(Auto)
## 8
## (a)
attach (OJ)
dim(OJ)
train <- sample (1:1070, 800)
test <- -train
OJ$Purchase <- as.factor (OJ$Purchase)
## (b)
model <- svm (Purchase ~., OJ[train,], kernel = "linear", cost = 0.01)
summary(model) ## U have 438 support vectors
## (c)
y_pred <- predict (model)
table (OJ[train, "Purchase"], y_pred) ## Train error = 129/800 = 0.16
y_pred <- predict (model, newdata = OJ[test,])
table (OJ[test, "Purchase"], y_pred) ## Train error = 42/270 = 0.156
## (d)
models <- tune (svm, Purchase~., data= OJ[train,], kernel = "linear", ranges = 
                  list (cost = c(0.01, 0.05,0.1, 1,2,5,10)))
summary(models) ## cost = 0.1 is best, error = 0.1625
y_pred <- predict (models$best.model)
table (OJ[train,"Purchase"], y_pred) ## train error is 0.1675
#    y_pred
#    CH  MM
#CH 426  63
#MM  71 240
y_pred <- predict (models$best.model, newdata = OJ[test,])
table (OJ[test,"Purchase"], y_pred)  ## test error is 0.159
#    y_pred
#    CH  MM
#CH 143  21
#MM  22  84
## (f-h) are the same as (e)