library(ISLR)
##### 8a
lm.fit1 = lm (mpg ~ horsepower, data = Auto)
summary (lm.fit1)
par(mfrow = c(2,2))
plot(lm.fit1)
predict(lm.fit1, data.frame(horsepower = c(98)), interval = "confidence")
## i) Yes there is a relationship
## ii) Multiple R-squared:  0.6059
## iii) Negative
## iv)  fit      lwr      upr
##    24.46708 23.97308 24.96108
## v)
par(mfrow=c(1,1))
plot(Auto$horsepower, Auto$mpg)
abline(lm.fit1, col = "red")
par(mfrow = c(2,2))
plot(lm.fit1)
## vi) There is nonlinearity in the data and maybe some outliers
######################
### 9
## a) 
pairs(Auto)
## b)
cor(Auto[,1:8])
## c)
lm.fit2 = lm (mpg ~ .-name, data = Auto)
summary(lm.fit2)
## i. Yes there is a relationship
## ii. displacement, weight, year, origin all apear significant
## iii. For every 1 increase in year as all other variables are held constance
## mpg increases by 0.751 and this relatinship is statistically significant
## d) Residual plot suggests non-linearity of data.
## There are some outliers. Perhaps point 14 has high leverage
plot(lm.fit2)
## e)
lm.fit2 = lm (mpg ~ .-name+year:origin, data = Auto)
summary(lm.fit2)
## Year and origin appear to have an interaction
## f)
lm.fit2 = lm (mpg ~ .-name -year - origin+ I(sqrt(year))+
                + I(sqrt(origin)), data = Auto)
summary(lm.fit2)
plot(lm.fit2)
## I didn't notice anything haha
################3
## 10
## a)
lm.fit3 = lm (Sales~Price + Urban + US, data = Carseats)
summary(lm.fit3)
## d) For Price and US
## e)
lm.fit4 = lm(Sales~Price + US, data=Carseats)
summary(lm.fit4)
## f) For (a) R^2 = 0.2393. For (e) R^2 = 0.2393. Basically the same
## g)
confint(lm.fit4)
## h)
plot(lm.fit4)
## I dont think there is any leverage or outlier problems
########
## 11
set.seed(1)
x = rnorm(100)
y = 2*x+rnorm(100)
lm.fit5 = lm (y~x + 0)
summary(lm.fit5)
## a) Beta = 1.9939, std. err = 0.1065, t-test = 18.73, p-val = <2e-16
## b) Beta = 0.3911, std. err = 0.0209, t-test = 18.73, p-val = <2e-16
lm.fit6 = lm (x~y + 0)
summary(lm.fit6)
par(mfrow=c(1,2))
plot(x,y)
abline(lm.fit5)
plot(y,x)
abline(lm.fit6)
## 16
set.seed (1)
x1=runif (100)
x2 =0.5* x1+rnorm (100) /10
y=2+2* x1 +0.3* x2+rnorm (100)
cor(x1,x2)
plot(x1,x2)
lm.fit7 = lm (y~x1+x2)
summary(lm.fit7)
lm.fit8 = lm(y~x1)
summary(lm.fit8)
