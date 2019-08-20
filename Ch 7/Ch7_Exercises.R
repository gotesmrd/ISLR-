## 4
X = -2:2
Y = 1 + 1*I(X<=2 & X>=0)-(X-1)*I(X<=2 & X>=1) + 3*((X-3)*I(X>=3 & X<=4)+
                                                     I(X<=5 & X>4))
plot(X,Y, type='b')                                                   
## 6
## (a)
library (ISLR)
library(boot)
head(Wage)
set.seed(1)
cv.errors <-c()
for (i in 1:10)
{
  lm.fit <- glm(wage ~ poly(age, i, raw=T), data=Wage)
  a <- cv.glm(Wage, lm.fit,K=10)
  cv.errors <- cbind(cv.errors, a$delta[2])
  print(a$delta)
}
plot(1:10,cv.errors,type='l')
## 4 leads to the best
lm.fit1 <- lm(wage~poly(age, 1, raw=T), data=Wage)
lm.fit2 <- lm(wage~poly(age, 2, raw=T), data=Wage)
lm.fit3 <- lm(wage~poly(age, 3, raw=T), data=Wage)
lm.fit4 <- lm(wage~poly(age, 4, raw=T), data=Wage)
lm.fit5 <- lm(wage~poly(age, 5, raw=T), data=Wage)
anova(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5)
anova(lm.fit1, lm.fit5)
summary(lm.fit5)
plot(Wage$age, Wage$wage, col="gray", alpha=0.5)
lm.final <- lm(wage~poly(age,3,raw = T), data=Wage)
min_age <-min(Wage$age)
max_age <-max(Wage$age)
predictions <- predict.lm(lm.final,newdata=data.frame(age=seq(from=min_age,to=max_age)),se=T )
lines(x=seq(min_age,max_age),predictions$fit,col="red")
lines(x=seq(min_age,max_age), predictions$fit + 2*predictions$se.fit, col="blue")
lines(x=seq(min_age,max_age), predictions$fit - 2*predictions$se.fit, col="blue")
## (b)
set.seed(1)
cv.error <- rep (NA, 10)
for (i in 2:10)
{
  Wage$tmp <- cut(x=Wage$age, breaks=i)
  lm.fit <- glm(wage~tmp, data=Wage)
  cv.error [i] <- cv.glm (data=Wage, glmfit = lm.fit, K=10)$delta[2]
  print(i)
}
cv.error[1] <- mean((Wage$wage-mean(Wage$wage))^2)
cv.error
plot(1:10,cv.error,type='b')
plot(Wage$age, Wage$wage)
Wage$tmp <- cut(Wage$age, 6)
lm.final <- lm(wage~tmp, data=Wage)
pred<-predict.lm(lm.final, newdata = 
                   data.frame(tmp=cut(seq(from=min(Wage$age), 
                                          to=max(Wage$age),length.out = 3000),breaks=6)),se.fit = T)
                 #newdata = data.frame(tmp=cut(seq(from=min(Wage$age), to=max(Wage$age),length.out=3000),6)),se.fit=T)
lines(x=(seq(from=min(Wage$age), to=max(Wage$age),length.out = 3000)),y= pred$fit, col="red")
lines(x=(seq(from=min(Wage$age), to=max(Wage$age),length.out = 3000)),y= pred$fit+2*pred$se.fit, col="blue")
lines(x=(seq(from=min(Wage$age), to=max(Wage$age),length.out = 3000)),y= pred$fit-2*pred$se.fit, col="blue")
## 7 ... I'll skip it
pairs(wage~maritl+jobclass+education+health+race, data=Wage)
pairs(c(Wage$maritl, Wage$jobclass, Wage$wage))
## 8
attach(Auto)
pairs(mpg~.,data=Auto)
lm.fit1 <- lm(mpg~displacement, data=Auto)
lm.fit2 <- lm(mpg~poly(displacement,2,raw=T), data=Auto)
lm.fit3 <- lm(mpg~poly(displacement,3,raw=T), data=Auto)
lm.fit4 <- lm(mpg~poly(displacement,4,raw=T), data=Auto)
anova(lm.fit1,lm.fit2,lm.fit3,lm.fit4)
lm.fit.poly <- lm(mpg~poly(displacement,3,raw=T), data=Auto)
summary(lm.fit.poly)
lm.fit.poly1 <- lm(mpg~poly(displacement,3,raw=T)+cylinders, data=Auto)
summary(lm.fit.poly1)
anova(lm.fit2,lm.fit3)
library(splines)
lm.fit.ns <- lm(mpg~bs(x=displacement,df = 5))
summary(lm.fit.ns)
#plot(lm.fit.ns)
displacement.grid = seq(from=min(displacement),to=max(displacement))
pred<-predict(lm.fit.ns, newdata = data.frame(displacement = displacement.grid))
plot(displacement,mpg)
lines(displacement.grid,pred,col="red")
#install.packages("gam")
library(gam)
lm.gam <- gam(mpg ~ s (displacement, df=5) + s(acceleration,df=4)+cylinders+
                lo(horsepower,span = 0.5))
par(mfrow=c(1,4))
plot(lm.gam, se=T)
typeof(cylinders)
## 9
## (a)
library(MASS)
attach(Boston)
par(mfrow=c(1,1))
plot(dis,nox)
lm.poly <- lm(nox ~ poly(dis, 3, raw=T))
summary(lm.poly)
dis.grid = seq(from=min(dis)*1000, to=max(dis)*1000)/1000
pred <- predict (lm.poly, newdata = data.frame(dis=dis.grid),se.fit = T)
lines(x = dis.grid, pred$fit, col="red")
lines(x = dis.grid, pred$fit + 2*pred$se.fit, col="blue")
lines(x = dis.grid, pred$fit - 2*pred$se.fit, col="blue")
## (b)
res_ss <- rep(NA,10)
for (i in 1:10)
{
  lm.fit <- lm(nox ~ poly (dis, i, raw=T))
  res_ss [i] <- sum ( (dis - predict(lm.fit))^2)
  lines(dis.grid,predict(lm.fit, newdata = data.frame(dis=dis.grid)),col=i)
}
res_ss
## (c)
set.seed(1)
cv.errors <- rep(NA, 10)
for (i in 1:10)
{
  lm.fit <- glm (nox ~ poly(dis, i, raw=T))
  cv.errors[i] <- cv.glm (Boston, lm.fit, K = 10)$delta[2]
}
cv.errors
plot(1:10,cv.errors,type='b')
## 3 is best bc its simplest model with error in proximity of more complicated models
## (d)
lm.fit <- lm (nox ~ bs (dis, df=4))
plot(dis, nox)
lines(dis.grid, predict(lm.fit, newdata = data.frame(dis=dis.grid)), col="red")
lm.fit2 <- lm (nox ~ bs (dis, knots=c(7)))
lines(dis.grid, predict(lm.fit2, newdata = data.frame(dis=dis.grid)), col="blue")
lm.fit3 <- lm (nox ~ bs (dis, knots=c(5)))
lines(dis.grid, predict(lm.fit3, newdata = data.frame(dis=dis.grid)), col="green")
## (e)
RSS_errors <- rep (NA, 10)
plot(dis, nox)
for (i in 1:10)
{
  lm.fit <- lm (nox ~ bs (dis, df=i))
  RSS_errors [i] <- sum((nox - predict(lm.fit))^2)
  lines (dis.grid, predict (lm.fit, newdata = data.frame(dis = dis.grid)), col=i)
}
RSS_errors
## 10 df is the best
## (f)
set.seed(1)
cv.error <- rep(NA, 10)
for (i in 3:10)
{
  lm.fit <- glm (nox ~ bs (dis, df=i))
  cv.error[i] <- cv.glm (Boston, lm.fit, K=10)$delta[2]
}
cv.error
plot(3:10, cv.error[3:10],type='b')
which.min(cv.error)
## 10
## (a)
attach (College)
#install.packages("leaps")
library(leaps)
set.seed(1)
spot_i <- sample(1:777, size=500)
forstepwise <- regsubsets(Outstate~., method="forward", data=College[spot_i,],nvmax = 20 )
summary(forstepwise) ## there are 17 variables
cv.errors <- rep (NA, 17)
summary(forstepwise)$adjr2
summary(forstepwise)$rss/500
which.max(summary(forstepwise)$adjr2)
coef(forstepwise, id=4) ## models 11-13 are the lowest, 4 seems high adjr2 but not too complicated
## (b)
library (gam)
pairs(Outstate~Private+Room.Board+perc.alumni+Expend, data=College)
lm.fit <- gam (Outstate ~ Private + Room.Board + s(perc.alumni, df = 4) + lo(Expend, span=0.5), data = College[spot_i,])
par(mfrow = c(2,2))
plot(lm.fit, se=T)
## (c)
yhat <- predict(lm.fit, newdata = College[-spot_i,c("Private", "Room.Board", "perc.alumni","Expend")])
rss <-mean((College[-spot_i,"Outstate"] - yhat)^2)
tss <- mean((College[-spot_i, "Outstate"] - mean(College[-spot_i, "Outstate"]))^2)
1-rss/tss
## (d)
summary(lm.fit)
## 11
## (a)
X1 <- rnorm (100)
X2 <- rnorm (100)
err <- rnorm (100)
b0 <- 5; b1=7; b2 = 10;
Y = b0 + b1*X1 + b2*X2
df <- cbind(Y,X1,X2)
head(df)
## (b)
b1_hat <- 92
## (c)
a = Y - b1_hat*X1
b2_hat <- lm(a~X2)$coef[2]
## (d)
a = Y-b2_hat*X2
b1_hat <- lm(a~X1)$coef[2]
b0_hat <- lm(a~X1)$coef[1]
## (e)
par(mfrow = c(1,1))
betas <-c(b0_hat, b1_hat, b2_hat)
for (i in 1:1000)
{
  a = Y - b1_hat*X1
  b2_hat <- lm(a~X2)$coef[2]
  a = Y-b2_hat*X2
  b1_hat <- lm(a~X1)$coef[2]
  b0_hat <- lm(a~X1)$coef[1]
  betas <-rbind (betas, c(b0_hat, b1_hat, b2_hat))
  
}
matplot(1:5, betas[1:5,], col = 1:3, type="l")
## (f)
df <- data.frame(Y,X1,X2)
lm.fit <- lm(Y~., data= df)
b0_est <- lm.fit$coefficients[1]
b1_est <- lm.fit$coefficients[2]
b2_est <- lm.fit$coefficients[3]
abline (h = b0_est)
abline (h = b1_est)
abline( h= b2_est)
## (g) 2 steps were needed....
## 12.

