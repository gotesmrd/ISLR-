library (ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket)
cor(Smarket[,-9])
plot(Smarket$Volume)
attach(Smarket)
## Logistic Regression
glm.fit = glm (Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +
                 Volume, data = Smarket, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type="response")
head(glm.probs)
contrasts(Direction)
glm.pred=rep ("Down " ,1250)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred, Direction)
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fit=glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
            data=Smarket ,family =binomial ,subset =train )
glm.probs =predict (glm.fit ,Smarket.2005 , type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred ==Direction.2005)
## Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit , Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
head(lda.pred$x)
## Quadratic discriminant analysis
qda.fit = qda(Direction~Lag1 + Lag2, data=Smarket,
              subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
## K-Nearest Neighbors on stock market data
library (class)
train.X=cbind(Lag1 ,Lag2)[train,]
test.X=cbind (Lag1 ,Lag2)[!train ,]
train.Direction =Direction [train]
set.seed(1)
knn.pred=knn (train.X, test.X, train.Direction, k=1)
table(knn.pred ,Direction.2005)
knn.pred=knn (train.X,test.X,train.Direction ,k=3)
table(knn.pred ,Direction.2005)
## K-Nearest Neighbour on Caravan dataset
dim(Caravan )
attach (Caravan )
summary (Purchase )
head(Caravan[,-86])
standardized.X=scale(Caravan [,-86])
summary(standardized.X)
test =1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed (1)
knn.pred=knn (train.X,test.X,train.Y,k=1)
mean(test.Y!= knn.pred)
