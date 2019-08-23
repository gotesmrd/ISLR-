### Ch8 Exercises
## 3
p = seq(0, 100)/100
gini = p*(1-p) + (1-p)*p
class_err = ifelse (p<=0.5, 1-p,p)
cross_ent = -(p*log10(p) + (1-p)*log10(1-p))
matplot (p, cbind(gini,cross_ent, class_err))
## 5
v <-c(0.1,0.15,0.2,0.2,0.55,0.6,0.6,0.65,0.7,0.75)
mean(v)
## 7
library(randomForest)
library(MASS)
attach(Boston)
dim(Boston)
set.seed(999)
train <- sample(1:nrow(Boston), 300)
num_trees <- seq(100,500,100)
num_mtry <- seq (1,13,2)
results <- c()
final_boss <-c()
for (i in 1:length(num_trees))
{
  results <-c()
  for (j in 1:length(num_mtry))
  {
    trees.random <- randomForest(medv ~. , data=Boston, subset=train,
                     ntree = num_trees[i], mtry = num_mtry[j])
    y_hat <- predict(trees.random, newdata = Boston[-train,])
    err <- mean ((y_hat - Boston[-train, "medv"])^2)
    results <- rbind (results, err)
  }
  final_boss <-cbind(final_boss, results)
}
head(t(final_boss)) ## mtry = 9, num_tree = 100 and 500 are best
head(num_trees)
matplot(num_trees, t(final_boss), type="l")
legend (100, 18, c("1", "3", "5",
                   "7","9","11","13"), ncol=3)
## 8
library(ISLR)
attach(Carseats)
## (a)
dim(Carseats)
train <- sample (1:nrow(Carseats), 200)
test <- -train
## (b)
library(tree)
reg_tree <- tree(Sales ~., data=Carseats, subset = train )
plot(reg_tree)
text(reg_tree)
y_hat <- predict (reg_tree, newdata = Carseats[test,])
mean((Carseats[test, "Sales"] - y_hat)^2) ## MSE = 4.66
## (c)
cv.reg.tree <- cv.tree(reg_tree)
plot(cv.reg.tree$size, cv.reg.tree$dev, type='b')
cv.final.tree <- prune.tree (reg_tree, best = 10)
y_hat <- predict(cv.final.tree, newdata = Carseats[test,])
mean((Carseats[test, "Sales"] - y_hat)^2) ## MSE = 4.84 ... didn't improve it
## (d)
library(randomForest)
set.seed(901)
bag_tree <- randomForest(Sales~., data=Carseats, subset = train,
                         mtry = 10, importance=T, ntree=500)
y_hat <- predict(bag_tree, newdata = Carseats[test,])
mean((Carseats[test, "Sales"] - y_hat)^2) ## MSE = 2.45
importance(x=bag_tree, type=1) ## ShelveLoc and Price most important
varImpPlot(x=bag_tree)
## (e)
rand_tree <- randomForest (Sales~., data=Carseats, subset = train,
                           mtry = sqrt(10), importance=T, ntree=500)
y_hat <- predict(rand_tree, newdata = Carseats[test,])
mean((Carseats[test, "Sales"] - y_hat)^2) ## MSE = 2.9
importance(rand_tree)
varImpPlot(rand_tree)## ShelveLoc and Price most important
## Looks like as m (Num of variables in each split) 
## decreased error was stable
## 9
attach(OJ)
dim(OJ)
names(OJ)
## (a)
set.seed(1013)
train <- sample (1:1070, 800)
test <- -train
## (b)
reg_tree <- tree(Purchase~., data = OJ, subset = train)
summary(reg_tree) ## training error = 0.169
plot(reg_tree) ## 7 terminal nodes
text(reg_tree, pretty = 0)
1 - mean((OJ[train, "Purchase"] == predict(reg_tree, type = "class")))
## (c)
reg_tree
## (d)
plot(reg_tree); text(reg_tree)
## (e)
y_pred <- predict(reg_tree, newdata = OJ[test,], type="class")
table(OJ[test, "Purchase"], y_pred)
(127+85)/(127+38+20+85) ## 0.785 => Test Error = 0.215
## (f)
cv.pred.tree <- cv.tree(reg_tree)
## (g)
plot(cv.pred.tree$size, cv.pred.tree$dev, type = 'b')
## (h)
## A tree of size 5 is best
## (i)
pruned.tree <- prune.tree(reg_tree, best = 6, method = "misclass")
## (j)
summary(pruned.tree) ## Misclass error = 0.1688...similar to above
## (k)
y_pred <- predict(pruned.tree, newdata = OJ[test,], type="class")
table(OJ[test, "Purchase"], y_pred)
(127+85)/(127+38+20+85) ## 0.785 => Test Error = 0.215 .. the same
## 10
## (a)
attach(Hitters)
dim(Hitters)
table(Hitters$Salary)
summary(Hitters$Salary)
hitters <- Hitters[!is.na(Hitters$Salary),]
dim(hitters)
summary(hitters$Salary)
## (b)
train <- 1:200
test <- -train
## (c) -(d)
library(gbm)
wei <- seq (1, 100, 10)/1000
#counter <-1
storage <-c()
for (i in wei){
  boosted.tree <- gbm (Salary ~., data = hitters[train,], 
                       n.trees = 1000, shrinkage = i)
  train.err <- mean((hitters[train, "Salary"] - 
                       predict(boosted.tree, n.trees = 1000))^2)
  test.err <- mean((hitters[test, "Salary"] - 
                      predict(boosted.tree, newdata = hitters[test,],
                                n.trees = 1000))^2)
  storage <- rbind(storage, c(train.err, test.err))
}
matplot(wei, storage, type='b')
## Looks like lambda = 0.1 gives the lowest train error
## But lambda = 0.01 gives lowest test error
## (e) Skipped
## (f) It appears Walks is most important followed by CHmRun
summary(boosted.tree)
## (g)
library(randomForest)
set.seed(123)
bag.tree <- randomForest (Salary ~., data=hitters, subset = train,
                          mtry = 19)
y_hat <- predict (bag.tree, newdata=hitters[test,])
mean((hitters[test, "Salary"] - y_hat)^2) ## test MSE = 54421.05
## 11
attach (Caravan)
dim(Caravan)
names(Caravan)
## (a)
train <- 1:1000
test <- -train
## (b)
library (gbm)
Cara <- Caravan
Cara$pur<- ifelse (Cara$Purchase == "Yes", 1, 0)
boosted.model <- gbm (pur~.-Purchase, data = Cara[train,], n.trees = 1000,
                      shrinkage = 0.01)
summary(boosted.model) ## PPERSAUT is most important, then MKOOPKLA
## (c)
y_hat <- predict (boosted.model, newdata = Cara[test,], n.trees = 1000,
                  type="response")
y_hat <- ifelse (y_hat>0.2, 1, 0)
table (Cara$pur[test], y_hat) ## 160 ppl predicted to make purchase
                              ## 37/160 = 23% actually did ... =( awful
## What if we apply logistic regression
log.fit <- glm (pur~.-Purchase, data= Cara[train, ], family = "binomial")
y_hat <- predict (log.fit, newdata = Cara[test,], type="response")
y_hat <- ifelse (y_hat >0.2, 1, 0)
table (Cara$pur[test], y_hat)
## 408 predicted, 58 get it so 14% accuracy and predicting who does buy
## What if we apply KNN
library(class)
y_hat <- knn(Cara[train, -(which(names(Cara) %in% c("Purchase", "pur")))], 
             Cara[test, -(which(names(Cara) %in% c("Purchase", "pur")))],
             cl = Cara$pur[train], k=10) 
head(y_hat)
table(Cara$pur[test], y_hat) ## 5 predicted, 1/5 = 20% do...
##12 Skipped
