library(ISLR)
names(Weekly)
head(Weekly)
dim(Weekly)
pairs(Weekly[,-9], col = Weekly$Direction)
boxplot(Lag1 ~ Direction, data = Weekly)
par(mfrow=c(1,1))

## 10)
log.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +
                 Volume, data = Weekly, family = "binomial")
summary(log.fit)
## Lag2 appears significant
predictions <- predict (log.fit, type="response")
# 10 c) table(ifelse(predictions>0.5, "Up", "Down"), Weekly$Direction)
table(Weekly$Direction)
#    Down  Up
#Down   54  48
#Up    430 557

# 10 d) 
ind <- Weekly$Year <=2008
log.fit2 <- glm(Direction ~ Lag2, family = "binomial", data = Weekly,
                subset = ind)
pred2 <- predict(log.fit2, type = "response", newdata = Weekly[Weekly$Year>2008, ])
table(ifelse(pred2 >0.5, "Up", "Down"), Weekly$Direction[Weekly$Year>2008])
#      Down Up
#Down    9  5
#Up     34 56
# The correct predictins is 65/104

# e) 
lda.fit2 <- lda(Direction ~ Lag2, data = Weekly, subset = ind)
pred3 <- predict(lda.fit2, , newdata = Weekly[Weekly$Year>2008, ])
table(pred3$class, Weekly$Direction[Weekly$Year>2008])
#     Down Up
#Down    9  5
#Up     34 56
# 56+9 / 104 = 65/104 = 0.625

# f)
library(MASS)
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = ind)
pred4 <- predict(qda.fit, , newdata = Weekly[Weekly$Year>2008, ])
table(pred4$class, Weekly$Direction[Weekly$Year>2008])
#     Down Up
#Down    0  0
#Up     43 61
# 61/104 = 0.587

# g)
knn.fit <- knn(train = data.frame(Weekly$Lag2[Weekly$Year<=2008]), 
               test = data.frame(Weekly$Lag2[Weekly$Year>2008]),
               cl = Weekly$Direction[ind], k=1)
knn.fit
table(knn.fit, Weekly$Direction[Weekly$Year>2008])
# knn.fit Down Up
#Down     21 29
#Up       22 32
# 52/104 = 0.5

######
#########
######
#11
#a)
mpg01 <- ifelse (Auto$mpg > median(Auto$mpg), 1, 0)
auto <-data.frame(Auto, mpg01)
auto$mpgcolor <- ifelse (auto$mpg01 == 0, "red", "blue")
#b) Variables associated with mpg01 include:
#             mpg (obviously), displacement, horsepower, weight, 
pairs(auto[,1:8],
      col = auto$mpgcolor)
#c) 
ind_train <- auto$year<79
# d) 
lda.fit <- lda (mpg01 ~ displacement + horsepower + weight, data = auto,
                subset = ind_train)
predictions <- predict (lda.fit, auto[-ind_train,c("displacement","horsepower","weight")],
                        type = "response")
table(predictions$class, auto[-ind_train, "mpg01"])
# The test error is (16+25)/391 = 0.105
# e)
qda.fit <- qda (mpg01 ~ displacement + horsepower + weight, data = auto,
                subset = ind_train)
predictions <- predict (qda.fit, auto[-ind_train,c("displacement","horsepower","weight")],
                        type = "response")
table(predictions$class, auto[-ind_train, "mpg01"])
# The test error is (23+17)/391 = 0.102
# f)
logistic.fit <- glm (mpg01 ~ displacement + horsepower + weight, data = auto,
                subset = ind_train, family = "binomial")
predictions <- predict (logistic.fit, auto[-ind_train,c("displacement","horsepower","weight")],
                        type = "response")
table(ifelse (predictions > 0.5, 1, 0), auto[-ind_train, "mpg01"])
# The test error is (39+14)/391 = 0.136
# g)
graph_table <-matrix(nrow=20,ncol=2)
for (i in 1:20) {
  knn.fit <- knn (auto[ind_train,c("displacement","horsepower","weight")],
                  auto[-ind_train,c("displacement","horsepower","weight")],
                  cl = auto[ind_train,c("mpg01")],
                  k = i)
  err <- (table(knn.fit, auto[-ind_train,c("mpg01")])[1,2] + 
    table(knn.fit, auto[-ind_train,c("mpg01")])[2,1])/length(knn.fit)
  graph_table [i,1] <-err
  graph_table [i,2] <- i
  print(err)
}
plot(graph_table[,2], 1-graph_table[,1], type = "b")
# K =1 seems to perform best. We get a test error of about 6%
###########
## 12
#############
# a) 
Power <- function(){
  print(2^3)
}
Power()
# b)
Power2 <- function (x, a){
  print(x^a)
}
Power2(3,8)
# c)
Power2(10,3) # 1000
Power2(8,17) # 2.25 *10^15
Power2(131,3)# 2248091
# d)
Power3 <- function (x, a){
  results <- (x^a)
  return (results)
}
# e)
power3_table <- matrix(nrow = 10, ncol =2)
for (i in 1:10)
{
  power3_table[i,1]<- i
  power3_table[i,2]<-Power3(i,2)
}
plot(power3_table[,1], power3_table[,2], xlab = "X", ylab = "X^2",
     type="b")
# f)
PlotPower <- function (x, a){
  plotpower_table <- matrix(nrow = length(x), ncol =2)
  for (i in x)
  {
    # indexing so that if x doesnt start at 1 plot doesnt 
    # crash bc bunch of indices are not initialized which is
    # the case if index is just "i"
    plotpower_table[i-min(x)+1,1]<- i 
    plotpower_table[i-min(x)+1,2]<-Power3(i,a)
  }
  plot(plotpower_table[,1], plotpower_table[,2], xlab = "X",
       type="b")
}
PlotPower(5:25,3)
