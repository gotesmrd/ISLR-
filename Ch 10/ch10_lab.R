## 10.4 => Lab 1: Principal Component Analysis
states = row.names(USArrests)
states
names(USArrests)
apply (USArrests, 2, mean)
apply (USArrests, 2, var)
pr.out <- prcomp (USArrests, scale = T)
names (pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim (pr.out$x)
biplot (pr.out, scale=0)
pr.out$rotation <- -pr.out$rotation
pr.out$x = -pr.out$x
biplot (pr.out, scale=0)
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve
plot (pve, xlab = "Principal Component", ylab = "Proportion of variance
      Explained", ylim = c(0,1), type='b')
plot (cumsum(pve), xlab = "Principal Component", ylab = "
      Cumulative Proportion of Variance Explained", ylim = c(0,1),
      type="b")
a = c(1,2,8,-3)
cumsum(a)
## 10.5 ==> Lab 2: Clustering
## 10.5.1 K-Means Clustering
set.seed(2)
x = matrix (rnorm (50*2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4
km.out <- kmeans (x,2,nstart=20)
km.out$cluster
plot (x, col = (km.out$cluster+1), main = "K-means Clustering Results
With K=2", xlab = "", ylab = "", pch = 20, cex=2)
set.seed (4)
km.out = kmeans (x, 3, nstart = 20)
km.out
set.seed (3)
km.out = kmeans (x,3,nstart=1)
km.out$tot.withinss
km.out = kmeans (x,3,nstart=20)
km.out$tot.withinss
## 10.5.2 Hierarchical Clustering
hc.complete <- hclust (dist(x), method = "complete")
hc.average <- hclust (dist(x), method = "average")
hc.single <- hclust (dist(x), method = "single")
par(mfrow = c(1,3))
plot (hc.complete, main = "Complete Linkage", xlab = "", sub="", cex = .9)
plot (hc.average, main = "Average Linkage", xlab = "", sub="", cex = .9)
plot (hc.single, main = "Single Linkage", xlab = "", sub="", cex = .9)
cutree (hc.complete, 2)
cutree (hc.average, 2)
cutree (hc.single, 2)
cutree (hc.single, 4)
xsc = scale (x)
plot (hclust(dist(xsc), method = "complete"), main="Hierarchical
      Clustering with Scaled Features")
x = matrix (rnorm (30*3), ncol=3)
dd = as.dist(1-cor(t(x)))
plot (hclust(dd, method = "complete"), main = "Copmlete Linkage with
      Correlation - based distance", xlab = "", sub="")
## 10.6 Lab 3: NCI0 Data Example
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data
dim(nci.data)
nci.labs [1:4]
table(nci.labs)
pr.out = prcomp (nci.data, scale = T)
Cols = function (vec){
  cols = rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}
par(mfrow = c(1,2))
plot (pr.out$x[,1:2], col = Cols (nci.labs), pch = 19,
      xlab = "Z1", ylab = "Z2")
plot (pr.out$x[,c(1,3)], col = Cols (nci.labs), pch = 19,
      xlab = "Z1", ylab = "Z3")
summary(pr.out)
plot(pr.out)
pve = 100 * pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot (pve, type = "o", ylab = "PVE", xlab = "Principal Component",
      col="blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = 
       "Principal Component", col = "brown3")
## 10..2 Clustering the Observations of the NCI60 Data
sd.data = scale (nci.data)
par (mfrow = c(1,3))
data.dist = dist (sd.data)
plot (hclust(data.dist), labels = nci.labs, main = "Complete Linkage",
      xlab = "", sub="", ylab="")
plot (hclust (data.dist, method = "average"), labels = nci.labs,
      main = "Average Linkage", xlab="", sub="", ylab="")
plot (hclust(data.dist, method="single"), labels = nci.labs,
      main = "Single Linkage", xlab = "", sub="", ylab="")
hc.out <- hclust (dist(sd.data))
hc.clusters <- cutree (hc.out, 4)
table (hc.clusters, nci.labs)
par(mfrow = c(1,1))
plot (hc.out, labels = nci.labs)
abline (h=139, col="red")
hc.out
set.seed(2)
km.out = kmeans (sd.data, 4, nstart=20)
km.clusters = km.out$cluster
table (km.clusters, hc.clusters)
hc.out = hclust (dist(pr.out$x[,1:5]))
plot (hc.out, labels = nci.labs, main = "Hier. Clust. on First
      Five Score Vectores")
table (cutree (hc.out, 4), nci.labs)
## Unit 10.R Review Questions
#install.packages("miceadds")
library (miceadds)
## 10.R.1
#load.Rdata (filename ="C:/Users/Owner/Desktop/Programming_Education/Stanford\ -\ ISLR/Ch\ 10/10.R.Rdata", "d")
d<- get(load ("C:/Users/Owner/Desktop/Programming_Education/Stanford\ -\ ISLR/Ch\ 10/10.R.Rdata", verbose=T))
Xs <- rbind (x, x.test)
pr <- prcomp(x, scale = T)
# 10.R.2
#install.packages("pls")
models <- model.matrix (y ~ pr$x[,1] + pr$x[,2]+pr$x[,3]+
                          pr$x[,4]+pr$x[,5])
lm.fit <- lm (y ~ pr$x[,1] + pr$x[,2]+pr$x[,3]+
                pr$x[,4]+pr$x[,5])
pr <- prcomp(x.test, scale = T)
newdata <- data.frame( cbind((pr1$x[,1]) , pr1$x[,2],pr1$x[,3],
                             pr1$x[,4],pr1$x[,5]))
y_pred <- predict (lm.fit, data.frame( cbind(pr$x[,1] + pr$x[,2]+pr$x[,3]+
                                               pr$x[,4]+pr$x[,5])))
mean ((y.test-y_pred)^2)
df <- cbind (x, y)
y_train <- pcr (y~., 5, data=df)
#ddd <- data.frame(cbind (x.test, y.test))
y_pred <- predict (y_train, newdata =  x.test)
mean ((y.test-y_pred)^2)

#library(pls)
#train <- cbind (x, y)
#pcr.fit <- pcr (y~., 5, train)
df1 <- cbind (x, y)
lm.fit <- lm (y~., data=df1)

preds <- predict (lm.fit, newdata = x.test)
mean ((preds - y.test)^2)
