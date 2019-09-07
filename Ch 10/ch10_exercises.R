## Ch 10 Exercises
## Applied
## 9
## (a)
library(ISLR)
attach(USArrests)
hier <- hclust(dist (USArrests), method="complete")
## (b) h = 125
cuts<-cutree(hier, h=125)
cuts ## Shows which states belong to which clusters
## (c)
us <- scale(USArrests)
## (d)
hier1 <- hclust (dist(us), method = "complete")
par(mfrow = c(1,2))
plot(hier)
plot(hier1)
## Standardization leads to 4ish groups whereas not standardizing gives 3ish
## Values should be standardized so that units of measures dont influence clustering
######
## 10
#######
## (a)
input <- matrix (rnorm (60*50), ncol=50)
input[21:40,] <- input[21:40,]+2
input[41:60,] <- input[41:60,] + 5
y <- rep (1,60)
y [21:40] <- 2
y[41:60] <-3
## (b)
par(mfrow=c(1,1))
pcr <- prcomp (input, scale=T)
biplot(pcr, choices = c(1,2), arrow.len=0)
plot (pcr$x[,1], pcr$x[,2], col = rep (c(1,2,3), c(20,20,20)))
## (c)
km <- kmeans(input, centers = 3)
names(km)
table(km$cluster, y)
## Cluster perfectly
## (d)
km <- kmeans(input, centers = 2)
names(km)
table(km$cluster, y) ## 2 of the clusters are incorrectly merged
## (e)
km <- kmeans(input, centers = 4)
names(km)
table(km$cluster, y) ## 1 cluster is incorrectly split into 2
## (f)
km <- kmeans(cbind(pcr$x[,1], pcr$x[,2]), centers = 3)
table(km$cluster, y) ## still divides it well
## (g)
km <- kmeans(scale(input), centers = 3)
km$cluster
table(km$cluster, y)
