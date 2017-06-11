install.packages("MASS")
library(MASS)
install.packages("cluster")
library(cluster)
install.packages("clusterSim")
library(clusterSim)

setwd("D:/Indi_Desktop/Data Mining & Business Analytics/資料集")

gender_size<- read.csv("gender_size.csv")

# K means
result0= kmeans(gender_size[,1:3], center=5)
print(result0)
result0$centers

table(gender_size$Gender, result0$cluster)

plot(gender_size[,1:3], pch=(result0$cluster), col=result0$cluster)

points(result0$centers, col=1:3, pch=8)
#---------------------------------------------
min.nc=2
max.nc=8

KM=array(0, c(max.nc-min.nc+1, 2))
for (nc in min.nc : max.nc)
{ fitkm=kmeans(iris[, -5], center=nc)
KM[nc-(min.nc-1), 1]= fitkm$betweenss/fitkm$tot.withinss
KM[nc-(min.nc-1), 2]= index.DB(iris[,-5], fitkm$cluster, centrotypes="centroids", p=2)$DB
}
which(KM[,1]==max(KM[,1]))
which(KM[,2]==min(KM[,2]))
