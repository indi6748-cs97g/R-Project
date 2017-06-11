rm(list = setdiff(ls(), lsf.str()))

install.packages("MASS")
library(MASS)
install.packages("cluster")
library(cluster)
install.packages("clusterSim")
library(clusterSim)

install.packages("e1071") #for cmeans
library(e1071)

setwd("D:/Indi_Desktop/Data Mining & Business Analytics/資料集")


#s <- read.table("gender_size.csv", header=F, sep=',')
s <- read.csv("gender_size.csv")
summary(s)

maxmin <- function(x) (x - min(x))/(max(x)-min(x))
s <- apply(s[,1:3], 2, maxmin)

kms1 <- kmeans(s[,1:3], center=1)
kms2 <- kmeans(s[,1:3], center=2)
kms3 <- kmeans(s[,1:3], center=3)
kms4 <- kmeans(s[,1:3], center=4)
kms5 <- kmeans(s[,1:3], center=5)

kms1$centers
kms2$centers
kms3$centers
kms4$centers
kms5$centers


kms2$size
kms3$size
kms4$size
kms5$size

distS = dist(s[,1:3])
dbs1  = index.DB(s[,1:3],kms1$cluster, centrotypes="centroids")
dbs2  = index.DB(s[,1:3],kms2$cluster, centrotypes="centroids")
dbs3  = index.DB(s[,1:3],kms3$cluster, centrotypes="centroids")
dbs4  = index.DB(s[,1:3],kms4$cluster, centrotypes="centroids")
dbs5  = index.DB(s[,1:3],kms5$cluster, centrotypes="centroids")

#dbs1$DB
dbs2$DB
dbs3$DB
dbs4$DB
dbs5$DB
x <- 2:5
y <- c(dbs2$DB, dbs3$DB, dbs4$DB, dbs5$DB)
plot(x, y, main="Davies-Bouldin index for the K-means",  ylab='Davies-Bouldin index', xlab='Number of clusters')


# > dbs2$DB
# [1] 0.7414059
# > dbs3$DB
# [1] 0.7059271
# > dbs4$DB
# [1] 0.8633704
# > dbs5$DB
# [1] 1.023817

#c-means
#cl<- cmeans(s[,1:3],2,20,verbose=TRUE,method="cmeans",m=2)

cms2 <- cmeans(s[,1:3],2)
cms3 <- cmeans(s[,1:3],3)
cms4 <- cmeans(s[,1:3],4)
cms5 <- cmeans(s[,1:3],5)
cms2$centers
cms3$centers
cms4$centers
cms5$centers

distS = dist(s[,1:3])
dbs2  = index.DB(s[,1:3],cms2$cluster, centrotypes="centroids")
dbs3  = index.DB(s[,1:3],cms3$cluster, centrotypes="centroids")
dbs4  = index.DB(s[,1:3],cms4$cluster, centrotypes="centroids")
dbs5  = index.DB(s[,1:3],cms5$cluster, centrotypes="centroids")
x <- 2:5
y <- c(dbs2$DB, dbs3$DB, dbs4$DB, dbs5$DB)
plot(x, y, main="Davies-Bouldin index for the c-means",  ylab='Davies-Bouldin index', xlab='Number of clusters')


install.packages("flust")
library(flust)
fkM_clus2 = FKM(s[,1:3],k=2,m=1.5,stand=1)
fkM_clus3 = FKM(s[,1:3],k=3,m=1.5,stand=1)
fkM_clus4 = FKM(s[,1:3],k=4,m=1.5,stand=1)
fkM_clus5 = FKM(s[,1:3],k=5,m=1.5,stand=1)
xb2=XB(fkM_clus2$Xca,fkM_clus2$U,fkM_clus2$H,fkM_clus2$m)
xb3=XB(fkM_clus3$Xca,fkM_clus3$U,fkM_clus3$H,fkM_clus3$m)
xb4=XB(fkM_clus4$Xca,fkM_clus4$U,fkM_clus4$H,fkM_clus4$m)
xb5=XB(fkM_clus5$Xca,fkM_clus5$U,fkM_clus5$H,fkM_clus4$m)
x <- 2:5
y <- c(xb2, xb3, xb4, xb5)
plot(x, y, main="XB index for the k-means",  ylab='XB index', xlab='Number of clusters')