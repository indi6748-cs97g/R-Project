https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Expectation_Maximization_(EM)


rm(list = setdiff(ls(), lsf.str()))

install.packages("mclust")
library(mclust)

setwd("D:/Indi_Desktop/Data Mining & Business Analytics/資料集")
rm(list = setdiff(ls(), lsf.str()))
#s <- read.table("gender_size.csv", header=F, sep=',')
s <- read.csv("gender_size.csv")
normal_s <- s
normal_s[,1] <- s[,1]-min(s[,1]) / max(s[,1])-min(s[,1])
normal_s[,2] <- s[,2]-min(s[,2]) / max(s[,2])-min(s[,2])
normal_s[,3] <- s[,3]-min(s[,3]) / max(s[,3])-min(s[,3])
s <- normal_s



class = s$Gender
table(class)

X = s[,-4]
head(X)
clPairs(X, class)

BIC = mclustBIC(X)

summary(BIC)
plot(BIC)


