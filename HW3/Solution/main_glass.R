install.packages("class")
install.packages("caret")
library(class)
library(caret)

setwd("C:/MIMIC/Desktop/è³‡æ?™é??")
#s <- read.table("gender_size.csv", header=F, sep=',')
s <- read.csv("glass.csv")
summary(s)

#initialization
X <- s[,1:10]
y <- s[,11]

#use the createFolds function from the caret package to make 3 folds
set.seed(1)
idx <- createFolds(y, k=3)
mean_acc = array(0, c(9,1))
acc = array(0, c(3, 1))

for (z in c(1,3,5,7,9))
{
  for (x in c(1:3)) 
  {
    pred <- knn(train=X[ -idx[[x]] , ], test=X[ idx[[x]], ], cl=y[ -idx[[x]] ], k=z)
    acc[x,1] <- mean(y[ idx[[x]] ] == pred)
  }  
  mean_acc[z,1] <- mean(acc)
}


