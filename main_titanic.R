# Load in the R package  
#install.packages('rpart') for DT
install.packages("rpart")
require(rpart)
require(rattle)
library(class)
library(caret)



setwd("C:/MIMIC/Desktop/資料集")
#s <- read.table("gender_size.csv", header=F, sep=',')
titanic <- read.csv("titanic_small.csv", header = TRUE, sep = ",")
summary(titanic)

#Use ggplot() to plot the distribution of sexes within the classes of the ship.
require(ggplot2)
### Loading required package: ggplot2

#性別&存活關係圖
ggplot(titanic,aes(x=factor(titanic$survival),fill=factor(titanic$gender)))+
  geom_bar(position="dodge")

#艙等與存活關係圖
ggplot(titanic,aes(x=factor(titanic$survival),fill=factor(titanic$class)))+
  geom_bar(position="dodge")

#年紀與存活關係圖
ggplot(titanic,aes(x=factor(titanic$survival),fill=factor(titanic$age)))+
  geom_bar(position="dodge")


#X <- cbind(titanic$gender, titanic$age, titanic$class)
#y <- c(titanic$survival)
#set.seed(1)
#idx <- createFolds(y, k=3)
 
# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(titanic), size=ceiling(0.8*nrow(titanic) ))
train <- titanic[train.index, ]
test <- titanic[-train.index, ]

# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(train$survival ~. , 
                   data=train)

require(rpart.plot) 
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=1)

##解釋看http://www.rpubs.com/skydome20/R-Note6-Apriori-DecisionTree
## 最下面節點的數字，代表...

#進行預測
pred <- predict(cart.model, newdata=test)

# 用table看預測的情況
table(real=test$survival, predict=pred)

# 計算預測準確率 = 對角線的數量/總數量
confus.matrix <- table(real=test$survival, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量