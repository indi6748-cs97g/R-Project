# 學號：0563414
# 姓名：范國晏
# 作業 04

#====================================================
mse <- function(error) #mean-squared error
{
  sum(error^2)/length(ts.id)
}
mad <- function(error) #mean absolute deviation
{
  sum(abs(error))/length(ts.id)
}
mape <- function(errpe) #mean absolute percentage error
{
  sum(abs(errpe))/length(ts.id)
}
#====================================================
install.packages("earth")
install.packages("caret")
install.packages("class")
install.packages("e1071")
install.packages("neuralnet")
install.packages("FNN")

setwd("~/Desktop/R資料夾")
setwd("D:/Indi_Desktop/個人研究與實驗/Data Mining & Business Analytics/HW4")
setwd("D:/Indi_Desktop/個人研究與實驗/Data Mining & Business Analytics/HW4/data")

#=============== 第一題 =============================
ww <- read.csv("winequality-white.csv")

#remark start
#inputData <- read.csv("winequality-white.csv")

## ----- step 1 選出重要變數 -----
##以線性迴歸(LM)選出重要變數
#ww.lm<-lm(quality~., data = tr.ww)
ww.lm<-lm(quality~., data = ww)
anova(ww.lm)
summary(step(ww.lm), k=2, method="both")

#以非線性迴歸(MARS)選出重要變數
library(earth)

ww.MARS<-earth( quality~., degree = 2, trace=1, ww)
summary(ww.MARS)

#選擇欄位:
#volatile.acidity、residual.sugar、free.sulfur.dioxide、pH、alcohol

predictors <- c(2, 4, 6, 9, 11, 12)

# ----- step 2 進行預測 -----
ww2 = ww[, predictors]
id=sample(1:nrow(ww2), round(nrow(ww2)/3))
tr.ww=ww2[-id,]
ts.ww=ww2[id,]

#KNN
library(class)
library(caret)
X <- tr.ww[, -5]
y <- tr.ww$quality
set.seed(1)
idx <- createFolds(tr.ww$quality, k=5)
mean_acc = array(0, c(1,1))
acc = array(0, c(5, 1))

for (z in c(1))
{
  for (x in c(1:5)) 
  {
    pred <- knn(train=X[ -idx[[x]] , ], test=X[ idx[[x]], ], cl=y[ -idx[[x]] ])
    acc[x,1] <- mean(y[ idx[[x]] ] == pred)
  }  
  mean_acc[z,1] <- mean(acc)
}

mean_acc[z,1]

#library(FNN)
#ww.pre<- knn.reg(tr.ww[,-6], ts.ww[,-6], tr.ww[,6], k = 3, algorithm= "brute")
#ww.result<- data.frame(realY=ts.ww$quality, predictY=ww.pre$pred)
#ww.er= ww.result$realY - ww.result$predictY
#ww.pe= ww.er/ ww.result$realY
#mse(ww.er)
#mad(ww.er)
#mape(ww.pe)


#SvR
library(e1071)
mean_acc = array(0, c(1,1))
acc = array(0, c(5, 1))
for (z in c(1))
{
  for (x in c(1:5)) 
  {
    
    model <- svm(X[ -idx[[x]] , ] , y[ -idx[[x]] ])
    pred <- predict(model, X[ idx[[x]], ])
    acc[x,1] <- mean(y[ idx[[x]] ] == round(pred))
  }  
  mean_acc[z,1] <- mean(acc)
}

mean_acc[z,1]

#(BPN) ## 
## https://rpubs.com/skydome20/R-Note8-ANN
library(neuralnet)
mean_acc = array(0, c(1,1))
acc = array(0, c(5, 1))
for (z in c(1))
{
  for (x in c(1:5)) 
  {
    
    backnet = neuralnet(y[-idx[[x]]]~X$alcohol[-idx[[x]]]+X$free.sulfur.dioxide[-idx[[x]]]+
                          X$volatile.acidity[-idx[[x]]]+X$residual.sugar[-idx[[x]]]+
                          X$fixed.acidity[-idx[[x]]], 
                        inputData[-idx[[x]],], hidden=10, learningrate=0.01)
    
    
    
    ###
    pred <- compute(backnet, X$alcohol[idx[[x]]]+X$free.sulfur.dioxide[idx[[x]]]+
                      X$volatile.acidity[idx[[x]]]+X$residual.sugar[idx[[x]]]+
                      X$fixed.acidity[idx[[x]]])
    
    acc[x,1] <- mean(y[ idx[[x]] ] == round(pred))
  }  
  mean_acc[z,1] <- mean(acc)
}

mean_acc[z,1]

#=============== 第二題 =============================
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("adabag")

# 先手動講將 y 只值改為 0/1
bank_y <- read.csv("bank_y.csv")
bank <- read.csv("bank.csv")

# ----- step 1 選出重要變數 -----
#以線性迴歸(LM)選出重要變數
#base.mod <- lm(bank_y$y ~ 1 , data= bank_y)
#all.mod <- lm(bank_y$y ~ . , data= bank_y)
#stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
#shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
#shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
#print(shortlistedVars)

bk.lm<-lm(y~., data = bank_y)
anova(bk.lm)
summary(step(bk.lm), k=2, method="both")


#以非線性迴歸(MARS)選出重要變數
library(earth)
#marsModel <- earth(bank_y$y ~ ., data=bank_y) # build model
#ev <- evimp (marsModel) # estimate variable importance
#win.graph()
#plot(ev)

bk.MARS<-earth( y~., degree = 2, trace=1, bank_y)
summary(bk.MARS)

# CART、boosting、bagging及隨機森林(random forest)
library(rpart)
library(rpart.plot)
library(adabag)
library(rpart)
library(rpart.plot)
library(randomForest)

# 切資料集
#predictors <- c(1)

#bankX <- bank[, predictors]
#banky <- bank$y

#ind<-sample(2, nrow(bank), replace=T, prob=c(0.6, 0.4))
#tr.bank<-bank[ind==1, ]
#ts.bank<-bank[ind==2, ]

id=sample(1:nrow(bank), round(nrow(bank)/3))
tr.bank=bank[-id,]
ts.bank=bank[id,]


# CART
red.bk=rpart(y~., tr.bank, method="class", minsplit=10, minbucket=10)
print(red.bk)
summary(red.bk)
win.graph()
rpart.plot(red.bk, fallen.leaves=TRUE)

catr.red=predict(red.bk, tr.bank, type="class") #training set
table(tr.bank$y, catr.red)
sum( as.numeric(catr.red == tr.bank[,12]) )/ nrow(tr.bank)


# boosting
boo.bk<-boosting(y~., tr.bank, mfinal=10)
boo.bk$importance
bt.bk<-predict(boo.bk, ts.bank)
names(bt.bk)
bt.bk$confusion
sum( as.numeric(bt.bk$class == ts.bank[, 17]) )/ nrow(ts.bank)

# bagging
bag.bk<-bagging(y~., tr.bank, mfinal=10)
names(bag.bk)
bag.bk$importance
bg.bk<-predict(bag.bk, ts.bank)
names(bg.bk)
bg.bk$confusion
sum( as.numeric(bg.bk$class == ts.bank[, 17]) )/ nrow(ts.bank)

# 隨機森林(random forest)  RF
#bank.rf<-randomForest(y~., data=tr.bank, ntree=50, importance=T, proximity=T, na.action=na.omit)
#importance(bank.rf)
#bank.rf$confusion
#MDSplot(bank.rf, tr.bank$y, pch=as.numeric(tr.bank$y))
#plot(bank.rf)
#print(bank.rf)
#bank.pred<-predict(bank.rf, newdata=ts.bank)
#table(ts.bank$y, bank.pred)

bank.rf<-randomForest(y~., data=tr.bank, ntree=100, mtry=6, importance=T, proximity=T)
importance(bank.rf)
plot(bank.rf)
print(bank.rf)
rf.bk<-predict(bank.rf, newdata= ts.bank)
table(ts.bank$y, rf.bk)
sum( as.numeric(rf.bk == ts.bank[, 17]) )/ nrow(ts.bank)
