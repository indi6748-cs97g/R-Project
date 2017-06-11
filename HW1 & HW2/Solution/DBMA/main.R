#http://blog.csdn.net/sanqima/article/details/42746419
#支持度,置信度, 提升度教學


# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)

graphics.off()
par("mar") 
par(mar=c(1,1,1,1))

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

rules<-sort(rules, by="confidence", decreasing=TRUE)
summary(rules)
#summary of quality measures:
#    support          confidence        lift     
# Min.   :0.00102   Min.   :0.80   Min.   : 3.1  
# 1st Qu.:0.00102   1st Qu.:0.83   1st Qu.: 3.3  
# Median :0.00122   Median :0.85   Median : 3.6  
# Mean   :0.00125   Mean   :0.87   Mean   : 4.0  
# 3rd Qu.:0.00132   3rd Qu.:0.91   3rd Qu.: 4.3  
# Max.   :0.00315   Max.   :1.00   Max.   :11.2  

#mining info:
#      data ntransactions support confidence
#Groceries          9835   0.001        0.8

#支持度超過.006的前十名規則
rules <- apriori(Groceries, parameter = list(supp = 0.006))
inspect(rules[1:10])
#不存在!
#Error in slot(x, s)[i] : subscript out of bounds 

#支持度超過.006 & conf = 0.6的前十名規則
rules <- apriori(Groceries, parameter = list(supp = 0.006, conf=0.6))
inspect(rules[1:10])
#不存在!
#Error in slot(x, s)[i] : subscript out of bounds 

#b.	承a子題，依據提升度定義，是否所有規則的商品組合均符合正向相關？
#上提supp = .006沒有答案, 若改為.0006則答案如下
#rulues & 提升度均不同
rules <- apriori(Groceries, parameter = list(supp = 0.0006))
inspect(rules[1:10])


rules <- apriori(Groceries, parameter = list(supp = 0.0006 , conf=0.6))
inspect(rules[1:10])




#c.	哪些商品適合和全脂牛奶一起兜售？
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#[1] {whole milk} => {other vegetables} 0.07483477 0.2928770  1.513634
#[2] {whole milk} => {rolls/buns}       0.05663447 0.2216474  1.205032
#[3] {whole milk} => {yogurt}           0.05602440 0.2192598  1.571735
#[4] {whole milk} => {root vegetables}  0.04890696 0.1914047  1.756031
#[5] {whole milk} => {tropical fruit}   0.04229792 0.1655392  1.577595
#[6] {whole milk} => {soda}             0.04006101 0.1567847  0.8991124




