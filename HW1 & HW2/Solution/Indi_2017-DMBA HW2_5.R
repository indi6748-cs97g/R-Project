#4 皮馬族印第安人糖尿病資料集
source("https://bioconductor.org/biocLite.R")

data(PimaIndiansDiabetes)

how-to-remove-outliers-from-a-dataset
http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset

http://rstudio-pubs-static.s3.amazonaws.com/160016_bb1d796f2c6f48698edae9a3bad7e72f.html

Data Cleaning - How to remove outliers & duplicates
http://qsel.columbia.edu/formhub.R/demo/RemoveOutliers.html



Outlier detection and treatment with R
https://datascienceplus.com/outlier-detection-and-treatment-with-r/

#4 皮馬族印第安人糖尿病資料集
# x6    1. Number of times pregnant
# x148  2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# x72   3. Diastolic blood pressure (mm Hg)
# x35   4. Triceps skin fold thickness (mm)
# x0    5. 2-Hour serum insulin (mu U/ml)
# x33.6 6. Body mass index (weight in kg/(height in m)^2)
# x.627 7. Diabetes pedigree function
# x50   8. Age (years)
# x1    9. Class variable (0 or 1)


rm(list = setdiff(ls(), lsf.str()))

install.packages("Matrix")
install.packages("arules")
install.packages("arulesViz")

library("Matrix")
# Load the libraries
library(arules)
library(arulesViz)

normailization <- function(x) (x - min(x))/(max(x)-min(x))
diabetes <- read.csv("D:\\Indi_Desktop\\Data Mining & Business Analytics\\資料集\\pima-indians-diabetes.csv")
diab_normal <- apply(diabetes[,1:9], 2, normailization)

#二元化
set_diab_normal <- diab_normal
for (x in c(1:8)) 
{
    diablet_median <- median(diab_normal[,x]) 
    set_diab_normal[which( diab_normal[,x] >= diablet_median),x] = 1
    set_diab_normal[which( diab_normal[,x] <  diablet_median),x] = 0
}       

#調整graphic的顯示格式
graphics.off()
par("mar") 
par(mar=c(1,1,1,1))

rules <- apriori(data=set_diab_normal, parameter = list(supp = 0.006))
inspect(rules[1:10])