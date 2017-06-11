#3 零售資料集的資料來源
install.packages("arules")
library(arules)
data("Groceries")

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


Davies Bouldin Index
	又稱 DBI 或 DB 指標，是一種非模糊型的分群適切性指標(Clustering Validity Index)，主要利用幾何原理進行運算，分別以群聚間離散程度與群聚內緊密程度作為衡量依據(彭雅芬，2005)，即不同集群間的資料點距離越大越好，代表兩群之間的相似度與關聯性低且差異性高，而同群內的資料點距離越小越好，代表該群內的資料點其相似度與關聯性高，因此可明確指出最適合資料本身特性的分群結果。當 DB 指標值越小，則代表各集群資料分散程度小且集群間距離大，意指為最佳之聚類數目。因此，在某一固定聚類數目情形下，得到最小的 DB指標，則代表此聚類數目下該集群結果為資料的最佳聚類數目

	資料來源：應用類神經網路技術探討科技接受模式下護理人員數位學習之使用意願  p.37-38 


XB指標
	The optimal number of cluster k is achieved when the index value is minimized.
	資料來源：https://r-how.com/packages/fclust/XB

用XB指標評估 c-means
	http://www.lac.inpe.br/~rafael.santos/R/Snippets-FCM/index.html
	
BIC 
	for EM initialized by model-based hierarchical clustering for parameterized Gaussian mixture models.
	http://svitsrv25.epfl.ch/R-doc/library/mclust/html/mclustBIC.html

	範例：	http://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html
	BIC = mclustBIC(X)
	summary(BIC)
	## Best BIC values:
	##              VVV,3       VVE,3       EVE,4
	## BIC      -4760.091 -4775.53693 -4793.26143
	## BIC diff     0.000   -15.44628   -33.17079