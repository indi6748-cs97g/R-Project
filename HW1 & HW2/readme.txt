#3 �s���ƶ�����ƨӷ�
install.packages("arules")
library(arules)
data("Groceries")

#4 �ְ��ڦL�Ħw�H�}���f��ƶ�
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
	�S�� DBI �� DB ���СA�O�@�ثD�ҽk�������s�A���ʫ���(Clustering Validity Index)�A�D�n�Q�δX���z�i��B��A���O�H�s�E�������{�׻P�s�E����K�{�ק@���Ŷq�̾�(�^����A2005)�A�Y���P���s��������I�Z���V�j�V�n�A�N���s�������ۦ��׻P���p�ʧC�B�t���ʰ��A�ӦP�s��������I�Z���V�p�V�n�A�N��Ӹs��������I��ۦ��׻P���p�ʰ��A�]���i���T���X�̾A�X��ƥ����S�ʪ����s���G�C�� DB ���ЭȶV�p�A�h�N��U���s��Ƥ����{�פp�B���s���Z���j�A�N�����̨Τ��E���ƥءC�]���A�b�Y�@�T�w�E���ƥر��ΤU�A�o��̤p�� DB���СA�h�N���E���ƥؤU�Ӷ��s���G����ƪ��̨λE���ƥ�

	��ƨӷ��G���������g�����޳N���Q��ޱ����Ҧ��U�@�z�H���Ʀ�ǲߤ��ϥηN�@  p.37-38 


XB����
	The optimal number of cluster k is achieved when the index value is minimized.
	��ƨӷ��Ghttps://r-how.com/packages/fclust/XB

��XB���е��� c-means
	http://www.lac.inpe.br/~rafael.santos/R/Snippets-FCM/index.html
	
BIC 
	for EM initialized by model-based hierarchical clustering for parameterized Gaussian mixture models.
	http://svitsrv25.epfl.ch/R-doc/library/mclust/html/mclustBIC.html

	�d�ҡG	http://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html
	BIC = mclustBIC(X)
	summary(BIC)
	## Best BIC values:
	##              VVV,3       VVE,3       EVE,4
	## BIC      -4760.091 -4775.53693 -4793.26143
	## BIC diff     0.000   -15.44628   -33.17079