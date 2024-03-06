#Name:  Rukshan Fernando                                                .
#UoWNO: 18098217                                                        .
#StID:  2019784  

#Creating a Work Directory#
setwd("c:/Users/Rukshan Fernando/OneDrive/Desktop/Level_5/2ndSem/5DATA001C.2 Machine Learning and Data Mining/Assesment")

#libraries has to be import# 
library(readxl) 
library(factoextra) 
library(NbClust)
library(gmodels) 

#Excel data should be read#
whitewineX <- read_xlsx("w1809821_p1_Whitewine_v2.xlsx") 

#Removing rows with missing or incomplete values#
sum(is.na(whitewineX)) 
whitewineX_V1 <- na.omit(whitewineX) 

#Looking for outliers# 
boxplot(whitewineX_V1$`fixed acidity`, plot=FALSE)$out 

#Detected outliers are saved to a variable# 
outliers <- boxplot(whitewineX_V1$`fixed acidity`, plot=FALSE)$out 

#Removing outliers from the new data and allocating it to a new data frame#
whitewineX_V2 <- whitewineX_V1[-which(whitewineX_V1$`fixed acidity` %in% outliers),] 



#repeat the procedure for each column#
boxplot(whitewineX_V2$`volatile acidity`, plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$`volatile acidity`, plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$`volatile acidity` %in% outliers),] 

boxplot(whitewineX_V2$`citric acid`, plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$`citric acid`, plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$`citric acid` %in% outliers),] 

boxplot(whitewineX_V2$`residual sugar`, plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$`residual sugar`, plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$`residual sugar` %in% outliers),] 

boxplot(whitewineX_V2$chlorides , plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$chlorides , plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$chlorides  %in% outliers),] 

boxplot(whitewineX_V2$`free sulfur dioxide` , plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$`free sulfur dioxide` , plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$`free sulfur dioxide`  %in% outliers),] 

boxplot(whitewineX_V2$`total sulfur dioxide` , plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$`total sulfur dioxide` , plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$`total sulfur dioxide`  %in% outliers),] 

boxplot(whitewineX_V2$pH , plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$pH , plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$pH  %in% outliers),] 

boxplot(whitewineX_V2$sulphates , plot=FALSE)$out 
outliers <- boxplot(whitewineX_V2$sulphates , plot=FALSE)$out 
whitewineX_V2 <- whitewineX_V2[-which(whitewineX_V2$sulphates  %in% outliers),] 

#Outliers have been removed#


#Scaling the data# 
#Standardization of Z-scores# 
whitewineX_V2ZScor <- scale(whitewineX_V2) 
whitewineX_V2ZScor_Input <- subset(whitewineX_V2ZScor, select = -quality) 
whitewineX_V2ZScor_Output <- subset(whitewineX_V2, select = quality) 
boxplot(whitewineX_V2ZScor_Input) 
whitewineX_V3 <- cbind(whitewineX_V2ZScor_Input, whitewineX_V2ZScor_Output) 

#Calculating the number of clusters# 
#part of Elbow_Plot# 
library(factoextra) 
fviz_nbclust(whitewineX_V2ZScor_Input, kmeans, method="wss") +   
  labs(subtitle = "Elbow Method") 

#Method of Silhouette# 
fviz_nbclust(whitewineX_V2ZScor_Input, kmeans, method="silhouette") +   
  labs(subtitle = "Silhouette Method") 

#Part of NBclust# 
NbClust(whitewineX_V2ZScor_Input, distance = "euclidean", min.nc = 2, max.nc = 10,         
        method = "kmeans", index = "all",) 

#Part of the Kmeans Clustering# 

kms_Out <- kmeans(whitewineX_V2ZScor_Input, 4) 
print(kms_Out$betweenss) 
print(kms_Out$totss) 

#Make the clusters visible#
kms_Cluster <- kms_Out$cluster 
rownames(whitewineX_V2ZScor) <- paste(whitewineX_V2$quality, 1:dim(whitewineX_V2)[1], sep = "_") 
fviz_cluster(list(data = whitewineX_V2ZScor_Input, cluster = kms_Cluster)) 

#Part of the Confusion Matrix# 
valueExpected <- factor(whitewineX_V2$quality) 
valuePredicted <- factor(kms_Out$cluster) 

CrossTable(valueExpected, valuePredicted) 


#Part of the PCA Method# 
WineData_v2Z_PCA = princomp(whitewineX_V2ZScor) 
summary(WineData_v2Z_PCA) 
plot(WineData_v2Z_PCA) 
screeplot(WineData_v2Z_PCA, type = "lines", main = "PCA") 
biplot(WineData_v2Z_PCA) 


WineData_v2Z_PCA_NEW <- cbind(whitewineX_V3, WineData_v2Z_PCA$scores) 

#KMEANS using a new Dataset#
kms_new_pca <-kmeans(WineData_v2Z_PCA_NEW[13:23], 4) 
fviz_cluster(kms_new_pca, WineData_v2Z_PCA_NEW[13:23], cluster = kms_new_pca$cluster) 

kms_new_pca$betweenss 
kms_new_pca$tot.withinss 
kms_new_pca$totss 

