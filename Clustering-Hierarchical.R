###### Hierarchical Clustering Case Study - Two Wheeler Servicing ###### 


#### Step-1 - Data collection ####
data1=read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Clustering Case Study - R Program/Two-wheeler+Case+Study_Hierarchical/Two-wheeler Case Study_Hiearchical/data1.csv")


#### Step-2 - Data Wrangling ####
### Filtering data: considering only those customers for the analysis for whom the 3rd visit happened

data2=subset(data1,data1$NUM_PAID_VIS==2&is.na(data1$VISIT3)==FALSE)

### Filtering data: only features Average usage during paid and free servicing period are chosen
data3=data2[,c(14,15)]
head(data3)



#### Step-3 - Normalizing data ####

library(caret)
data4=preProcess(data3, method=c("center", "scale"))
#----------> method as center and scale says R to Normalize Data with 
#----------> Mean = 0 and SD = 1

data5 <- as.matrix(predict(data4, data3))
summary(data5)

#----------> Hopkins test is used to check if data is clusterable
library("factoextra")
library("clustertend")
set.seed(500)
n1=round(nrow(data5)/10)
fit2=get_clust_tendency(data5, n = n1, graph = TRUE)
fit2$hopkins_stat


#### Step-4 - Apply k-Means #### 
fviz_nbclust(data5, kmeans, method = "wss")
fviz_nbclust(data5, kmeans, method = "silhouette")


set.seed(123)
km=kmeans(data5, 3)
km

library(factoextra) #----------> clustering visualization
fviz_cluster(km, geom = "point", data = data5)


#### Step-5 - Extracting observations in clusters #### 

cl1=subset(data3,km$cluster==1)
cl2=subset(data3,km$cluster==2)
cl3=subset(data3,km$cluster==3)
table(km$cluster)

apply(cl1,2,mean)
apply(cl1,2,sd)
apply(cl2,2,mean)
apply(cl2,2,sd)
apply(cl3,2,mean)
apply(cl3,2,sd)


#### Step-6 - Hierarchical clusters ####

library(cluster)   
library(factoextra)
library(dendextend)


dist_val=dist(data5, method = "euclidean")
hcl1=hclust(dist_val, method = "average" )

plot(hcl1,cex = 0.8, hang = -1)

rect.hclust(hcl1, k = 3, border = 2:4)

fviz_nbclust(data5, FUN = hcut, method = "silhouette")

clus=cutree(hcl1, k = 3)
table(clus)
fviz_cluster(list(data = data5, cluster = clus))


#### Repeating Step-5 - Extracting observations in clusters #### 

cl1=subset(data3,clus==1)
cl2=subset(data3,clus==2)
cl3=subset(data3,clus==3)
table(clus)

apply(cl1,2,mean)
apply(cl1,2,sd)
apply(cl2,2,mean)
apply(cl2,2,sd)
apply(cl3,2,mean)
apply(cl3,2,sd)

