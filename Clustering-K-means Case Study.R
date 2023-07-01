###### k-Means Clustering Case Study - Two Wheeler Servicing ###### 


#### Step-1 - Data collection ####
dataset=read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Clustering Case Study - R Program/Two-wheeler+Case+Study_k+means/Two-wheeler Case Study_k means/data1.csv")


#### Step-2 - Data Wrangling ####

### Filtering data: considering only those customers for the analysis for whom the 3rd visit happened 
data2 = subset(dataset, dataset$NUM_PAID_VIS==2 & is.na(dataset$VISIT3)==FALSE)
head(data2)

### Checking number of observations (customers) on whom the analysis will be performed 
N=nrow(data2)
N

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


# Therefore, following are the implications:

# Based on the type of bike usage by the customers, the company can plan the time and resources required for the servicing.
# It can plan for different service centers guiding different customer segments to each of them depending on the amount of servicing required.
