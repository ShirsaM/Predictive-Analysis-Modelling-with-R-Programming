###### Logistic Regression Case Study - Two Wheeler Servicing ###### 


#### Step-1 - Data collection ####
dataset=read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Logistic Regression Case Study - R Program/data1.csv")

#### Step-2 - Data Wrangling ####

### Filtering data: considering only those customers for the analysis for whom the 3rd visit happened 
data2 = subset(dataset, dataset$NUM_PAID_VIS==2 & is.na(dataset$VISIT3)==FALSE)
head(data2)

### Checking number of observations (customers) on whom the analysis will be performed 
N=nrow(data2)
N

### Filtering relevant columns needed for analysis
data_an=data2[,c(5,7,8,9,10,11,12,14,15,16,17,18,19)]
nn=nrow(data_an)
nn


#### Step-3 - Data split int Train & Test ####

### Creating training and test set
set.seed(123)
indx=sample(1:nn,0.9*nn)   #--------> choosing 90% of data
traindata=data_an[indx,]
testdata=data_an[-indx,]


### Fitting full logistic regression (LoR) model with all features
fullmod=glm(as.factor(VISIT3)~as.factor(PROD_MOD2)
            +as.factor(CITY2)+AGE+as.factor(GEN)+INCOME+as.factor(LOAN)
            +NUM_FREE_VIS+FREE_AV_US+PAID_AV_US+PAID_AV_TIME+PAID_AV_FD
            +(NUM_OVRCH),data=traindata,family="binomial")
#------------> the family is mentioned binomial for logistic regression and 
#------------> if it is poisson regression then mention poisson.
#------------> GLM - generalised linear model

summary(fullmod)

#### Selecting features for fitting reduced logistic regression model
library(MASS)
step=stepAIC(fullmod)

mod2=glm(as.factor(VISIT3) ~ as.factor(PROD_MOD2) + as.factor(LOAN) + 
           NUM_FREE_VIS + FREE_AV_US + PAID_AV_US, family = "binomial", 
         data = traindata)
summary(mod2)



#### Step-4 - Predictive Analysis ####

### predicting success probabilities using the LR model
testdata_new=testdata[,c(1,6,7,8,9)]
pred_prob=predict(mod2,testdata_new,type="response")
hist(pred_prob)


### predicting success probability for an individual
sampletest=data.frame(t(c(6,0,3,3500,5000)))
colnames(sampletest)=c("PROD_MOD2","LOAN","NUM_FREE_VIS","FREE_AV_US","PAID_AV_US")
predict(mod2,sampletest,type="response")


#### Plotting ROC 
library(pROC)
roc1=roc(testdata[,13],pred_prob,plot=TRUE,legacy.axes=TRUE)
#-----------> legacy.axes = TRUE, because we need to get 1-Specificity in X-axis


plot(roc1)
roc1$auc
#-----------> auc = Area Under Curve


#### Using ROC in deciding threshold
thres=data.frame(sen=roc1$sensitivities, spec=roc1$specificities,thresholds=roc1$thresholds)
thres[thres$sen>0.65&thres$spec>0.4,]


library(caret)
pred_Y=ifelse(pred_prob > 0.098,1,0)
confusionMatrix(as.factor(testdata[,13]), as.factor(pred_Y))



