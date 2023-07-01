###### Random Forest Case Study ######


#### Step-1 - Data collection ####
dataset=read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Random Forest Case Study - R Program/R+Demonstration+-+Random+Forests/R Demonstration - Random Forests/data1.csv")


#### Step-2 - Data Wrangling ####

### Filtering data: considering only those customers for the analysis for whom the 3rd visit happened 
data2 = subset(dataset, dataset$NUM_PAID_VIS==2 & is.na(dataset$VISIT3)==FALSE)

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


#### Step-5 - Random Forest ####
library(randomForest)

### Converting response variable to Categoriacla and other varaiables too
data_an$VISIT3=as.factor(data_an$VISIT3)
data_an$PROD_MOD2=as.factor(data_an$PROD_MOD2)
data_an$CITY2=as.factor(data_an$CITY2)
data_an$GEN=as.factor(data_an$GEN)
data_an$LOAN=as.factor(data_an$LOAN)


### response variable VISIT3, after that ~ and then . signifies, e are considering all features for precdiction
modRF=randomForest(VISIT3~ ., data=data_an,ntree=500, mtry=8)
modRF

testdata$VISIT3=as.factor(testdata$VISIT3)
testdata$PROD_MOD2=as.factor(testdata$PROD_MOD2)
testdata$CITY2=as.factor(testdata$CITY2)
testdata$GEN=as.factor(testdata$GEN)
testdata$LOAN=as.factor(testdata$LOAN)


### We remove the response variable column 13 to get correct prediction
predict(modRF,testdata[3,-13],type="response")


#### Therefore, the response we get is '0' that is 
#### the customer will not be turning up for 3rd paid visit 
