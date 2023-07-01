###### Decision Tree Case Study ######


#### Step-1 - Data collection ####
dataset=read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Decision Tree Case Study - R Program/R+Demonstration+-+Decision+Trees/R Demonstration - Decision Trees/demo_dt.csv")


#### Step-2 - Data split int Train & Test ####

traindata = dataset[1:90,]
testdata = dataset[91:100,]


#### Step-3 - Growing the decision tree ####

library(rpart)
fit = rpart(as.factor(HOSP) ~ AGE + SBP , method = "class", data= traindata)

### method "class" means "CLASSIFICATION"
### Before ~ sign, we mention response variabe and after ~ we mention predictor variables



#### Step-4 - Create attractive postscript plot of tree ####

library(rpart.plot)
rpart.plot(fit)


#### Step-5 - Predict ####

pred= predict(fit, testdata[,1:2],type="class")   #-------> testdata[,1:2] means we are using only first 2 columns and no other

table(testdata[,3], pred)


#### Therefore, out of total 10 datapoints in Test Data, the model has correctly predicted 9 datapoints
#### Hence, accuracy ~ 90%

