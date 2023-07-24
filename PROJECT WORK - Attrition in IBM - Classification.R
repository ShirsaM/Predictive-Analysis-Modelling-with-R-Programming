#################################
###  PROJECT WORK - CLASSIFICATION  
#################################

###### Section-1 - Collecting Data
#### Importing Data syntax

df = read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Classification Project- R program/WA_Fn-UseC_-HR-Employee-Attrition.csv")
head(df)

#### Data file imported in environment


###### Section-2 - Data Wrangling/Cleaning
#### Describing the data


# Let us look at a compact description of all objects in the data
str(df)

# Our data has 9 categorical variables, and we want these columns to be of type factor instead of character.

df$Attrition <- as.factor(df$Attrition)
df$BusinessTravel <- as.factor(df$BusinessTravel)
df$Department <- as.factor(df$Department)
df$EducationField <- as.factor(df$EducationField)
df$Gender <- as.factor(df$Gender)
df$JobRole <- as.factor(df$JobRole)
df$MaritalStatus <- as.factor(df$MaritalStatus)
df$Over18 <- as.factor(df$Over18)
df$OverTime <- as.factor(df$OverTime)


#### Dropping/deleting some columns - using select() or by taking a subset of df

install.packages("dplyr")
library("dplyr")

df = df %>% select(-c(EmployeeCount,StandardHours,EmployeeNumber, Over18))
str(df)

### ALTERNATIVELY: use the below code to remove columns
# df <- subset(df, select = -c(EmployeeCount,StandardHours,EmployeeNumber)) 



###### Section-3 - Data Preprocessing 

#### *Note: We have to mapout the string values of JobRole column since it returns a parse error in the later part (creating the glmboost model)
# for this we use function mapvalues() from plyr package

install.packages("plyr")
library("plyr")
df$JobRole <- mapvalues(df$JobRole, from = c("Healthcare Representative",
                                             "Human Resources",
                                             "Laboratory Technician",
                                             "Manager",
                                             "Manufacturing Director",
                                             "Research Director",
                                             "Research Scientist",
                                             "Sales Executive",
                                             "Sales Representative"),
                        to = c('healthcarerepresentative',
                               'humanresources',
                               'laboratorytechnician',
                               'manager',
                               'manufacturingdirector',
                               'researchdirector',
                               'researchscientist',
                               'salesexecutive',
                               'salesrepresentative'))


#### Map values of Response Variable - ATTRITION - from ['Yes', 'No'] to binary values [0,1]

df$Attrition <- mapvalues(df$Attrition, from = c('Yes', 'No') , 
                          to = c(1,0))

# Check the final structure of the dataframe
str(df)
nrow(df)
ncol(df)

#### Checking for Null / Duplicate entries

colSums(is.na(df))
sum(duplicated(df))


#### Obtaining summary of the cleaned data

summary(df)



###### Section-4 - EDA & Data Visualization
#### Performing the analysis on the response variable 

### DESCRIPTIVE STATISTICS ###

### Histogram of the response variable ###

install.packages("ggplot2")
install.packages("pastecs")
library(ggplot2)
library(pastecs)


ggplot(df,aes(x= Attrition)) + 
  geom_bar(color="yellow" , width=0.5) +
  ggtitle("Attrition Count") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# hjust takes 3 values: 0-left , 0.5-middle , 1-right
# vjust takes 3 values: 0-bottom , 0.5-middle , 1-top



### Visualizing correlation between Response variable Attrition & numeric features ###

plot1 = ggplot(df, aes(x= JobSatisfaction , fill= Attrition)) +
  geom_bar(color="grey" , width=0.5) +
  scale_fill_manual(values = c("1" = "#BB2936","0"="#2A7BBA")) +
  ggtitle("Attrition vs Job Satisfaction") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  
plot2 = ggplot(df, aes(x= RelationshipSatisfaction , fill= Attrition)) +
  geom_bar(color="grey" , width=0.5) +
  scale_fill_manual(values = c("1" = "#BB2936","0"="#2A7BBA")) +
  ggtitle("Attrition vs Work Relationship") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

plot3 = ggplot(df, aes(x= EnvironmentSatisfaction , fill= Attrition)) +
  geom_bar(color="grey" , width=0.5) +
  scale_fill_manual(values = c("1" = "#BB2936","0"="#2A7BBA")) +
  ggtitle("Attrition vs Work Environment") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

plot4 = ggplot(df, aes(x= TotalWorkingYears , fill= Attrition)) +
  geom_bar(color="grey" , width=0.5) +
  scale_fill_manual(values = c("1" = "#BB2936","0"="#2A7BBA")) +
  ggtitle("Attrition vs Total Experience") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

plot5 = ggplot(df, aes(x= JobInvolvement , fill= Attrition)) +
  geom_bar(color="grey" , width=0.5) +
  scale_fill_manual(values = c("1" = "#BB2936","0"="#2A7BBA")) +
  ggtitle("Attrition vs Job Involvement") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

plot6 = ggplot(df, aes(x= NumCompaniesWorked , fill= Attrition)) +
  geom_bar(color="grey" , width=0.5) +
  scale_fill_manual(values = c("1" = "#BB2936","0"="#2A7BBA")) +
  ggtitle("Attrition vs No. of Companies Worked") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# arranging the above 6 plots using grid.arrange() function from gridExtra library

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2, nrow = 3)


### Obtaining descriptive statistics ###
# We can summarize descriptive statistics of categorical data using ExpCustomStat() function of SmartEDA library

install.packages("SmartEDA")
library(SmartEDA)

ExpCustomStat(df, Cvar = c("Attrition"), gpby = FALSE)


### INFERENTIAL STATISTICS ###

# perform shapiro-wilk's test to check for normality of the observations


shapiro.test(df$JobSatisfaction)
shapiro.test(df$EnvironmentSatisfaction)
shapiro.test(df$RelationshipSatisfaction)
shapiro.test(df$TotalWorkingYears)
shapiro.test(df$JobInvolvement)
shapiro.test(df$NumCompaniesWorked)
### interpreting the results of the shapiro wilk's tests:
# H0: null hypothesis: data is normally distributed
# H1: alternate hypothesis: data is not normally distributed
# as the p-values of the test are < 0.05, we reject H0 and hence the observations are NOT Normal


# perform Two-sample t-tests between the two groups of the response variable based on the continuous predictors 
# check for any difference existing between the two groups at 5% level of significance.

t.test(JobSatisfaction ~ Attrition, data=df)
t.test(EnvironmentSatisfaction ~ Attrition, data=df)
t.test(RelationshipSatisfaction ~ Attrition, data=df)
t.test(TotalWorkingYears ~ Attrition, data=df)
t.test(JobInvolvement ~ Attrition, data=df)
t.test(NumCompaniesWorked ~ Attrition, data=df)

### interpreting the results of the t-tests:
# a negative t-value indicates that the mean of group 0 (Attrition=No) is significantly smaller than mean of group 1 (Attrition=Yes)
# p-value < 0.005 indicates the probability of such a large difference between the 2 groups by chance is very small
# here, H0: null hypothesis: mean of group 1 = mean of group 0
# and H1: alternate hypothesis: mean of group 1 != mean of group 0
# in all the Welch Two sample t-tests, we REJECT H0 (as H1 is true). 
# This tells us that there s a strong evidence that there is a statistically SIGNIFICANT difference between group 0 and group 1



###### Section-5 - Model Evaluation - LOGISTIC REGRESSION

#### Creating training and test set
set.seed(123)
nn=nrow(df)
nn

# Performing an 80%–20% split of the data into training and test sets

indx=sample(1:nn,0.8*nn)   #--------> choosing 80% of data
traindf=df[indx,]
testdf=df[-indx,]


head(traindf)
head(testdf)

nrow(traindf)  # 80% of dataset
nrow(testdf)   # 20% of dataset


#### Fitting full logistic regression (LR) model with all features

fullmod=glm(as.factor(Attrition) ~ Age + as.factor(BusinessTravel) + DailyRate + as.factor(Department) +
              DistanceFromHome + Education + as.factor(EducationField) + EnvironmentSatisfaction +
              as.factor(Gender) + HourlyRate + JobInvolvement + JobLevel + as.factor(JobRole) + JobSatisfaction +
              as.factor(MaritalStatus) + MonthlyIncome + MonthlyRate + NumCompaniesWorked + as.factor(OverTime) +
              PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel +
              TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole +
              YearsSinceLastPromotion + YearsWithCurrManager , 
            data = traindf, family = "binomial")

summary(fullmod)


#### Selecting features for fitting reduced logistic regression model
# there are many features in the data set, and using all of them for model building might be redundant. 
# So, we need to  perform feature selection using the stepAIC function in R.

library(MASS)
step=stepAIC(fullmod)

# the stepAIC function uses the lowest AIC value to select the best mode from the various subset models obtained by adjusting the number of predictors.
# now we train the model using only those features selected by stepAIC

mod2 =glm(as.factor(Attrition) ~ Age + as.factor(BusinessTravel) + DistanceFromHome + 
            as.factor(EducationField) + EnvironmentSatisfaction + JobInvolvement + 
            as.factor(JobRole) + JobSatisfaction + as.factor(MaritalStatus) + 
            NumCompaniesWorked + as.factor(OverTime) + RelationshipSatisfaction + 
            TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + 
            YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
            YearsWithCurrManager , 
            data = traindf, family = "binomial")

summary(mod2)

# through variable selection using StepAIC, we have reduced the predictors from 30 to 19.
# mod2 is the LR model with the optimal subset of predictors after variable selection
# Below are the components of the model:
# 1) Partial Regression Coefficients: are the factors of the model
# 2) Estimates: are the values of beta corresponding to the factors
# 3) Standard Errors: are the errors captured between the actual values of Estimates and the values derived from Maximum Likelihood Estimation.Lower values of Standard error inplies more precise estimated values.
# 4) z-value: test statistic value
# 5) p-value: p-values corresponding to the test statistic. Here, Null hypothesis is that the value of estimate = 0. 
# for features with p-value< 0.05, we Reject H0; that means Estimate value is non-zero.This indicates individual significance of the feature in the model.

# Let's consider the below predictors and their impacts on the response variable:
# (i) JobInvolvement: p-value <0.05, hence we Reject H0; indicates this predictor is significant 
# (ii) MaritalStatus-Single : p-value <0.05, hence we Reject H0; indicates this predictor is significant


# 6) Residual Deviance: is a measure that indicates the deviation of the fitted model from the most saturated model. 
# When residual deviance is low, the reliability and fit of the model is high. This means that the saturated and fitted models will be as close as possible.
# 7) AIC-Akaike Information Criterion: is a measure of relative amount of information lost.
# Lower the value of AIC, the better is the model fit. This technique is also used for model selection.
# In our LR model: Residual Deviance is 711.92 and AIC value is 777.92.
# The Residual Deviance is low and AIC value as well is the lowest post no. of iterations; this implies that mod2 is a better fitted model.



###### Section-6 - Predictive Analysis 
### predicting success probabilities (Y=1) using the LR model

head(testdf)

testdf_new=testdf[,c(1,3,6,8,9,12,14,15,16,19,20,23,25,26,27,28,29,30,31)]
head(testdf_new)

prediction = predict(mod2,testdf_new,type="response")
hist(prediction)

# From the histogram of predicted success probabilities, we have found that majority of the observations in the test set have a probability value <= 0.10 


#### Plotting ROC -  Receiver Operating Characteristic 

library(pROC)
testdf[,2]  #-----------> the response variable- Attrition

roc1=roc(testdf[,2],prediction,plot=TRUE,legacy.axes=TRUE)
#-----------> legacy.axes = TRUE, because we need to get 1-Specificity in X-axis


plot(roc1)
roc1$auc
#-----------> auc = Area Under Curve

# Remarks: the AUC value obtained is 0.8876, which is >0.8. This implies that the model has a good classification power.
# That is, the model is not good enough to differentiate between the ‘Attrition-Yes’ category with the ‘Attrition-No’ category in an optimal manner.


#### Using ROC in deciding threshold
thres=data.frame(sen=roc1$sensitivities, spec=roc1$specificities,thresholds=roc1$thresholds)
head(thres)
thres[thres$sen>0.9&thres$spec>0.6,]

# Remarks: from the output of allowed values of threshold, 
# we consider the threshold value as 0.9189. 
# The corresponding sensitivity is 0.9166 > 0.9 and specificity is 0.6341 > 0.6


#### Interpreting both continuous and categorical features in the LR model
# we will use the features considered earlier:

# (i) JobInvolvement: The coefficient (estimate) corresponding to JobInvolvement is -0.519.
# On increasing the Job Involvement by 1, the ODDS of attrition reduces by (1- e^-0.519)*100 = 40.48%. This means that more involvement at work has a detrimental effect on the attrition of the employees.

# (ii) MaritalStatus : The coefficient (estimate) corresponding to MaritalStatus - Married is 0.480.
# This means that ODDS of married employees leaving the organisation is (e^0.480 -1)*100 = 61.60% greater than unmarried employees.


#### Building Confusion Matrix

library(caret)
pred_Y=ifelse(prediction > 0.9189,1,0)
confusionMatrix(as.factor(testdf[,2]), as.factor(pred_Y))

# Remarks: 
# Using the threshold value of 0.9189 we created a model for the test data
# Accuracy of the model is high - 84.01%
# Sensitivity: 83.95% : there is low chance the model will wrongly classify if employees would be leaving the organisation
# Specificity: 100% : there is no  chance the model will wrongly classify if employees would not be leaving the organisation
# We also observe that there are about 47 employees who have actually not left the organisation but the model has wrongly classified them as exited employees


###### Above was the logistic regression model to predict f an employee would leave the organisation or not. And now we will deploy a Random Forest model and compare both of these models #######


###### Section-7 - Model Evaluation - RANDOM FOREST

library(randomForest)

head(df)
str(df)
# all categorical features are converted to factors already


# response variable Attrition, after that ~ ; and then . signifies we are considering all features for prediction
modRF=randomForest(Attrition~ ., df, ntree=500, mtry=8)
modRF

# Remarks: 
# the Confusion Matrix shows that (1218+48) = 1266 are correctly classified whereas 
# (15+189) = 204 are wrongly classified by the Random Forest model.
# Therefore, the Accuracy of this model is (1266/(1266+204)) = 86.12%
# the Out-Of-Bag error rate is 13.88%, which means about 13.88% of OOB samples are wrongly classified.
# The OOB Error rate is LOW; hence using a Random forest model is far more ACCURATE than using an LR model to classify.


str(testdf)

### we randomly choose an employee entry and predict if he/she would be exiting form the organisation using random forest model
testdf[5,]  #--------> randomly chosen 5th entry

### We remove the response variable column 2 to get correct prediction
predict(modRF,testdf[5,-2],type="response")

#### Therefore, the response we get is '0' that is the employee will not be leaving the organisation.
### the model accuracy is high as we have already obtained earlier and with the above prediction we find
### the actual and predicted value of response variable- Attrition is the same = 0.
### on tuning the hyperparameters - ntree and mtry, we get the accuracy to be similar



##### Therefore, concluding from the above 2 models: Logistic Regression (LR) and Random Forest (RF) model we find that:
### the Accuracy of RF model 86.12% > accuracy of LR model 84.01%
### Hence, RF model does better for classifying the Attrition in the organisation than LR model
