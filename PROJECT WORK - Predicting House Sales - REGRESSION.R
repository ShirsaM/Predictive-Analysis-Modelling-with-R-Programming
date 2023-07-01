###### Section-1 - Collecting Data
#### Importing Data syntax

df = read.csv("C:/Users/ADMIN/Documents/IIMK-UpGrad/Regression Project/kc_house_data.csv")
head(df)
#### Data file imported in environment


###### Section-2 - Data Wrangling/Cleaning
#### Describing the data

summary(df)
nrow(df)
ncol(df)

#### Checking for Null / duplicate entries

colSums(is.na(df))
sum(duplicated(df))

#### Converting date column values to "Date" format

install.packages("anytime")
library("anytime")
df$date <- anytime(df$date)
df$date <- as.Date(df$date)

str(df)   # compactly describes object 

# creating a new column with only the year from date column
df$selling_year <- as.integer(format(df$date , format = "%Y"))
summary(df$selling_year)


#### Dropping/deleting some columns - using select()

install.packages("dplyr")
library("dplyr")

df = df %>% select(-c(date, id, zipcode))
str(df)


###### Section-3 - Data Preprocessing using One-Hot-Encoding
#### Creating a new variable- if house was renovated or not

df$renovated <- 0

for(i in 1:nrow(df)){
  if(df$yr_renovated[i] != 0)
    df$renovated[i] <- 1
}
str(df$renovated)

#### Replacing variables yr_built and yr_renovated with a single column

for(i in 1:nrow(df)){
  if(df$yr_renovated[i] != 0)
    df$yr_built[i] <- df$yr_renovated[i]
}

df$age <- df$selling_year - df$yr_built

summary(df$age)


df = df %>% select(-c(yr_built , yr_renovated))
df = df %>% select(-c(selling_year))
str(df)


###### Section-4 - EDA & Data Visualization
#### Performing the univariate analysis on the response variable (Y = price)

install.packages("ggplot2")
install.packages("pastecs")
library(ggplot2)
library(pastecs)

hist(df$price, 
     col="yellow", 
     main="Histogram Distribution for House Price",
     xlab="Price range",
     breaks = 20)

boxplot(df$price,
        col = "red",
        main = "Box Plot of the response variable")

# another syntax for creating box plot using ggplot
ggplot( df, aes(y=price)) + geom_boxplot() + scale_fill_grey()


#### Visualizing correlation between features

install.packages("corrplot")
library(corrplot)

corrplot(cor(df), main="Correlatin Matrix")

pairs(~price+bathrooms+sqft_living+grade+sqft_above+sqft_living15 , 
      data= df, 
      main="High Positive Correlation",
      col="green")

pairs(~price+bedrooms+floors+view+sqft_basement+lat , 
      data= df, 
      main="Low Positive Correlation",
      col="orange")


#### Visualizing response vs important predictor

plot(price~bathrooms, data=df, main="Price vs Bathrooms", col="blue")
plot(price~sqft_living, data=df, main="Price vs Living Squarefeet", col="blue")
plot(price~grade, data=df, main="Price vs Grade", col="blue")


#### Statistical Description of response variable

stat.desc(df$price)


###### Section-5 - Model Evaluation
#### Taking a subset of the original data

df2 = subset(df, df$bedrooms <=20)
head(df2)

#### Train and test split

install.packages("lmtest")
library(lmtest)

n=nrow(df2)
samp <- sample(n, round(0.9*n), replace = FALSE)

train <- df2[samp,]
test <- df2[-samp,]

head(train)
head(test)

#### Model fitting

mod1 = lm(data=train, price~.)
summary(mod1)

#### Presenting model with the optimal subset of predictors- excluding floors & sqft_basement

mod2 = lm((price)~as.factor(view)+bedrooms+bathrooms+sqft_living+sqft_lot+waterfront+condition+grade+sqft_above+lat+long+sqft_living15+sqft_lot15+renovated+age ,
          data=train)
summary(mod2)


###### Section-6 - Regression Diagnostics

#### Checking whether residuals are distributed normally
plot(mod2)   # plotting QQ plot
shapiro.test(mod2$residuals)
# here we find that residuals are weakly normal as the QQ-plot has no strong Linearity. 

#### Checking whether residuals are homoscedastic
# the plot follows a parabolic pattern suggesting that a quadratic equation might be a better fit as compared to a linear regression model.


#### Checking whether residuals are independent

install.packages("lmtest")
library(lmtest)
dwtest(mod2)

#### Checking for Multicollinearity

install.packages("car")
library(car)
vif(mod2)


###### Section-7 - Conclusions from Model

# We found that MOD2 is a better fitted mode than MOD1, after taking an optimal subset of predictors
# Total number 0f predictors are:- 18 where 14 numeric predictors + (5-1) categorical predictors

# From the model summary of MOD2, We fund that almost all predictor variables (excluding sqft_lot), are highly significant in predicting Y.
# 3stars - high significance because the corresponding p-value is cosiderably less than 0.05
# 2stars - fairly high significance because the corresponding p-value is much less than 0.05

# The R2 is 0.6948, that is, 69.48% of the total variability in the response variable, ‘Price’ is being explained by the full MLR model. 
# The adjusted R2 is 0.6945; 69.45% of the variability in 'Price' is explained by the full MLR model after being adjusted for redundant predictors.
# A low difference between R2 and adjusted R2 suggests that not many redundant predictors are present in the model. 

# Based on the overall model parameters, we can also comment that the model fit is adequate.
# The p-value generated on testing for the overall model significance comes out to be less than 2.2e-16. 
# Hence, we can confidently say the full MLR model is significant at a 5% level of significance.

# The test used here is Null hypothesis H0: There is atleast one predictor which is insignificant
#                       Alternate Hypothesis H1: Each of the individual predictor is significant
# Here, from the p-value of the F-Test, we conclude that we reject H0 and that Each of the individual predictor is significant

# Model Assumptions:
# Observing from the QQ-plot, the normal probability plot of residuals, the points tend to follow a straight line, hence we conclude the residuals to be weakly normal
# the plot follows a parabolic pattern suggesting that a quadratic equation might be a better fit as compared to a linear regression model.This indicates violation of hmoscedasticity of residuals.
# This test checks whether the random errors are auto-correlated or not. The null and alternative hypothesis for this test is given by:
# H0: The residuals from the linear regression are uncorrelated (independence)
# H1: There exists some form of autocorrelation (dependence) among the residuals.
# In this case the p-value obtained from DW Test is less than 0.05, indicating that there is some form of auto-correlation (dependence) of residuals
# In this model, we do not have any predictors with VIF greater than 10, indicating absence of multicolinearity.


# The model is fairly significant in achieving the purpose of prediction of House Sale prices in King Country, USA based on historical data of 2014-2015.




