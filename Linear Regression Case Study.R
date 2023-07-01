###### Section 1
#### Importing Data syntax

datafull=read.csv("data1.csv")
#### Data1 file imported in environment

#### Data2 is a part of Data1, syntax
data2= subset(data1,data1$VISIT3 == 1)
head(data2)

install.packages("ggplot2")
library(ggplot2)

install.packages("pastecs")
library(pastecs)

#### Visual-1: Histogram
qplot(data2$VISTIME_23, 
      geom = "histogram", 
      binwidth=15, 
      main="Histogram for VISTIME23", 
      xlab = "VISTIME23",
      fill=I("orange"),
      col=I("blue")) + theme_bw()

#### Visual-2: Boxplot
ggplot(data2, aes(y=VISTIME_23)) + geom_boxplot() + scale_fill_grey() + theme_classic()

#### Descriptive statistics
stat.desc(data2$VISTIME_23)


##### creating linear model

head(traindata)
head(testdata)

mod1=lm((VISTIME_23)~as.factor(PROD_MOD2)+as.factor(CITY2)+AGE+as.factor(GEN)+INCOME+as.factor(LOAN)+NUM_FREE_VIS+FREE_AV_US+PAID_AV_US+PAID_AV_TIME+PAID_AV_FD+NUM_OVRCH,data=traindata)

summary(mod1)

##### model evaluation

testdata1= testdata[ , c(5,7,8,12,14,18)]
pred_val = predict(mod1,testdata)
cor(testdata$VISTIME_23, pred_val)
sqrt(mean((testdata$VISTIME_23 - pred_val))) 


#### RESIDUAL ASSUPMPTIONS
plot(mod1)

shapiro.test(mod1$residuals)

install.packages("lmtest")
library(lmtest)
dwtest(mod1)

###### Feature selection

install.packages("MASS")
library(MASS)
step.model=stepAIC(mod1, direction = "both")
summary(step.model)