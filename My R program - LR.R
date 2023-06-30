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