hpdata <- read.csv("C:/Users/ZH1/Desktop/Fall 2018 MATH564/project/all/sample_submission.csv", header = TRUE)
train <- read.csv("C:/Users/ZH1/Desktop/Fall 2018 MATH564/project/all/train.csv", header = TRUE)
test <- read.csv("C:/Users/ZH1/Desktop/Fall 2018 MATH564/project/all/test.csv", header = TRUE)
#review  first 5 rows of data
head(hpdata, n=5)
dim(hpdata)
str(hpdata)
summary(hpdata)
summary(train)
summary(test)
#boxplot for train data
library(reshape)
melttrain <- melt(train)
boxplot(data=melttrain, value~variable)
library(ggplot2)
ptrain <- ggplot(melttrain, aes(factor(variable), value)) 
ptrain + geom_boxplot() + facet_wrap(~variable, scale="free")
#boxplot for test data
melttest <- melt(test)
ptest <- ggplot(melttest, aes(factor(variable), value)) 
ptest + geom_boxplot() + facet_wrap(~variable, scale="free")
#boxplot for house price data
melthp <- melt(hpdata)
php <- ggplot(melthp, aes(factor(variable), value)) 
php + geom_boxplot() + facet_wrap(~variable, scale="free")
#correlation matrix
cor(train[,unlist(lapply(train, is.numeric))])
cor(test[,unlist(lapply(test, is.numeric))])
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(train[,unlist(lapply(train, is.numeric))])
library(corrplot)
x <- cor(train[,unlist(lapply(train, is.numeric))])
corrplot(x,type="upper")
install.packages("qdap")
library(qdap)
dist_tab(hpdata)
#histogram of saleprice
require("ggplot2")
ggplot(data = hpdata, aes(x = SalePrice)) + 
  geom_histogram()
#density plot of saleprice
ggplot(data = hpdata, aes(x = SalePrice)) + 
  stat_density()
#density plot for train data
ggplot(data = melttrain, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
#density plot for test data
ggplot(data = melttest, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")


