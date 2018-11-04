# math564 project data explore
# 181104 yuqing zhao

df=read.csv('train.csv')
summary(df)

# check varaible type
catv=names(df[,sapply(df,is.factor)]) # categorical
catv
numv=names(df[,sapply(df,is.numeric)]) # numeric 
numv
logv=names(df[,sapply(df,is.logical)]) # binary
logv

for(i in numv){
  boxplot(df[i],main = paste("Boxplot of ", sep='',i),xlab = i )
}

library(corrplot)
cor = cor(df[numv]) #get correaltion matrix for numeric variable
corrplot.mixed(cor) # really messay figure, need more explore


# plot(df[numv]) # too large to plot


l=lm(SalePrice~., data=df[numv])
plot(l) # check linear regression assumption.  There are outliers and residual assumption does not stand

car::vif(l) # Variance inflation factors.
# Error in vif.default(l) : there are aliased coefficients in the model
# The error indicates multicolinearity
alias(l)

plot(l$fitted.values, l$residuals)
# need to plot each variable versus residual


