# cross validation model frame
# 181114 yuqing zhao

df= read.csv('~/Documents/GitHub/18FMath564-pj/data/train_processed.csv')
testdf= read.csv('~/Documents/GitHub/18FMath564-pj/data/test_processed.csv')
#remove first two columns which are index
df=df[,-c(1,2)]
testdf=testdf[,-c(1,2)]


# define rmlse function
rmlse <- function(yhat, y) {
  n <- length(yhat)
  return(sqrt((1/n)*sum((log(yhat)-log(y))^2)))
}


# scale the numeric data since scaled data are required in some models
# I think we need to scale data and we can discuss this
# numv = names(df[,sapply(df,is.numeric)]) # numeric 
# numv
# catv = names(df[,sapply(df,is.factor)]) # numeric 
# catv
# logv = names(df[,sapply(df,is.logical)]) # binary
# logv
# df_scale = scale(df[,numv],center=FALSE)
# df_scale = data.frame(tdf_scale,df[,catv],df[,logv])

# perform cross-validtion on training data set
library(caret)
controlParameter=trainControl(method = "cv",number = 10,savePredictions = TRUE)

# Reference:
# http://topepo.github.io/caret/train-models-by-tag.html


# linear regression
set.seed(564)
lm = train(SalePrice~., data = df,method='lm',trControl=controlParameter)
summary(lm)

# Feature selection by AIC stepwise algorithm
# Errors for below three method, I am not sure why is that
library(leaps)
lm_forward=train(SalePrice~.,data=df,method='leapForward',trControl=controlParameter)
lm_backward=train(SalePrice~.,data=df,method='leapBackward',trControl=controlParameter)
lm_step=train(SalePrice~.,data=df,method='leapSeq',trControl=controlParameter)

# Generalized Linear Model with Stepwise Feature Selection
# glm_stepAIC=train(SalePrice~.,data=df,method='glmStepAIC',trControl=controlParameter)

# Linear Regression with Stepwise Selection
lm_stepAIC=train(SalePrice~.,data=df,method='lmStepAIC',trControl=controlParameter)


# Tree-Based model
# CART Tree
tree_cp=train(SalePrice~.,data=df,method='rpart',trControl=controlParameter)
# tree_1se=train(SalePrice~.,data=df,method='rpart1SE',trControl=controlParameter)


# Make Prediction
lm_pred = predict(lm,df)
# lm_forward_pred = predict(lm_forward,df)
# lm_backward_pred = predict(lm_backward,df)
# lm_step_pred = predict(lm_step,df)
lm_stepAIC_pred = predict(lm_stepAIC,df)


# To calculate rmlse
lm_rmlse = rmlse(abs(lm_pred), df$SalePrice)
lm_stepAIC_rmlse = rmlse(abs(lm_stepAIC_pred), df$SalePrice)



# tree regression via rpart package
# grow the tree
library(rpart)
tr=rpart(df$SalePrice~.,data = df, method = 'anova')
printcp(tr)
plotcp(tr)
summary(tr)
plot(tr, uniform=TRUE, main="Regression Tree")
text(tr, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfit= prune(tr, cp=0.020665) # use the minimum Cp
# plot the pruned tree 
plot(pfit, uniform=TRUE, main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

