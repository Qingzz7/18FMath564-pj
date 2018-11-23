# cross validation model frame
# 181114 yuqing zhao

df= read.csv('/Users/Grant/gdfs/My Drive/F2018/MATH564/18FMath564-pj/data/train_processed.csv')
testdf= read.csv('/Users/Grant/gdfs/My Drive/F2018/MATH564/18FMath564-pj/data/test_processed.csv')
#remove first two columns which are index
df=df[,-c(1,2)]
testdf=testdf[,-c(1,2)]


# There are linearly dependent factors amongst the 'No Garage' and 'No Basement' type variables.
# GarageFinish, GarageQual, GarageCond, BsmntFinType1, BsmntCond, Exterior2nd have NAs
# There's major overlap in general between these factors - see contingency tables 

xtabs(~GarageQual+GarageCond+GarageFinish, data=df)
xtabs(~BsmtCond+BsmtFinType1, data=df)

# Keep GarageQual and BsmtCond

df <- df[, -c(which(colnames(df) == "GarageFinish"),
              which(colnames(df) == "Exterior2nd"),
              which(colnames(df) == "GarageCond"),
              which(colnames(df) == "BsmtFinType1"))]

# define rmlse function
rmlse <- function(yhat, y) {
  n <- length(yhat)
  return(sqrt((1/n)*sum((yhat-y)^2))) # no need for log here since y has been log transformed
}


# scale the numeric data since scaled data are required in some models
# I think we need to scale data and we can discuss this
numv = names(df[,sapply(df,is.numeric)]) # numeric 
numv
catv = names(df[,sapply(df,is.factor)]) # numeric 
catv
logv = names(df[,sapply(df,is.logical)]) # binary
logv
df_scale = scale(df[,numv],center=FALSE)
df_scale = data.frame(df_scale,df[,catv],df[,logv])

# perform cross-validtion on training data set
library(caret)
controlParameter=trainControl(method = "cv",number = 10,savePredictions = TRUE)

# Reference:
# http://topepo.github.io/caret/train-models-by-tag.html


# linear regression
set.seed(564)
lm = train(SalePrice~., data = df,method='lm',trControl=controlParameter)
coef(lm)
summary(lm)

# Feature selection by AIC stepwise algorithm
# Errors for below three method, I am not sure why is that
library(leaps)
lm_forward=train(SalePrice~.,data=df,method='leapForward',trControl=controlParameter)
lm_backward=train(SalePrice~.,data=df,method='leapBackward',trControl=controlParameter)
lm_step=train(SalePrice~.,data=df,method='leapSeq',trControl=controlParameter)
# Linear Regression with Stepwise Selection
# lm_stepAIC=train(SalePrice~.,data=df,method='lmStepAIC',trControl=controlParameter)

# Generalized Linear Model with Stepwise Feature Selection
# glm_stepAIC=train(SalePrice~.,data=df,method='glmStepAIC',trControl=controlParameter)

# models with penalty
# Ridge regression
lambdas = 10^seq(10, -2, length = 100)
# ridge_fit <- cv.glmnet(train_matrix, y, alpha=0, lambda=lambdas)
ridgeGrid=expand.grid(alpha=0,lambda=lambdas)
lm_ridge=train(SalePrice~., data=df, method = 'glmnet',trControl=controlParameter,tuneGrid=ridgeGrid)

# Lasso regression
# lasso_fit <- cv.glmnet(train_matrix, y, alpha=1, lambda=lambdas)
lassoGrid=expand.grid(alpha=1,lambda=lambdas)
lm_lasso=train(SalePrice~., data=df, method = 'glmnet',trControl=controlParameter,tuneGrid=lassoGrid)

# Elasticnet regression
lm_elas=train(SalePrice~., data=df, method = 'glmnet', trControl=controlParameter)



# Tree-Based model
# CART Tree, cannot set method to be anova 
tree_cp=train(SalePrice~.,data=df,method='rpart',trControl=controlParameter)
# tree_1se=train(SalePrice~.,data=df,method='rpart1SE',trControl=controlParameter)



# Make Prediction
lm_pred = predict(lm,df)
lm_forward_pred = predict(lm_forward,df)
lm_backward_pred = predict(lm_backward,df)
lm_step_pred = predict(lm_step,df)
lm_stepAIC_pred = predict(lm_stepAIC,df)
lm_lasso_pred=predict(lm_lasso,df)
lm_ridge_pred=predict(lm_ridge,df)
lm_elas_pred=predict(lm_elas,df)
tree_cp_pred=predict(tree_cp,df)

# To calculate rmlse
lm_rmlse = rmlse(abs(lm_pred), df$SalePrice)
lm_forward_rmlse = rmlse(abs(lm_forward_pred), df$SalePrice) 
lm_backward_rmlse = rmlse(abs(lm_backward_pred), df$SalePrice)
lm_step_rmlse = rmlse(abs(lm_step_pred), df$SalePrice)
lm_stepAIC_rmlse = rmlse(abs(lm_stepAIC_pred), df$SalePrice)
lm_lasso_rmlse=rmlse(abs(lm_lasso_pred), df$SalePrice)
lm_ridge_rmlse=rmlse(abs(lm_ridge_pred), df$SalePrice)
lm_elas_rmlse=rmlse(abs(lm_elas_pred), df$SalePrice)
tree_cp_rmlse=rmlse(abs(tree_cp_pred), df$SalePrice)




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

