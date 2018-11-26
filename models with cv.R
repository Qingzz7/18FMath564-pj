library(rpart)
library(caret)
library(leaps)
library(glmnet)
library(ggplot2)

set.seed(564)


# cross validation model frame
# 181114 yuqing zhao

train_df <- read.csv('/Users/Grant/gdfs/My Drive/F2018/MATH564/18FMath564-pj/data/train_processed.csv')
val_df <- read.csv('/Users/Grant/gdfs/My Drive/F2018/MATH564/18FMath564-pj/data/test_processed.csv')

#remove first two columns which are index
train_df <- train_df[,-c(1,2)]
val_df <- val_df[,-c(1,2)]


# There are linearly dependent factors amongst the 'No Garage' and 'No Basement' type variables.
# GarageFinish, GarageQual, GarageCond, BsmntFinType1, BsmntCond, Exterior2nd have NAs
# There's major overlap in general between these factors - see contingency tables 
# Also removing GarageType since it's insignificant and linearly dependent amongst NoGarage factors
# Remove Basement Condition in favor of Basement Quality

# xtabs(~GarageQual+GarageCond+GarageFinish, data=train_df)
# xtabs(~BsmtCond+BsmtFinType1, data=train_df)

# Keep GarageQual and BsmtCond

train_df <- train_df[, -c(which(colnames(train_df) == "GarageFinish"),
                            which(colnames(train_df) == "Exterior2nd"),
                            which(colnames(train_df) == "GarageCond"),
                            which(colnames(train_df) == "BsmtFinType1"))]

val_df <- val_df[, -c(which(colnames(val_df) == "GarageFinish"),
                          which(colnames(val_df) == "Exterior2nd"),
                          which(colnames(val_df) == "GarageCond"),
                          which(colnames(val_df) == "BsmtFinType1"))]

# define rmlse function
rmlse <- function(yhat, y) {
  n <- length(yhat)
  return(sqrt((1/n)*sum((yhat-y)^2))) # no need for log here since y has been log transformed
}


# scale the numeric data since scaled data are required in some models
# I think we need to scale data and we can discuss this
numv <- names(train_df[,sapply(train_df,is.numeric)]) # numeric 
catv <- names(train_df[,sapply(train_df,is.factor)]) # numeric 
logv <- names(train_df[,sapply(train_df,is.logical)]) # binary
df_scale <- scale(train_df[,numv],center=FALSE)
df_scale <- data.frame(df_scale,train_df[,catv],train_df[,logv])

# perform cross-validtion on training data set
controlParameter=trainControl(method = "cv", number = 10, savePredictions = TRUE)

# Reference:
# http://topepo.github.io/caret/train-models-by-tag.html


# linear regression
lm_ols <- train(SalePrice~.,
                data = train_df,
                method='lm',
                trControl=controlParameter)
ols_fit <- lm_ols$finalModel

# Some manual feature engineering

# Try a linear model with the most significant feature from each category (given OLS)
lm_cats <- train(SalePrice~TotBathrooms+SaleCondition+GarageArea+
                   KitchenQual+GrLivArea+TotalBsmtSF+OverallCond+OverallQual+
                   BldgType+Condition1+MSZoning,
                 data=train_df, 
                 method="lm",
                 trControl=controlParameter)
cats_fit <- lm_cats$finalModel

# Feature selection by AIC stepwise algorithm
# Errors for below three method, I am not sure why is that
lm_forward <- train(SalePrice~., 
                    data=train_df,
                    method='leapForward',
                    trControl=controlParameter,
                    tuneGrid = expand.grid(nvmax = seq(1, 180, 1)))
fwd_fit <- lm_forward$finalModel

lm_backward <- train(SalePrice~., 
                     data=train_df,
                     method='leapBackward', 
                     trControl=controlParameter, 
                     tuneGrid = expand.grid(nvmax = seq(1, 180, 1)))
bwd_fit <- lm_backward$finalModel
#lm_step <- train(SalePrice~.,
#                 data=train_df,
#                 method='leapSeq',
#                 trControl=controlParameter,
#                 tuneGrid = expand.grid(nvmax = seq(1, 50, 1)))

# Linear Regression with Stepwise Selection - warning: takes a while
# lm_stepAIC <- train(SalePrice~.,data=train_df,method='lmStepAIC',trControl=controlParameter)

# Generalized Linear Model with Stepwise Feature Selection
# glm_stepAIC <- train(SalePrice~.,data=df,method='glmStepAIC',trControl=controlParameter)

# Regularization methods

# Ridge regression - not converging nicely
lambdas <- 10^seq(-1, -5, length = 100) # This NaNs after like 400
ridgeGrid <- expand.grid(alpha=0,lambda=lambdas)
lm_ridge <- train(SalePrice~., data=train_df, method = 'glmnet', trControl=controlParameter, tuneGrid=ridgeGrid)
ridge_fit <- lm_ridge$finalModel

# Lasso regression - this one converges better
lambdas <- 10^seq(-2, -5, length = 300) # Opt lambda probably between .00001 and .01
lassoGrid <- expand.grid(alpha=1,lambda=lambdas)
lm_lasso <- train(SalePrice~., data=train_df, method = 'glmnet', trControl=controlParameter, tuneGrid=lassoGrid)
lasso_fit <- lm_lasso$finalModel

# Elasticnet regression
elasGrid <- expand.grid(alpha=seq(0, 1, length=21),lambda=lambdas)
lm_elas <- train(SalePrice~., data=train_df, method = 'glmnet', trControl=controlParameter, tuneGrid=elasGrid)
elas_fit <- lm_elas$finalModel

# Tree-Based model
# CART Tree, cannot set method to be anova 
treeGrid <- expand.grid(cp=10^seq(-5,-3, length=101))
tree_cp <- train(SalePrice~.,
                 data=train_df,
                 method='rpart',
                 trControl=controlParameter,
                 tuneGrid=treeGrid)
# Zeroing in on the optimal value
treeFineGrid <- expand.grid(cp=seq(0.0002,.0004, length=101))
tree_cp <- train(SalePrice~.,
                 data=train_df,
                 method='rpart',
                 trControl=controlParameter,
                 tuneGrid=treeFineGrid)
tree_fit <- tree_cp$finalModel

# tree_1se <- train(SalePrice~.,data=df,method='rpart1SE',trControl=controlParameter)

# tree regression via rpart package
# grow the tree
tree_anova <- rpart(train_df$SalePrice~.,data = train_df, method = 'anova')
printcp(tree_anova)
plotcp(tree_anova)
summary(tree_anova)
plot(tree_anova, uniform=TRUE, main="Regression Tree")
text(tree_anova, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfit <- prune(tree_anova, cp=0.020665) # use the minimum Cp
# plot the pruned tree 
plot(pfit, uniform=TRUE, main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


# Make Prediction
lm_ols_pred <- predict(lm_ols,train_df)
lm_cats_pred <- predict(lm_cats,train_df)
lm_forward_pred <- predict(lm_forward,train_df)
lm_backward_pred <- predict(lm_backward,train_df)
#lm_step_pred <- predict(lm_step,train_df)
# lm_stepAIC_pred = predict(lm_stepAIC,df)
lm_lasso_pred <- predict(lm_lasso,train_df)
lm_ridge_pred <- predict(lm_ridge,train_df)
lm_elas_pred <- predict(lm_elas,train_df)
tree_cp_pred <- predict(tree_cp,train_df)

# To calculate rmlse
lm_rmlse <- rmlse(abs(lm_ols_pred), train_df$SalePrice)
lm_cats_rmlse <- rmlse(abs(lm_cats_pred), train_df$SalePrice)
lm_forward_rmlse <- rmlse(abs(lm_forward_pred), train_df$SalePrice) 
lm_backward_rmlse <- rmlse(abs(lm_backward_pred), train_df$SalePrice)
# lm_step_rmlse <- rmlse(abs(lm_step_pred), train_df$SalePrice)
# lm_stepAIC_rmlse = rmlse(abs(lm_stepAIC_pred), df$SalePrice)
lm_lasso_rmlse <- rmlse(abs(lm_lasso_pred), train_df$SalePrice)
lm_ridge_rmlse <- rmlse(abs(lm_ridge_pred), train_df$SalePrice)
lm_elas_rmlse <- rmlse(abs(lm_elas_pred), train_df$SalePrice)
tree_cp_rmlse <- rmlse(abs(tree_cp_pred), train_df$SalePrice)

# Tabular scores for comparison

rmlse_scores <- c(lm_rmlse, lm_cats_rmlse, lm_forward_rmlse,
                  lm_backward_rmlse, lm_ridge_rmlse, lm_lasso_rmlse,
                  lm_elas_rmlse, tree_cp_rmlse
                  # lm_step_rmlse,
                  # lm_stepAIC_rmlse
                  )
                  

names(rmlse_scores) <- c("OLS_Full", "OLS_Manual", "OLS_Forward",
                         "OLS_Backward", "Ridge", "LASSO",
                         "Elastic","Tree_CP"
                         # OLS_StepAIC,
                         # OLS_Step
                         )

# Compile all the best CV scores

best_lm_ols <- lm_ols$results[as.numeric(rownames(lm_ols$bestTune)),]
best_lm_cats <- lm_cats$results[as.numeric(rownames(lm_cats$bestTune)),]
best_lm_forward <- lm_forward$results[as.numeric(rownames(lm_forward$bestTune)),]
best_lm_backward <- lm_backward$results[as.numeric(rownames(lm_backward$bestTune)),]
#lm_step$results[as.numeric(rownames(lm_step$bestTune)),]
best_lm_ridge <- lm_ridge$results[as.numeric(rownames(lm_ridge$bestTune)),]
best_lm_lasso <- lm_lasso$results[as.numeric(rownames(lm_lasso$bestTune)),]
best_lm_elastic <- lm_elas$results[as.numeric(rownames(lm_elas$bestTune)),]
best_tree_cp <- tree_cp$results[as.numeric(rownames(tree_cp$bestTune)),]

cv_results <- data.frame(method = names(rmlse_scores), 
                         rmse = c(best_lm_ols['RMSE'][1,1],
                                  best_lm_cats['RMSE'][1,1],
                                  best_lm_forward['RMSE'][1,1],
                                  best_lm_backward['RMSE'][1,1],
                                  best_lm_ridge['RMSE'][1,1],
                                  best_lm_lasso['RMSE'][1,1],
                                  best_lm_elastic['RMSE'][1,1],
                                  best_tree_cp['RMSE'][1,1]),
                         rmse_sd = c(best_lm_ols['RMSESD'][1,1],
                                      best_lm_cats['RMSESD'][1,1],
                                      best_lm_forward['RMSESD'][1,1],
                                      best_lm_backward['RMSESD'][1,1],
                                      best_lm_ridge['RMSESD'][1,1],
                                      best_lm_lasso['RMSESD'][1,1],
                                      best_lm_elastic['RMSESD'][1,1],
                                      best_tree_cp['RMSESD'][1,1]))


ggplot(cv_results, aes(x=method, y=rmse)) + 
         geom_dotplot(binaxis = 'y', stackdir = 'center') +
         geom_errorbar(aes(ymin=rmse-rmse_sd, ymax=rmse+rmse_sd), width=.2,
                                  position=position_dodge(.0)) +
         xlab("Method") +
         ylab("Cross-Validation RMSE")


# Explore the different final models from CV

plot(ols_fit$residuals)
plot(cats_fit$residuals)


# Residual analysis

summary(lm_ols)
summary(lm_ridge)
