library(devtools)
library(glmnetUtils)
library(reshape2)
library(ggplot2)
library(rpart)
library(xgboost)
library(randomForest)

rmlse <- function(yhat, y) {
  n <- length(yhat)
  return(sqrt((1/n)*sum((log(yhat)-log(y))^2)))
}

train_fp <- "/Users/Grant/gdfs/My Drive/F2018/MATH564/18FMath564-pj/data/"
test_fp <- "/Users/Grant/gdfs/My Drive/F2018/MATH564/18FMath564-pj/data/"

train_fn <- "train_processed.csv"
test_fn <- "test_processed.csv"

train <- read.csv(paste(train_fp,train_fn, sep=""))[,c(-1,-2)]
test <- read.csv(paste(test_fp, test_fn, sep=""))[,c(-1, -2)]
train_num <- train[,sapply(train, is.numeric)]

train_matrix <- model.matrix(SalePrice~.,train)
y <- train$SalePrice

# OLS full model
ols_fit <- lm(SalePrice~., train)
ols_coef <- coef(ols_fit)
ols_preds <- predict(ols_fit, train)
ols_rmlse <- rmlse(abs(ols_preds), train$SalePrice)


# Ridge regression
lambdas <- 10^seq(10, -2, length = 100)
ridge_fit <- cv.glmnet(train_matrix, y, alpha=0, lambda=lambdas)
ridge_preds <- predict(ridge_fit, s=ridge_fit$lambda.min, newx=train_matrix)
ridge_rmlse <- rmlse(abs(ridge_preds), y)

# Lasso regression
lasso_fit <- cv.glmnet(train_matrix, y, alpha=0, lambda=lambdas)
lasso_preds <- predict(lasso_fit, s=lasso_fit$lambda.min, newx=train_matrix)
lasso_rmlse <- rmlse(lasso_preds, y)

plot(ridge_fit)
plot(lasso_fit)

# Tree regression - not very good
tree_fit <- rpart(SalePrice~., data=train, method="anova")
tree_fit <- prune(tree_fit, cp=0.01)
tree_preds <- predict(tree_fit, newx=train)
tree_rmlse <- rmlse(tree_preds, y)

summary(tree_fit)
plotcp(tree_fit)
plot(tree_fit)
text(tree_fit)
printcp(tree_fit)

# Try a random forest for regression
rf_fit <- randomForest(SalePrice~., data=train)
rf_preds <- predict(rf_fit, newx=train)