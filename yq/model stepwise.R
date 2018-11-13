# project model trail 
# 181109 yuqing zhao

df= read.csv('~/Documents/GitHub/18FMath564-pj/data/train_processed.csv')
tdf= read.csv('~/Documents/GitHub/18FMath564-pj/data/test_processed.csv')
#remove first two columns which are index
df=df[,-c(1,2)]
tdf=tdf[,-c(1,2)]

# regressions without cross-validation
# linear regression
lm=lm(SalePrice~., data = df)
summary(lm)

library(MASS)
# variable selection by AIC stepwise algorithm
fit0=glm(df$SalePrice~1, data=df)
fit1=glm(df$SalePrice~., data=df)
lm_forward=step(fit0,scope = list(lower=fit0,upper=fit1),direction = 'forward',data=df)
formula(lm_forward)
lm_backward=step(fit1,scope = list(lower=fit0,upper=fit1),direction = 'backward',data=df)
formula(lm_backward) # has different result from forward and stepwise results
lm_step=step(fit0,scope = list(lower=fit0,upper=fit1),direction = 'both',data=df)
summary(lm_step)
formula(lm_step)


# tree regression via rpart package
# grow the tree
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

# bagging tree ?




# perform cross-validtion on training data set
controlParameter=trainControl(method = "cv",number = 10,savePredictions = TRUE)
set.seed(571)

lm_model<-train(modelForm,data=train,method='lm',trControl=controlParameter)
summary(lm_model)
