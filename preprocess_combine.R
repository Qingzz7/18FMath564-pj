train_fp <- "~/Documents/GitHub/18FMath564-pj/data/"
test_fp <- "~/Documents/GitHub/18FMath564-pj/data/"

train_fn <- "train.csv"
test_fn <- "test.csv"

# The original raw dataset
train_origdata <- read.csv(paste(train_fp,train_fn, sep=""))
test_origdata <- read.csv(paste(test_fp, test_fn, sep=""))

# The dataset to process and perform operations on
train_procdata <- train_origdata
test_procdata <- test_origdata

# Dataframes to store outliers or other strange observations
#  train_outliers <- train_origdata[FALSE,]
# test_outliers <- test_origdata[FALSE,]


# Turn MSSubClass into a factor
train_procdata$MSSubClass <- as.factor(train_origdata$MSSubClass)
test_procdata$MSSubClass <- as.factor(test_origdata$MSSubClass)

# There are some MSZoning NA's in test, and label them as RL
test_procdata$MSZoning[is.na(test_origdata$MSZoning)] <- as.factor("RL")

# Presume the NAs in LotFrontage mean no street on the property
train_procdata$LotFrontage[is.na(train_procdata$LotFrontage)] <- 0 
test_procdata$LotFrontage[is.na(test_procdata$LotFrontage)] <- 0 

# Alley NA's means no alley - replace them with some other factor
levels(train_procdata$Alley)==levels(test_procdata$Alley) # check the level in train and test
levels(train_procdata$Alley) <- c('Grvl', 'Pave', 'NoAlley')
train_procdata$Alley[is.na(train_procdata$Alley)] <- as.factor('NoAlley')
levels(test_procdata$Alley) <- c('Grvl', 'Pave', 'NoAlley')
test_procdata$Alley[is.na(test_procdata$Alley)] <- as.factor('NoAlley')

# Utilities are the same for all but 1, just drop it
train_utilities_outliers <- train_procdata[train_procdata$Utilities != 'AllPub',]
test_utilities_outliers <- test_procdata[test_procdata$Utilities != 'AllPub',]
train_procdata <- within(train_procdata, rm(Utilities))
test_procdata <- within(test_procdata, rm(Utilities))

# Create a factor if a house has been remodeled
train_procdata$BeenRemod <- as.factor(train_origdata$YearRemodAdd - train_origdata$YearBuilt > 0)
# train_procdata$YrsSncRemod <- max(train_origdata$YearRemodAdd) - train_origdata$YearRemodAdd
test_procdata$BeenRemod <- as.factor(test_origdata$YearRemodAdd - test_origdata$YearBuilt > 0)
# test_procdata$YrsSncRemod <- max(test_origdata$YearRemodAdd) - test_origdata$YearRemodAdd

# A function to generate an interval label to find nonlinearities between 'eras'
era <- function(year) {
  if (year < 1900) {
    return('1800s')
  }
  if (year >= 1900 && year <= 1909) {
    return('1900s')
  }
  if (year >= 1910 && year <= 1919) {
    return('1910s')
  }
  if (year >= 1920 && year <= 1929) {
    return('1920s')
  }
  if (year >= 1930 && year <= 1939) {
    return('1930s')
  }
  if (year >= 1940 && year <= 1949) {
    return('1940s')
  }
  if (year >= 1950 && year <= 1959) {
    return('1950s')
  }
  if (year >= 1960 && year <= 1969) {
    return('1960s')
  }
  if (year >= 1970 && year <= 1979) {
    return('1970s')
  }
  if (year >= 1980 && year <= 1989) {
    return('1980s')
  }
  if (year >= 1990 && year <= 1999) {
    return('1990s')
  }
  return('2000s')
}
train_procdata$EraBuilt <- as.factor(sapply(train_origdata$YearBuilt, era))
test_procdata$EraBuilt <- as.factor(sapply(test_origdata$YearBuilt, era))
# Then remove Yearbuilt
train_procdata$YearBuilt=NULL
test_procdata$YearBuilt=NULL

# RoofMatl is mostly CompShg
train_roof_outliers <- train_origdata[train_origdata$RoofMatl != 'CompShg',]
test_roof_outliers <-  test_origdata[test_origdata$RoofMatl != 'CompShg',]
train_procdata <- within(train_procdata, rm(RoofMatl))
test_procdata <- within(test_procdata, rm(RoofMatl))

# MasVnr & MasVnrArea NA for 8 values and they don't mean none - going to presume None anyway since Area and Type are all NA 
train_mas_outliers <- train_origdata[is.na(train_origdata$MasVnrArea),]
test_mas_outliers <- test_origdata[is.na(test_origdata$MasVnrArea),]

train_procdata$MasVnrArea[is.na(train_procdata$MasVnrArea)] <- 0
test_procdata$MasVnrArea[is.na(test_procdata$MasVnrArea)] <- 0
train_procdata$MasVnrType[is.na(train_procdata$MasVnrType)] <- as.factor("None")
test_procdata$MasVnrType[is.na(test_procdata$MasVnrType)] <- as.factor("None")


# ExterCond & ExterQual should be switched to integers
ratingtoint <- function(f) {
  if (is.na(f)) { # NAs typically mean no rating, so just give these a 'typical' response
    return (3)
  }
  if (f == "Ex") {
    return (5)
  }
  if (f == "Gd") {
    return (4)
  }
  if (f == "TA") {
    return (3)
  }
  if (f == "Fa") {
    return (2)
  }
  return (1)
}

train_procdata$ExterQualNum <- sapply(train_origdata$ExterQual, ratingtoint)
train_procdata <- within(train_procdata, rm(ExterQual))
test_procdata$ExterQualNum <- sapply(test_origdata$ExterQual, ratingtoint)
test_procdata <- within(test_procdata, rm(ExterQual))

train_procdata$ExterCondNum <- sapply(train_origdata$ExterCond, ratingtoint)
train_procdata <- within(train_procdata, rm(ExterCond))
test_procdata$ExterCondNum <- sapply(test_origdata$ExterCond, ratingtoint)
test_procdata <- within(test_procdata, rm(ExterCond))

# Basement variables - quite a few no basements that need to be filled in
levels(train_procdata$BsmtCond) <- c(levels(train_procdata$BsmtCond), "No")
levels(train_procdata$BsmtQual) <- c(levels(train_procdata$BsmtQual), "No")
levels(train_procdata$BsmtFinType1) <- c(levels(train_procdata$BsmtFinType1), "No")
levels(train_procdata$BsmtFinType2) <- c(levels(train_procdata$BsmtFinType2), "No")
levels(train_procdata$BsmtExposure) <- c(levels(train_procdata$BsmtExposure), "No")

train_procdata$BsmtCond[is.na(train_procdata$BsmtCond)] <- as.factor("No")
train_procdata$BsmtQual[is.na(train_procdata$BsmtQual)] <- as.factor("No")
train_procdata$BsmtFinType1[is.na(train_procdata$BsmtFinType1)] <- as.factor("No")
train_procdata$BsmtFinType2[is.na(train_procdata$BsmtFinType2)] <- as.factor("No")
train_procdata$BsmtExposure[is.na(train_procdata$BsmtExposure)] <- as.factor("No")

levels(test_procdata$BsmtCond) <- c(levels(test_procdata$BsmtCond), "No")
levels(test_procdata$BsmtQual) <- c(levels(test_procdata$BsmtQual), "No")
levels(test_procdata$BsmtFinType1) <- c(levels(test_procdata$BsmtFinType1), "No")
levels(test_procdata$BsmtFinType2) <- c(levels(test_procdata$BsmtFinType2), "No")
levels(test_procdata$BsmtExposure) <- c(levels(test_procdata$BsmtExposure), "No")

test_procdata$BsmtCond[is.na(test_procdata$BsmtCond)] <- as.factor("No")
test_procdata$BsmtQual[is.na(test_procdata$BsmtQual)] <- as.factor("No")
test_procdata$BsmtFinType1[is.na(test_procdata$BsmtFinType1)] <- as.factor("No")
test_procdata$BsmtFinType2[is.na(test_procdata$BsmtFinType2)] <- as.factor("No")
test_procdata$BsmtExposure[is.na(test_procdata$BsmtExposure)] <- as.factor("No")

# Track the heating outliers and drop it
train_heating_outliers <- train_origdata[train_origdata$Heating != "GasA",]
test_heating_outliers <- test_origdata[test_origdata$Heating != "GasA",]
train_procdata <- within(train_procdata, rm(Heating))
test_procdata <- within(test_procdata, rm(Heating))

# Order the heating quality ratings
train_procdata$HeatingQCNum <- sapply(train_origdata$HeatingQC, ratingtoint)
train_procdata <- within(train_procdata, rm(HeatingQC))
test_procdata$HeatingQCNum <- sapply(test_origdata$HeatingQC, ratingtoint)
test_procdata <- within(test_procdata, rm(HeatingQC))

# The 1 Electrical NA looks normal, so impute from the most common class
train_procdata$Electrical[is.na(train_procdata$Electrical)] <- as.factor("SBrkr")

# Numericize kitchen quality
train_procdata$KitchenQualNum <- sapply(train_origdata$KitchenQual, ratingtoint)
test_procdata$KitchenQualNum <- sapply(test_origdata$KitchenQual, ratingtoint)
train_procdata <- within(train_procdata, rm(KitchenQual))
test_procdata <- within(test_procdata, rm(KitchenQual))

# Functional has a couple NAs in the test set - these seem to be pretty rough houses from other traits, so going to give them minor dings
test_procdata$Functional[is.na(test_procdata$Functional)] <- as.factor("Min2")

# Fireplace needs to have a different factor other than NA
levels(train_procdata$FireplaceQu) <- c(levels(train_origdata$FireplaceQu), "No")
train_procdata$FireplaceQu[is.na(train_procdata$FireplaceQu)] <- as.factor("No")
levels(test_procdata$FireplaceQu) <- c(levels(test_origdata$FireplaceQu), "No")
test_procdata$FireplaceQu[is.na(test_procdata$FireplaceQu)] <- as.factor("No")

# Garage NA's need to be replaced with NoGarage
levels(train_procdata$GarageType) <- c(levels(train_procdata$GarageType), "NoGarage")
train_procdata$GarageType[is.na(train_procdata$GarageType)] <- as.factor("NoGarage")
levels(test_procdata$GarageType) <- c(levels(test_procdata$GarageType), "NoGarage")
test_procdata$GarageType[is.na(test_procdata$GarageType)] <- as.factor("NoGarage")

levels(train_procdata$GarageFinish) <- c(levels(train_procdata$GarageFinish), "NoGarage")
train_procdata$GarageFinish[is.na(train_procdata$GarageFinish)] <- as.factor("NoGarage")
levels(test_procdata$GarageFinish) <- c(levels(test_procdata$GarageFinish), "NoGarage")
test_procdata$GarageFinish[is.na(test_procdata$GarageFinish)] <- as.factor("NoGarage")

levels(train_procdata$GarageQual) <- c(levels(train_procdata$GarageQual), "NoGarage")
train_procdata$GarageQual[is.na(train_procdata$GarageQual)] <- as.factor("NoGarage")
levels(test_procdata$GarageQual) <- c(levels(test_procdata$GarageQual), "NoGarage")
test_procdata$GarageQual[is.na(test_procdata$GarageQual)] <- as.factor("NoGarage")

levels(train_procdata$GarageCond) <- c(levels(train_procdata$GarageCond), "NoGarage")
train_procdata$GarageCond[is.na(train_procdata$GarageCond)] <- as.factor("NoGarage")
levels(test_procdata$GarageCond) <- c(levels(test_procdata$GarageCond), "NoGarage")
test_procdata$GarageCond[is.na(test_procdata$GarageCond)] <- as.factor("NoGarage")

# Should maybe just drop year built since there's no logical value to impute for a place with no garage
train_procdata <- within(train_procdata, rm(GarageYrBlt))
test_procdata <- within(test_procdata, rm(GarageYrBlt))

# Only a few samples have pools -> just make a binary factor
train_procdata$NoPool <- as.factor(train_procdata$PoolArea > 0)
test_procdata$NoPool <- as.factor(test_procdata$PoolArea > 0)

levels(train_procdata$PoolQC) <- c(levels(train_procdata$PoolQC), "No")
train_procdata$PoolQC[is.na(train_procdata$PoolQC)] <- as.factor("No")
levels(test_procdata$PoolQC) <- c(levels(test_procdata$PoolQC), "No")
test_procdata$PoolQC[is.na(test_procdata$PoolQC)] <- as.factor("No")

# Fence is generally NA
levels(train_procdata$Fence) <- c(levels(train_procdata$Fence), "None")
train_procdata$Fence[is.na(train_procdata$Fence)] <- as.factor("None")
levels(test_procdata$Fence) <- c(levels(test_procdata$Fence), "None")
test_procdata$Fence[is.na(test_procdata$Fence)] <- as.factor("None")

# MiscFeature needs to have NA's replaced
levels(train_procdata$MiscFeature) <- c(levels(train_procdata$MiscFeature), "None")
train_procdata$MiscFeature[is.na(train_procdata$MiscFeature)] <- as.factor("None")
levels(test_procdata$MiscFeature) <- c(levels(test_procdata$MiscFeature), "None")
test_procdata$MiscFeature[is.na(test_procdata$MiscFeature)] <- as.factor("None")


# write.csv(train_procdata, file=paste(train_fp,"train_processed.csv", sep=""))
# write.csv(test_procdata, file=paste(test_fp,"test_processed.csv", sep=""))


# get a summary of the data after preprocessing
summary(train_procdata)
summary(test_procdata)
dim(train_procdata)
str(train_procdata)

# check varaible type
catv <- names(train_procdata[,sapply(train_procdata,is.factor)]) # categorical
catv
numv <- names(train_procdata[,sapply(train_procdata,is.numeric)]) # numeric 
numv
logv <- names(train_procdata[,sapply(train_procdata,is.logical)]) # binary
logv

library(corrplot)
cor <- cor(train_procdata[numv]) #get correaltion matrix for numeric variable
corrplot.mixed(cor) # really messay figure, need to explore more


library(ggplot2)
library(reshape)
melttrain <- melt(train_procdata)
melttest <- melt(test_procdata)

#boxplot for training data
ptrain <- ggplot(melttrain, aes(factor(variable), value)) 
ptrain + geom_boxplot() + facet_wrap(~variable, scale="free")

#density plot for train data
ggplot(data = melttrain, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
#density plot for test data
ggplot(data = melttest, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")


l=lm(SalePrice~., data=train_procdata)
plot(l) # check linear regression assumption.  There are outliers and residual assumption does not stand

car::vif(l) # Variance inflation factors.
# Error in vif.default(l) : there are aliased coefficients in the model
# The error indicates multicolinearity
alias(l)


