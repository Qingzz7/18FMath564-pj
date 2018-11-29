
train_fp <- "~/Documents/GitHub/18FMath564-pj/data/"
test_fp <- "~/Documents/GitHub/18FMath564-pj/data/"

train_fn <- "train.csv"
test_fn <- "test.csv"

# The original raw dataset
train_origdata <- read.csv(paste(train_fp,train_fn, sep=""))
test_origdata <- read.csv(paste(test_fp, test_fn, sep=""))

#2.3 data pre-processing
#look at SalePrice
#histogram of saleprice
require("ggplot2")
ggplot(data=train_origdata, aes(train_origdata$SalePrice)) + 
  geom_histogram (col="red", aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red")+
  labs(title="Histogram for SalePrice") +
  labs(x="SalePrice", y="Count")+
  theme(plot.title = element_text(hjust = 0.5))

#density plot of saleprice
ggplot(data = train_origdata, aes(x = SalePrice)) + 
  stat_density(colour = "skyblue2", fill="skyblue3")+
  labs(title="Density plot for SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))

#boxplot for saleprice
par(mfrow=c(1,2))
boxplot(train_origdata$SalePrice, main="Boxplot for SalePrice")
boxplot(log(train_origdata$SalePrice), main="Boxplot for Log transformation SalePrice")
par(mfrow=c(1,1))
#a quickly visualize of whole dataset 
library(ggplot2)
library(tabplot)
library(plyr)
data <- rbind(train_origdata[,2:80],test_origdata[,2:80])
tableplot(data[,1:21])
tableplot(data[,22:41])
tableplot(data[,42:61])
tableplot(data[,62:79])

#look the data
str(data)
dim(data)
# The dataset to process and perform operations on
train_procdata <- train_origdata
test_procdata <- test_origdata

# Dataframes to store outliers or other strange observations
#  train_outliers <- train_origdata[FALSE,]
# test_outliers <- test_origdata[FALSE,]


# Turn MSSubClass into a factor
train_procdata$MSSubClass <- as.factor(train_origdata$MSSubClass)
test_procdata$MSSubClass <- as.factor(test_origdata$MSSubClass)

# There are some MSZoning NA's in test, and label them as C (all)***
test_procdata$MSZoning[is.na(test_origdata$MSZoning)] <- as.factor("C (all)")
train_procdata$MSZoning[is.na(test_origdata$MSZoning)] <- as.factor("C (all)")

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

#drop street
train_procdata <- within(train_procdata, rm(Street))
test_procdata <- within(test_procdata, rm(Street))

#drop Condition2
train_procdata <- within(train_procdata, rm(Condition2))
test_procdata <- within(test_procdata, rm(Condition2))

#convert overallquality in to factor variable
train_procdata$OverallQual <- as.factor(train_origdata$OverallQual)
test_procdata$MSSubClass <- as.factor(test_origdata$OverallQual)

#convert overallcondition in to factor variable
train_procdata$OverallCond <- as.factor(train_origdata$OverallCond)
test_procdata$OverallCond <- as.factor(test_origdata$OverallCond)

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

# RoofMatl is mostly CompShg, discard it
train_procdata <- within(train_procdata, rm(RoofMatl))
test_procdata <- within(test_procdata, rm(RoofMatl))

# MasVnr & MasVnrArea NA for 8 values and they don't mean none - going to presume None anyway since Area and Type are all NA 
train_mas_outliers <- train_origdata[is.na(train_origdata$MasVnrArea),]
test_mas_outliers <- test_origdata[is.na(test_origdata$MasVnrArea),]

train_procdata$MasVnrArea[is.na(train_procdata$MasVnrArea)] <- 0
test_procdata$MasVnrArea[is.na(test_procdata$MasVnrArea)] <- 0
train_procdata$MasVnrType[is.na(train_procdata$MasVnrType)] <- as.factor("None")
test_procdata$MasVnrType[is.na(test_procdata$MasVnrType)] <- as.factor("None")


# ExterCond & ExterQual should be switched to integers ***I think they should be factor variable
# ratingtoint <- function(f) {
#   if (is.na(f)) { # NAs typically mean no rating, so just give these a 'typical' response
#     return (3)
#   }
#   if (f == "Ex") {
#     return (5)
#   }
#   if (f == "Gd") {
#     return (4)
#   }
#   if (f == "TA") {
#     return (3)
#   }
#   if (f == "Fa") {
#     return (2)
#   }
#   return (1)
# }

# train_procdata$ExterQualNum <- sapply(train_origdata$ExterQual, ratingtoint)
# train_procdata <- within(train_procdata, rm(ExterQual))
# test_procdata$ExterQualNum <- sapply(test_origdata$ExterQual, ratingtoint)
# test_procdata <- within(test_procdata, rm(ExterQual))
# 
# train_procdata$ExterCondNum <- sapply(train_origdata$ExterCond, ratingtoint)
# train_procdata <- within(train_procdata, rm(ExterCond))
# test_procdata$ExterCondNum <- sapply(test_origdata$ExterCond, ratingtoint)
# test_procdata <- within(test_procdata, rm(ExterCond))

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

# Track the heating outliers and drop it ***why GasA is outlier, I think we need to discard it
# I think Grant means that heating system that is not GasA is outlier since most data is with GasA,we may drop this column
train_heating_outliers <- train_origdata[train_origdata$Heating != "GasA",]
test_heating_outliers <- test_origdata[test_origdata$Heating != "GasA",]
train_procdata <- within(train_procdata, rm(Heating))
test_procdata <- within(test_procdata, rm(Heating))

# Order the heating quality ratings **ratingtoint not found? and why order it?
# Grant writes his own function called ratingtonit which is not applicable now. 
# I think we dont need to change it to int since it is categorical variables not ordianry variables

# train_procdata$HeatingQCNum <- sapply(train_origdata$HeatingQC, ratingtoint)
# train_procdata <- within(train_procdata, rm(HeatingQC))
# test_procdata$HeatingQCNum <- sapply(test_origdata$HeatingQC, ratingtoint)
# test_procdata <- within(test_procdata, rm(HeatingQC))

# The 1 Electrical NA looks normal, so impute from the most common class ***I use FuseA
# SBrkr is the most common one, so I changed it to SBrkr
train_procdata$Electrical[is.na(train_procdata$Electrical)] <- as.factor("SBrkr")

# Numericize kitchen quality **why, I think it is ok as factor
# train_procdata$KitchenQualNum <- sapply(train_origdata$KitchenQual, ratingtoint)
# test_procdata$KitchenQualNum <- sapply(test_origdata$KitchenQual, ratingtoint)
# train_procdata <- within(train_procdata, rm(KitchenQual))
# test_procdata <- within(test_procdata, rm(KitchenQual))

# Functional has a couple NAs in the test set - these seem to be pretty rough houses from other traits, so going to give them minor dings
#***I use Typ
test_procdata$Functional[is.na(test_procdata$Functional)] <- as.factor("Typ")

#***use "TA" to replace NA in KitchenQual
# This one is hard to tell and there is no NA in this column
# train_procdata$KitchenQual[is.na(train_procdata$KitchenQual)] <- as.factor("TA")

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
#**here should be labeled as withPool? cause poolarea >0? 
# Grant did this, all poolarea>0 is labeled as TRUE
train_procdata$Pool <- as.factor(train_procdata$PoolArea > 0)
test_procdata$Pool <- as.factor(test_procdata$PoolArea > 0)
# drop PoolArea
train_procdata$PoolArea=NULL
test_procdata$PoolArea=NULL

# levels(train_procdata$PoolQC) <- c(levels(train_procdata$PoolQC), "No")
# train_procdata$PoolQC[is.na(train_procdata$PoolQC)] <- as.factor("No")
# levels(test_procdata$PoolQC) <- c(levels(test_procdata$PoolQC), "No")
# test_procdata$PoolQC[is.na(test_procdata$PoolQC)] <- as.factor("No")

#discard PoolQC
train_procdata <- within(train_procdata, rm(PoolQC))
test_procdata <- within(test_procdata, rm(PoolQC))

# Fence is generally NA
levels(train_procdata$Fence) <- c(levels(train_procdata$Fence), "None")
train_procdata$Fence[is.na(train_procdata$Fence)] <- as.factor("None")
levels(test_procdata$Fence) <- c(levels(test_procdata$Fence), "None")
test_procdata$Fence[is.na(test_procdata$Fence)] <- as.factor("None")

# MiscFeature needs to have NA's replaced
# levels(train_procdata$MiscFeature) <- c(levels(train_procdata$MiscFeature), "None")
# train_procdata$MiscFeature[is.na(train_procdata$MiscFeature)] <- as.factor("None")
# levels(test_procdata$MiscFeature) <- c(levels(test_procdata$MiscFeature), "None")
# test_procdata$MiscFeature[is.na(test_procdata$MiscFeature)] <- as.factor("None")

#***discard MiscFeature
train_procdata <- within(train_procdata, rm(MiscFeature))
test_procdata <- within(test_procdata, rm(MiscFeature))

# more preprocessing after refer to other kaggle tests
# yuqing 181119

# combine number of bathrooms
train_procdata$TotBathrooms = train_procdata$FullBath + (train_procdata$HalfBath*0.5) + train_procdata$BsmtFullBath + (train_procdata$BsmtHalfBath*0.5)
test_procdata$TotBathrooms = test_procdata$FullBath + (test_procdata$HalfBath*0.5) + test_procdata$BsmtFullBath + (test_procdata$BsmtHalfBath*0.5)
# Then remove other bathrooms variables
train_procdata[,c('FullBath','HalfBath','BsmtFullBath','BsmtHalfBath')]=NULL
test_procdata[,c('FullBath','HalfBath','BsmtFullBath','BsmtHalfBath')]=NULL

# combine porch realted varaibles
train_procdata$TotPorch = train_procdata$OpenPorchSF+train_procdata$EnclosedPorch+train_procdata$X3SsnPorch+train_procdata$ScreenPorch+train_procdata$WoodDeckSF
test_procdata$TotPorch = test_procdata$OpenPorchSF+test_procdata$EnclosedPorch+test_procdata$X3SsnPorch+test_procdata$ScreenPorch+test_procdata$WoodDeckSF
# Then remove other porch variables
train_procdata[,c('OpenPorchSF','EnclosedPorch','X3SsnPorch','ScreenPorch','WoodDeckSF')]=NULL
test_procdata[,c('OpenPorchSF','EnclosedPorch','X3SsnPorch','ScreenPorch','WoodDeckSF')]=NULL


# find highily related numeric variables to sale price and drop numeric variables have correlation lower than 
# check varaible type
catv <- names(train_procdata[,sapply(train_procdata,is.factor)]) # categorical
catv
numv <- names(train_procdata[,sapply(train_procdata,is.numeric)]) # numeric 
numv
logv <- names(train_procdata[,sapply(train_procdata,is.logical)]) # binary
logv
#sort on decreasing correlations with SalePrice
cor_train = cor(train_procdata[,numv]) #get correaltion matrix for numeric variable
cor_train_sort = as.data.frame(sort(cor_train[,'SalePrice'],decreasing = TRUE))
# seperate low and high corelations column names
highcor_name = names(which(apply(cor_train_sort, 1, function(x) abs(x)>0.35)))
lowcor_name = names(which(apply(cor_train_sort, 1, function(x) abs(x)<0.35)))
train_procdata[,lowcor_name]=NULL
test_procdata[,lowcor_name]=NULL

# remove variables that are in multicolinearity relation
library(corrplot)
cor_mcl = cor(train_procdata[,names(train_procdata[,sapply(train_procdata,is.numeric)])]) 
#corrplot::corrplot.mixed(cor_mcl)
#correlation plot
corrplot(cor_mcl,type="upper")
dropVars = c('ExterQualNum','GarageCars','X1stFlrSF', 'TotRmsAbvGrd') # select variables with correlation larger than
train_procdata = train_procdata[,!(names(train_procdata) %in% dropVars)]
test_procdata = test_procdata[,!(names(test_procdata) %in% dropVars)]

# use a small random forest to find important categorical variables
library(randomForest)
set.seed(564)
RF = randomForest(x=train_procdata, y=train_procdata$SalePrice, ntree=100 ,importance=TRUE)
RF_imp=as.data.frame( importance(RF) )
imp_v = data.frame(Variables = row.names(RF_imp), IncMSE = RF_imp[,1],IncNodePurity=RF_imp[,2])
imp_v = imp_v[order(imp_v$IncNodePurity, decreasing = TRUE),]
train_procdata$Electrical=NULL
test_procdata$Electrical=NULL

# dropVarsc = c('Electrical') # IncNodePurity with < E09
# dropVarsc = imp_v$Variables[imp_v$IncNodePurity<1.0e+10] # IncNodePurity with < E10
# train_procdata = train_procdata[,!dropVarsc %in% dropVarsc]

#check test data and train data
dim(train_procdata)
dim(test_procdata)


# linear regression for test
l=lm(SalePrice~.,train_procdata)
plot(l)

# check and remove ourliers ?????
train_procdata=train_procdata[-c(524,1183,1299),] # from linear regression plots

# check target distribution
qqnorm(train_procdata$SalePrice)
qqline(train_procdata$SalePrice)
# apply log transformation to the target
train_procdata$SalePrice=log(train_procdata$SalePrice)
# write the data after preprocessing to file
write.csv(train_procdata, file=paste(train_fp,"train_processed.csv", sep=""))
write.csv(test_procdata, file=paste(test_fp,"test_processed.csv", sep=""))




prodata <- rbind(train_procdata[,-46],test_procdata[,1:50]) #??? to plot
dim(prodata)
#boxplot
library(reshape)
meltprodata <- melt(prodata)
ggplot(meltprodata, aes(factor(variable), value)) + 
  geom_boxplot(fill = "white", colour = "#3366FF",outlier.colour = "red", outlier.shape = 1) + facet_wrap(~variable, scale="free")+
  labs(title = "Boxplots for numeric variables")+
  theme(plot.title = element_text(hjust = 0.5))
  
#density plot
ggplot(data = meltprodata, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")+labs(title = "Density plot for numeric variables")+
  theme(plot.title = element_text(hjust = 0.5))
#histgram for numeric variables 
ggplot(meltprodata,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)+
  labs(title = "Histgram for numeric variables")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-100, 3000))


# check variables skewness ?????
# numv <- names(train_procdata[,sapply(train_procdata,is.numeric)]) # numeric 
# numv
# for(i in numv) {
#  if (abs(skewness(train_procdata[,i]))>0.8){
#    train_procdata[,i] = log(train_procdata[,i]+1)
#  }}








