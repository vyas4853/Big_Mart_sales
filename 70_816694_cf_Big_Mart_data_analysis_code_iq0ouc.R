library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)





setwd("G:\\R Language\\R project\\Big mart sales data analysis")

data <- read.csv("train_data.csv")
data1<- read.csv("test_data.csv")
str(data)
summary(data)
View(data1)
View(data)
############################################cleaning outliers data#####################################################
## cleaning the outlier data 
boxplot(data$Item_Outlet_Sales)
quantile(data$Item_Outlet_Sales, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.96,0.97,0.974,0.975,0.98,0.99,0.995,1))

data<- data[data$Item_Outlet_Sales<6250, ]
boxplot(data$Item_Outlet_Sales)
quantile(data$Item_Outlet_Sales, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.96,0.97,0.975,0.98,0.99,0.995,1))

summary(data)
str(data)

##########################################cleaning training data #################################################################
## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))



## There is no missing value in data$Item_Identifier
###To get Most Frequently used data 
###tail(names(sort(table(data$Item_Identifier))), 1)
##Taking care of missing data(Item_Identifier)
##data$Item_Identifier<- ifelse(is.na(data$Item_Identifier),tail(names(sort(table(data$Item_Identifier))), 1), data$Item_Identifier)


###Taking care of missing data(Item_Weight)
data$Item_Weight<- ifelse(is.na(data$Item_Weight),ave(data$Item_Weight, FUN= function(x) mean(x, na.rm = TRUE)), data$Item_Weight)

###Taking care of unstructured data(Item_Fat_Content )
unique(data$Item_Fat_Content)
table(data$Item_Fat_Content)
library(stringr)
data$Item_Fat_Content<- str_replace_all(data$Item_Fat_Content, "LF", "Low Fat")
data$Item_Fat_Content<- str_replace_all(data$Item_Fat_Content, "low fat", "Low Fat")
data$Item_Fat_Content<- str_replace_all(data$Item_Fat_Content, "reg", "Regular")
table(data$Item_Fat_Content)
data$Item_Fat_Content<- ifelse(is.na(data$Item_Fat_Content),"Low Fat", data$Item_Fat_Content)
table(data$Item_Fat_Content)

## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))



#############################################################################################
##cleaning data$otlet_size
table(data$Outlet_Size)
data$Outlet_Size[data$Outlet_Size=='']<- NA
data$Outlet_Size<- ifelse(is.na(data$Outlet_Size),3, data$Outlet_Size)
table(data$Outlet_Size)
unique(data$Outlet_Size)
##############################################################################################

data$Item_Visibility<- ifelse(data$Item_Visibility==0,ave(data$Item_Visibility, FUN= function(x) mean(x, na.rm = TRUE)), data$Item_Visibility)
table(data$Item_Visibility)
unique(data$Item_Visibility)
View(data)


data$Outlet_Year=2013-data$Outlet_Establishment_Year
View(data$Outlet_Year)
data$Outlet_Year<-as.factor(data$Outlet_Year)
View(data)

summary(data)

###column in consideration 2,3,4,5,6,9,10,11,12,13
## cleaning of the missing value has completed.

##converting data type of Item_Fat_content
data$Item_Fat_Content<- as.factor(data$Item_Fat_Content)
str(data)
train_data<-data
#####################################################cleaning test data##########################################################
# Check the missing value (if any)
sapply(data1, function(x) sum(is.na(x)))



## There is no missing value in data$Item_Identifier
###To get Most Frequently used data 
###tail(names(sort(table(data$Item_Identifier))), 1)
##Taking care of missing data(Item_Identifier)
##data$Item_Identifier<- ifelse(is.na(data$Item_Identifier),tail(names(sort(table(data$Item_Identifier))), 1), data$Item_Identifier)


###Taking care of missing data(Item_Weight)
data1$Item_Weight<- ifelse(is.na(data1$Item_Weight),ave(data1$Item_Weight, FUN= function(x) mean(x, na.rm = TRUE)), data1$Item_Weight)
sapply(data1, function(x) sum(is.na(x)))

###Taking care of unstructured data(Item_Fat_Content )
unique(data1$Item_Fat_Content)
table(data1$Item_Fat_Content)
library(stringr)
data1$Item_Fat_Content<- str_replace_all(data1$Item_Fat_Content, "LF", "Low Fat")
data1$Item_Fat_Content<- str_replace_all(data1$Item_Fat_Content, "low fat", "Low Fat")
data1$Item_Fat_Content<- str_replace_all(data1$Item_Fat_Content, "reg", "Regular")
table(data1$Item_Fat_Content)
data1$Item_Fat_Content<- as.factor(data1$Item_Fat_Content)

## Check the missing value (if any)
sapply(data1, function(x) sum(is.na(x)))
#############################################################################################
##cleaning data$otlet_size
table(data1$Outlet_Size)
data1$Outlet_Size[data1$Outlet_Size=='']<- NA
data1$Outlet_Size<- ifelse(is.na(data1$Outlet_Size),3, data1$Outlet_Size)
table(data1$Outlet_Size)
unique(data1$Outlet_Size)
################################################################################################################


data1$Item_Visibility<- ifelse(data1$Item_Visibility==0,ave(data1$Item_Visibility, FUN= function(x) mean(x, na.rm = TRUE)), data1$Item_Visibility)
table(data1$Item_Visibility)
unique(data1$Item_Visibility)
View(data1)


data1$Outlet_Year=2013-data1$Outlet_Establishment_Year
View(data1$Outlet_Year)
data1$Outlet_Year<-as.factor(data1$Outlet_Year)
View(data1)

summary(data1)
View(data1)
test_data<-data1
############################################################################################################################################
###craeating linear regression model
###fit<-lm(Item_Outlet_Sales ~ I(Item_Identifier=="DRF36")+I(Item_Identifier=="DRF60")+I(Item_Identifier=="DRG13")+I(Item_Identifier=="DRI51")+
###       I(Item_Identifier=="DRK37")+I(Item_Identifier=="DRM35")+I(Item_Identifier=="FDA15")+
###       Item_Fat_Content+I(Item_Type=="Dairy")+I(Item_Type=="Seafood")+I(Item_Type=="Soft Drinks")+
###       Item_MRP+I(Outlet_Identifier=="OUT013")+I(Outlet_Identifier=="OUT017")+I(Outlet_Identifier=="OUT018")+I(Outlet_Identifier=="OUT027")+I(Outlet_Identifier=="OUT035")+
###       I(Outlet_Identifier=="OUT045")+I(Outlet_Identifier=="OUT046")+I(Outlet_Identifier=="OUT049"),data=train_data)
###summary(fit)
##adjusted R-square: 0.5584 , it is a moderate model.

## multicollinearity Test
##H0- There should be no perfect linear relationship between predictors(independent variable) when value less than 2
###vif(fit)

## MAPE test (mean absolute percentage error) - How different the prediction is different from actual(Ranges0-1)
## Lesser the MAPE better the Prediction
###View(test_data)
###pred<- predict(fit , newdata= train_data)
###(sum((abs(train_data$Item_Outlet_Sales-pred))/train_data$Item_Outlet_Sales))/nrow(train_data)
## MAPE value is 0.9692112 , so this is worst prediction
###sqrt( mean(( train_data$Item_Outlet_Sales-pred)^2)) 
############################RMSE is 981.39##################
###ncol(train_data)
###View(train_data)
##################################### randomforest  ############################################
names(train_data)
library(randomForest)
set.seed(123)
regressor<- randomForest( x= train_data[c(2:6, 9, 10, 11, 13)], y=train_data$Item_Outlet_Sales, ntree=100)
pred1<- predict(regressor, newdata= train_data)
(sum((abs(train_data$Item_Outlet_Sales-pred1))/train_data$Item_Outlet_Sales))/nrow(train_data)
sqrt( mean(( train_data$Item_Outlet_Sales-pred1)^2)) 
######################################RMSE is 603 ####################################################
test_data$Item_Outlet_Sales_pred<- predict(regressor, newdata= test_data)
View(test_data)
write.csv(test_data,'G:\\R Language\\R project\\Big mart sales data analysis\\mdata1.csv')

################## datatype of both train and test dataset should be same otherwise it will give error train and test datast are not same############
##sapply(train_data, class)
##sapply(test_data, class)
##View(train_data)

