
##Author: Naren Nandi
##Created: 6/8/2018

require(caTools)
#Reading Dataset 
wine = read.csv("wine.csv")

# set seed to ensure you always have same random numbers generated
set.seed(101)
# splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
sample = sample.split(wine,SplitRatio = 0.75) 

# creates a training dataset named train_wine and a test data set with test_wine 
train_wine =subset(wine,sample ==TRUE) 
test_wine=subset(wine, sample==FALSE)

#Checking the structure of the Data Set
str(train_wine)

#Checking the Range of Values for each variable in the Data Set
summary(train_wine)

#Creating a one-variable linear regression equation using AGST(Average Growing Season Temperature) 
#to predict Price.
model1 = lm(Price ~ AGST, data=train_wine)
summary(model1)

#Calculating Sum of Squared Errors(SSE)
model1$residuals
SSE = sum(model1$residuals^2)
SSE

#Using 2 Independent Variables in other terms performing Multi Linear Regression
model2 = lm(Price ~ AGST + HarvestRain, data=train_wine)
summary(model2)

#Computing the SSE for model2
SSE = sum(model2$residuals^2)
SSE

#Using more than 2 Independent Variables for Multi Linear Regression
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=train_wine)
summary(model3)

#Computing the SSE for model2
SSE = sum(model3$residuals^2)
SSE 

#Computing Correlation between Variables in the Data Set to find Multicollinearity
cor(train_wine)

#Since FrancePop and Age are Highly Correlated we are excluding FrancePop from our
#regression model. Logically looking at the situation, Population of a country has nothing to
#do with the Price of Wine but Age of the Wine does. Since the more old the wine is the more expensive
#it would be priced at.

#Rebuilding the Model without FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=train_wine)
summary(model4)

#Computing the SSE for model4
SSE = sum(model4$residuals^2)
SSE

#Checking the structure of the test Set
str(test_wine)

#Predicting values using the Test Data set
predictTest = predict(model4, newdata = test_wine)
predictTest

#Calculating R Squared by first finding out SSE and SST
SSE = sum((test_wine$Price - predictTest)^2)
SST = sum((test_wine$Price - mean(train_wine$Price))^2)
1 - (SSE/SST)

#We have 0.72 as our R-Squared value and we can conclude that it is a pretty good out-of-sample R-squared 
