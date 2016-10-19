#Set File path
setwd("C:\\personal\\Big_Data\\SpringBoard\\Sample Experiments done\\logistic_regression")
#Reading data from file
hsdata <- read.csv("framingham.csv")
#displaying the struture of heart saftey data.
str(hsdata)
#load the caTools package
library(caTools)
#setting seed value to 100. Randomly selected.
set.seed(1000)
#creating sample data for training and testing
split <- sample.split(hsdata$TenYearCHD,SplitRatio = 0.65)
#Creating subset of training dataset from sample data splitted ealier.
train <- subset(hsdata, split==TRUE)
#Creating subset of testing dataset from sample data splitted ealier.
test <- subset(hsdata,split==FALSE)
# Using generalized linear model to predict TenYearCHD. All columns are selected 
# as input. 
hsLog <- glm(train$TenYearCHD~., data = train, family = binomial)
#Display summary for generalized linear model created for TenYearCHD prediction
summary(hsLog)
#using predict function on the glm model created earlier and new test dataset.
predictTest <- predict(hsLog , type = "response" , newdata = test)
#printing contingency table for test data which we splitted earlier
table(test$TenYearCHD,predictTest >0.5)
#loading ROCR package
library(ROCR)
#prediction object created for test model.
ROCR_predict_test <- prediction(predictTest,test$TenYearCHD)
#Display summary for prediction model for TenYearCHD.
summary(ROCR_predict_test)
#Evaluting the performance of the prediction model on test data.
as.numeric(performance(ROCR_predict_test,"auc")@y.values)
