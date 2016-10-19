#Set File path
setwd("C:\\personal\\Big_Data\\SpringBoard\\Sample Experiments done\\logistic_regression")
#Reading data from file
hcdata <- read.csv("quality.csv")
#print top records from the data
head(hcdata)
#printing contingency table for column PoorCare
table(hcdata$PoorCare)
#displaying the struture of health care data i.e. hcdata
str(hcdata)
# install package caTools if it is not already present
install.packages("caTools")
#load the caTools package
library(caTools)
#setting seed value to 88. Randomly selected.
set.seed(88)
#creating sample data for training and testing
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
#printing contingency table for sample data which we splitted earlier
table(split)
#Creating subset of training dataset from sample data splitted ealier.
qualityTrain <- subset(quality,split ==TRUE)
#Creating subset of testing dataset from sample data splitted ealier.
qualityTest <- subset(quality, split==FALSE)
#diplaying the number of rows in training data
nrow(qualityTrain)
#diplaying the number of rows in testing data
nrow(qualityTest)
# Using generalized linear model to predict PooCare. OfficeVisits and Narcotics
# column used as input. 
hcLog <- glm(PoorCare~ OfficeVisits +Narcotics, data=qualityTrain,
                  family = binomial)
#Displaying summary for generalized linear model created for PoorCare prediction
summary(hcLog)
#using predict function on the glm model created earlier
predictTrain <- predict(hcLog, type = "response")
#Displaying summary from predictTrain data
summary(predictTrain)
#Calculating mean value for PoorCare column from the sample test data
tapply(predictTrain, qualityTrain$PoorCare, mean)
#Creating confusion matrix with threshold value of 0.5 from PoorCare column
confusion_matrix_1 <- as.matrix(table(qualityTrain$PoorCare, predictTrain > 0.5))
#Calculating specificity for predict model with threshold 0.5
Specificity_matrix_1 <- (confusion_matrix_1[1]) /(confusion_matrix_1[1]+
                                                    confusion_matrix_1[3])
#Specificity for predict model with threshold 0.5
Specificity_matrix_1
#Calculating sensitivity for predict model with threshold 0.5
Sensitivity_matrix_1 <- (confusion_matrix_1[4]) /(confusion_matrix_1[4]+
                                                    confusion_matrix_1[2])
#Sensitivity for predict model with threshold 0.5
Sensitivity_matrix_1
#Accuracy for the traing data with threshold 0.5
accur_test_matrix_1 <- (confusion_matrix_1[1]+confusion_matrix_1[4]) /
  (confusion_matrix_1[1]+confusion_matrix_1[2]+confusion_matrix_1[3]+
     confusion_matrix_1[4])
#Accuracy for training data with threshold 0.5
accur_test_matrix_1
#Creating confusion matrix with threshold value of 0.7 from PoorCare column
confusion_matrix_2 <- as.matrix(table(qualityTrain$PoorCare, predictTrain > 0.7))
#Calculating specificity for predict model with threshold 0.7
Specificity_matrix_2 <- (confusion_matrix_2[1]) /(confusion_matrix_2[1]+
                                                    confusion_matrix_2[3])
#Specificity for predict model with threshold 0.7
Specificity_matrix_2

#Calculating sensitivity for predict model with threshold 0.7
Sensitivity_matrix_2 <- (confusion_matrix_2[4]) /(confusion_matrix_2[4]+
                                                    confusion_matrix_2[2])
#Sensitivity for predict model with threshold 0.7
Sensitivity_matrix_2
#Accuracy for the traing data with threshold 0.7
accur_test_matrix_2 <- (confusion_matrix_2[1]+confusion_matrix_2[4]) /
  (confusion_matrix_2[1]+confusion_matrix_2[2]+confusion_matrix_2[3]+
     confusion_matrix_2[4])
#Accuracy for training data with threshold 0.5
accur_test_matrix_2
#Creating confusion matrix with threshold value of 0.2 from PoorCare column
confusion_matrix_3 <- as.matrix(table(qualityTrain$PoorCare, predictTrain > 0.2))
#Calculating specificity for predict model with threshold 0.2
Specificity_matrix_3 <- (confusion_matrix_3[1]) /(confusion_matrix_3[1]+
                                                    confusion_matrix_3[3])
#Specificity for predict model with threshold 0.2
Specificity_matrix_3
#Calculating sensitivity for predict model with threshold 0.2
Sensitivity_matrix_3 <- (confusion_matrix_3[4]) /(confusion_matrix_3[4]+
                                                    confusion_matrix_3[2])
#Sensitivity for predict model with threshold 0.2
Sensitivity_matrix_3
#Accuracy for the traing data with threshold 0.2
accur_test_matrix_3 <- (confusion_matrix_3[1]+confusion_matrix_3[4]) /
  (confusion_matrix_3[1]+confusion_matrix_3[2]+confusion_matrix_3[3]+
     confusion_matrix_3[4])
#Accuracy for training data with threshold 0.2
accur_test_matrix_3

#install package ROCR if it is not already install
install.packages("ROCR")
#loading ROCR package
library(ROCR)
#prediction object created for training model.
ROCR_prediction <- prediction(predictTrain,qualityTrain$PoorCare)
#Evaluating predict train model using preformance function.
ROCR_performance <- performance(ROCR_prediction,"tpr", "fpr")
#plotting the predit train model after evaluated using performance function.
plot(ROCR_performance)
#Colors to threshold values added
plot(ROCR_performance,colorize=TRUE)
#added text in the plot along with color for the training model.
plot(ROCR_performance,colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),
     text.adj=c(-0.2, 1.7))
#predicting the test dataset using model trained earlier
predict_Test <- predict(QualityLog,type = "response", newdata = qualityTest)
#Displaying summary for predict result of testing dataset.
summary(predict_Test)
#Ploting predict result for testing dataset
plot(predict_Test)
# Creating prediction object for result obtained for testing data prediction.
ROCR_predict_test <- prediction(predict_Test,qualityTest$PoorCare)
#Evaluating performance of the model on testing dataset
ROCR_performance_test <- performance(ROCR_predict_test,"tpr", "fpr")
#Plotting the result of prediction for testing dataset.
plot(ROCR_performance_test)
#ploting the result of prediction on testing dataset with color 
plot(ROCR_performance,colorize=TRUE)
#ploting the result of prediction on testing dataset with color and text
plot(ROCR_performance,colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), 
     text.adj=c(-0.2, 1.7))
# Confusion matrix for test data
con_matrix <- as.matrix(table(qualityTest$PoorCare, predict_Test > 0.3))
# Specificity for test data
speci_test <- (con_matrix[1]) /(con_matrix[1]+con_matrix[3])
#Sensitivity for test data
sensi_test <- (con_matrix[4]) /(con_matrix[4]+con_matrix[2])
# Sensitity for test data is
sensi_test
#Accuracy for the test data using predict model
accur_test <- (con_matrix[1]+con_matrix[4]) /
  (con_matrix[1]+con_matrix[2]+con_matrix[3]+
     con_matrix[4])
#Accuracy for test data is
accur_test
#Overall error rate for test data 
error_rate <- (con_matrix[2]+con_matrix[3]) /
  (con_matrix[1]+con_matrix[2]+con_matrix[3]+
     con_matrix[4])
#Overall error rate for test dataset
error_rate

