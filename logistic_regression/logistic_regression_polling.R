#Set File path
setwd("C:\\personal\\Big_Data\\SpringBoard\\Sample Experiments done\\logistic_regression")
#Reading data from file
pollingdata <- read.csv("PollingData.csv")
#displaying the struture of polling data.
str(pollingdata)
#printing contingency table for column Year
table(pollingdata$Year)
#Displaying summary for polling data
summary(pollingdata)
#Installing mice package
install.packages("mice")
#load mice package
library("mice")
#Creating subset of polling data having columns Rasmussen, SurveyUSA, PropR
# and DiffCount.
simple <- pollingdata[c("Rasmussen", "SurveyUSA", "PropR" , "DiffCount")]
#Displaying summary for polling data with selected columns
summary(simple)
#setting seed value to 144. Randomly selected.
set.seed(144)
#Imputing the dataset by filling missing data.
imputed <- complete(mice(simple))
#Displaying the summary for the imputed data
summary(imputed)
#Replacing Rasmussen column data from polling with  imputed Rasmussen column 
pollingdata$Rasmussen <- imputed$Rasmussen
#Replacing SurveyUSA column data from polling with imputed SurveyUSA column
pollingdata$SurveyUSA <- imputed$SurveyUSA
#Displaying summary for polling data after imputing
summary(pollingdata)
#Creating the training dataset
Train <- subset(pollingdata,Year == 2004 | Year == 2008)
#Creating the testing dataset
Test <- subset(pollingdata, Year == 2012)
#printing contingency table for column Republican
table(Train$Republican)
#printing contingency table for column sign function of Rasmussen
table(sign(Train$Rasmussen))
#printing contingency table for column Republican and sign function of Rasmussen.
table(Train$Republican, sign(Train$Rasmussen))
#displaying the struture of training data.
str(Train)
# displaying the correlation between different columns of training dataset.
cor(Train[c("Rasmussen", "SurveyUSA", 
            "DiffCount" , "PropR" , "Republican")])
# Using generalized linear model to predict Republican. PropR column used 
# as input. 
mod1 <- glm(Republican~ PropR, data=Train, family = binomial())
#Displaying the summary for linear model created
summary(mod1)
#using predict function on the glm model created earlier
pred1 <-  predict(mod1,type="response")
#printing contingency table for column Republican and threshold greter than 0.5.
table(Train$Republican, pred1 >0.5)
# Using generalized linear model to predict Republican. SurveyUSA and Diffcount
# column used as input. 
mod2 <- glm(Republican~ SurveyUSA+DiffCount, data=Train, family = binomial)
#Displaying the summary for linear model created
summary(mod2)
#using predict function on the glm model created earlier
pred2 <-  predict(mod2,type="response")
#printing contingency table for column Republican and threshold greter than 0.5.
table(Train$Republican, pred2 >0.5)
#printing contingency table for column Republican and sign function of Rasmussen.
table(Test$Republican, sign(Test$Rasmussen))
#Using predict function on the glm model along with testing data.
TestPrediction <- predict(mod2, newdata = Test, type = "response")
#printing contingency table for column Republican and threshold greter than 0.5.
# Testing dataset is used.
table(Test$Republican,TestPrediction > 0.5)
# Subset of testing data have threshold greater than 0.5 and Republican  is zero.
subset(Test, TestPrediction >=0.5 & Republican == 0)
