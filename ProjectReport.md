---
title: "Practical Machine Learning Project"
output: html_document
---
## Introduction
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

## Loading Data
```{r,cache=TRUE,results='hide'}
library(caret)
library(rpart)
library(randomForest)
train_data <- read.csv("pml-training.csv");
test_data <- read.csv("pml-testing.csv");
```

## Cleaning Data
The data contains several variables with missing data, non numeric data, and data that is irrelevant to the model. Before applying any machine learning techniques it must be cleaned.
```{r,cache=TRUE}
train_data <- train_data[, colSums(is.na(train_data)) == 0] #Removes columns with missing data
remove <- grepl("window|timestamp|^X", names(train_data))
train_data <- train_data[, !remove] #remove X, user_name, anything with timestamp, anything with window
classe <- train_data$classe
train_data <- train_data[, sapply(train_data, is.numeric)] #remove columns with non numeric data
train_data$classe <- classe #Adds non numeric classe back in

test_data <- test_data[, colSums(is.na(test_data)) == 0] #Removes columns with missing data
remove <- grepl("window|timestamp|^X|problem", names(test_data))
test_data <- test_data[, !remove] #remove X, user_name, anything with timestamp or window, and problem_id
classe <- test_data$classe
test_data <- test_data[, sapply(test_data, is.numeric)] #remove columns with non numeric data
test_data$classe <- classe #Adds non numeric classe back in
```

## Data Slicing
We will partition the given training set into our real training set, and leave 30% of the data for conducting our own accuracy annalysis.
```{r,cache=TRUE}
set.seed(21422)
inTrain <- createDataPartition(train_data$classe, p=0.70, list=F)
trainData <- train_data[inTrain, ]
testData <- train_data[-inTrain, ]
```

## Applying Random Forest
The model will be built using the Random Forest method, and the control will use cross validation with 5 folds.
```{r,cache=TRUE}
modFit <- train(classe ~ ., data=trainData, method="rf", trControl=trainControl(method="cv", 5), ntree=250)
modFit
```
## Analysing Performance
```{r,cache=TRUE, results='hide'}
pred <- predict(modFit, testData)
confusionMatrix(testData$classe, pred)
```
```{r,cache=TRUE}
postResample(pred, testData$classe)
```
Our estimated model has 99.16% accuracy and 98.95% kappa. Hopefully our cross validation control will let the model have similiar accuracy on the out of sample data.  
  
## Final Test
Now we can apply our model to the test set.
```{r,cache=TRUE}
predict(modFit, test_data)
```
