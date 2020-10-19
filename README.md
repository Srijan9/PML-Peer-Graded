# PML-Peer-Graded
---
title: "Practical ML Peer Graded Project"
output:
  html_document: default
  pdf_document: default
---

# By Srijan Prem Srivastava
### Précis
##### In this assignment, the goal is to use data available to us by different equipments from 6 different users.

### Datasets:-
##### TThe data for testing are loaded from: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
##### The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.


### Assignment

# Extracting the Data and cleaning
### Load library
```{r, message=FALSE, warning=FALSE}
library(caret)
library(rpart)
library(rpart.plot)
library(knitr)
```
### Getting the required Data
```{r echo=TRUE}
training_data <- read.csv("pml-training.csv")
testing_data <- read.csv("pml-testing.csv")
inTrain <- createDataPartition(training_data$classe, p=0.6, list=FALSE)
myTraining <- training_data[inTrain, ]
myTesting <- training_data[-inTrain, ]
```
### Cleaning Data
```{r echo=TRUE}
nzv <- nearZeroVar(mTrain)
mTrain <- mTrain[, -nzv]
mTest <- mTest[, -nzv]
mostNA <- sapply(mTrain, function(x) mean(is.na(x))) > 0.95
mTrain <- mTrain[, mostNA==F]
mTest <- mTest[, mostlyNA==F]
mTrain <- myTrain[, -(1:5)]
mTest  <- myTest[, -(1:5)]
```
# Prediction data by model
### 1. Random forest
```{r echo=TRUE}
moFit <- randForest(classe ~ ., data=myTraining)
moFit
predict <- predict(moFit, myTest, type="class")
confusionMatrix(myTesting$classe, predict)
```
### i) Random forest
```{r echo=TRUE}
moFit_T <- rpart(classe~., myTrain)
# Predict Decision tree
predict_T <- predict(moFit_T, myTest, type="class")
confusionMatrix(myTest$classe, predict_T)
```
### 3. Generalized Boosted Model (GBM)
```{r, message=FALSE, warning=FALSE}
control_GBM <- trainControl(method = "repeatedv", number=5, repeats=1)
modFit_GBM <- train(classe~., myTraining, method="gbm", trControl=control_GBM, verbose=FALSE)
```
```{r echo=TRUE}
# Predict GBM
predict_GBM <- predict(moFit_GBM, myTest)
confusionMatrix(predict_GBM, myTest$classe)
```

# finding error by cross validation
#### Random forest, GBM models and Dicision tree give 99.610 %, 98.80 % and 75.43 % as efficiency.
#### The sample errors obtained for Random forest, Dicision tree, and GBM are 0.4 %, 24.6 %, and 1.2 %, respectively.

# test output final
#### The algorithm is runned of 20 test cases and most accurate model is Random Forest
```{r echo=TRUE}
predict_test <- predict(moFit, test, type = "class")
predict_test
```
