---
title: "Assignment - Prediction Assignment Writeup"
output: 
  html_document:
    keep_md: true 
---



## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

## Objective

* The goal of your project is to predict the manner in which they did the exercise. 
* This is the "class" variable in the training set. 
* You may use any of the other variables to predict with. 
* You should create a report describing how you built your model, how you used cross validation, what you think the      expected out of sample error is, and why you made the choices you did. 
* You will also use your prediction model to predict 20 different test cases.


## Load Data


```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
set.seed(1234)
```


```r
# Download data if it's not already done
if(!file.exists("pml-training.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                destfile = "pml-training.csv", method = "auto")
}

# Loading data
dsTrain <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))

# Dowload data if it's not already done
if(!file.exists("pml-testing.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                destfile = "pml-testing.csv", method = "auto")
}

# Loading data
dsTest<- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
```

## Clean Data


```r
# Drop columns that are not required in model.
dsTrainClean <- dsTrain[,8:length(colnames(dsTrain))]
dsTestClean <- dsTest[,8:length(colnames(dsTest))]

# Drop columns with that have mainly NA as values
dsTrainClean <- dsTrainClean[, colSums(is.na(dsTrainClean)) == 0] 
dsTestClean  <- dsTestClean[, colSums(is.na(dsTestClean)) == 0]

# Check for near zero variance predictors and drop them if necessary
nzv <- nearZeroVar(dsTrainClean,saveMetrics=TRUE)
zero.var.ind <- sum(nzv$nzv)

if ((zero.var.ind>0)) {
        dsTrainClean <- dsTrainClean[,nzv$nzv==FALSE]
}
```


## Final Data (Train, Validation, Test)


```r
inTrain <- createDataPartition(dsTrainClean$classe, p=0.70, list=F)
TrainData <- dsTrainClean[inTrain, ]
ValidateData <- dsTrainClean[-inTrain, ]
TestData  <- dsTestClean 
#ValidateData$classe
```

## RF Model


```r
rfModel <- train(classe ~ ., data=TrainData, method="rf", ntree=10)
rfModel
```

```
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9709448  0.9632349
##   27    0.9808605  0.9757911
##   52    0.9712119  0.9635870
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 27.
```

```r
predVal <- predict(rfModel, ValidateData)

confusionMatrix(predVal, as.factor(ValidateData$classe))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1672    4    0    0    0
##          B    1 1124   17    1    5
##          C    0    9 1007   11    1
##          D    1    1    2  950    2
##          E    0    1    0    2 1074
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9901          
##                  95% CI : (0.9873, 0.9925)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9875          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9988   0.9868   0.9815   0.9855   0.9926
## Specificity            0.9991   0.9949   0.9957   0.9988   0.9994
## Pos Pred Value         0.9976   0.9791   0.9796   0.9937   0.9972
## Neg Pred Value         0.9995   0.9968   0.9961   0.9972   0.9983
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2841   0.1910   0.1711   0.1614   0.1825
## Detection Prevalence   0.2848   0.1951   0.1747   0.1624   0.1830
## Balanced Accuracy      0.9989   0.9909   0.9886   0.9921   0.9960
```

```r
accuracy <- postResample(predVal, as.factor(ValidateData$classe))
accuracy
```

```
##  Accuracy     Kappa 
## 0.9901444 0.9875329
```
## Result

* Accuracy of this model is 0.9901
* Out-of-Sample error(using ValidateData) is 0.0099
* Higher accuracy is achievable with higher number of trees, at the expense of computational time.


## Classification of Test Set

```r
pred <- predict(rfModel, TestData)
pred
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
