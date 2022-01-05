setwd("C:/Users/mirza/Desktop/Assignments")

library(class)
library(gmodels)
library(caret)

#importing data
myData <- read.csv("heart.csv", stringsAsFactors = FALSE)
head(myData)

hist(myData$class)
hist(myData$age)
hist(myData$sugar)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize all data data
#class 0 for absence and 1 for presence
myData <- as.data.frame(lapply(myData[1:14], normalize))
head(myData)

#split into training and testing
#270*0.8=216;    216 persons for test and rest for testing
train1 <- myData[1:216,]
test1 <- myData[217:270,]

#set class as labels
#Absence (0) or presence (1) 
train_labels <- as.factor(train1$class)
test_labels <- as.factor(test1$class)

#sqrt(216)=14.69694; take 15 because it is odd number
#training
model1 <- knn(train = train1, test = test1, cl = train_labels, k = 15)
#evaluating performance
CrossTable(x=test_labels, y=model1, prop.chisq = FALSE)



set.seed(123)
#create folds
folds <- createFolds(myData$class, k = 10)
folds

#9 folds for training and 1 for testing
train2 <- myData[-folds$Fold10, ]
test2 <- myData[folds$Fold10, ]

model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 15)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)

model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 13)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)

model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 11)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)

model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 9)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)

model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 7)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)

model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 5)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)


model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 3)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)


model2 <- knn(train = train2, test = test2, cl = as.factor(train2$class), k = 1)
#evaluating performance
CrossTable(x=as.factor(test2$class), y=model2, prop.chisq = FALSE)
sensitivity(as.factor(model2), as.factor(test2$class), positive = 1)
specificity(as.factor(model2), as.factor(test2$class), negative = 0)



