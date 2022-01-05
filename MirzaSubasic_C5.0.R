setwd("C:/Users/mirza/Desktop/Assignments")

library(gmodels)
library(C50)

#importing data
myData <- read.csv("heart.csv", stringsAsFactors = FALSE)
head(myData)
tail(myData)

hist(myData$class)
hist(myData$age)
hist(myData$sugar)

#split into training and testing
train1 <- myData[1:216,]
test1 <- myData[217:270,]


# build the decision tree
model1 <- C5.0(train1[-1], as.factor(train1$class))

# display detailed information including decisions about the tree
summary(model1)
plot(model1)

# making prediction
pred <- predict(model1, test1)

pred

# cross tabulation of predicted versus actual classes
CrossTable(test1$class, pred, prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))

sensitivity(as.factor(pred), as.factor(test1$class), positive = 2)

specificity(as.factor(pred), as.factor(test1$class), negative = 1)


