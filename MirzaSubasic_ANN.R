setwd("C:/Users/mirza/Desktop/Fakultet/Introduction to machine learning/Assignments")

library(neuralnet)

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
train <- myData[1:216,]
test <- myData[217:270,]


#########################   hidden = 2  ##############################
model_ANN <- neuralnet(class ~ age + sex + chestPain + pressure + cholestoral. + 
                        sugar + electrocardiographic. + hrate + angina. + oldPeak +
                         slopePeak + vessels. + thal, data = train, hidden = 2)
plot(model_ANN)

#making predictions
ann.results <- compute(model_ANN, test)

#outputs predicted and actual values
results <- data.frame(actual = test$class, prediction = ann.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


#########################   hidden = 3  ##############################
model_ANN <- neuralnet(class ~ age + sex + chestPain + pressure + cholestoral. + 
                         sugar + electrocardiographic. + hrate + angina. + oldPeak +
                         slopePeak + vessels. + thal, data = train, hidden = 3)
plot(model_ANN)

#making predictions
ann.results <- compute(model_ANN, test)

#outputs predicted and actual values
results <- data.frame(actual = test$class, prediction = ann.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


#########################   hidden = 4  ##############################
model_ANN <- neuralnet(class ~ age + sex + chestPain + pressure + cholestoral. + 
                         sugar + electrocardiographic. + hrate + angina. + oldPeak +
                         slopePeak + vessels. + thal, data = train, hidden = 4)
plot(model_ANN)

#making predictions
ann.results <- compute(model_ANN, test)

#outputs predicted and actual values
results <- data.frame(actual = test$class, prediction = ann.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

#########################   hidden = 5  ##############################
model_ANN <- neuralnet(class ~ age + sex + chestPain + pressure + cholestoral. + 
                         sugar + electrocardiographic. + hrate + angina. + oldPeak +
                         slopePeak + vessels. + thal, data = train, hidden = 5)
plot(model_ANN)

#making predictions
ann.results <- compute(model_ANN, test)

#outputs predicted and actual values
results <- data.frame(actual = test$class, prediction = ann.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

#########################   hidden = 6  ##############################
model_ANN <- neuralnet(class ~ age + sex + chestPain + pressure + cholestoral. + 
                         sugar + electrocardiographic. + hrate + angina. + oldPeak +
                         slopePeak + vessels. + thal, data = train, hidden = 6)
plot(model_ANN)

#making predictions
ann.results <- compute(model_ANN, test)

#outputs predicted and actual values
results <- data.frame(actual = test$class, prediction = ann.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

#########################   hidden = 7  ##############################
model_ANN <- neuralnet(class ~ age + sex + chestPain + pressure + cholestoral. + 
                         sugar + electrocardiographic. + hrate + angina. + oldPeak +
                         slopePeak + vessels. + thal, data = train, hidden = 7)
plot(model_ANN)

#making predictions
ann.results <- compute(model_ANN, test)

#outputs predicted and actual values
results <- data.frame(actual = test$class, prediction = ann.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

