library(plyr)
library(caret)
library(corrplot)

fire <- read.csv("D:\\DataScience\\Neural_Network\\forestfires.csv")

View(fire)
str(fire)
summary(fire)

fire$month <- as.numeric(factor(fire$month))
fire$day <- as.numeric(factor(fire$day))
fire$size_category <- as.numeric(factor(fire$size_category))


forestfire <- as.data.frame(fire)

normalize <- function(x){
  return ((x-min(x)) / (max(x) - min(x)))
} 

forest <- as.data.frame(lapply(forestfire, normalize))

set.seed(111)

train <- forest[1:300,] 
test <- forest[301:517,]

library(neuralnet)

?neuralnet

forest_model <- neuralnet(formula = area ~. ,data = train)
plot(forest_model)

result_model <- list()
result_model <- compute(forest_model , test)
str(result_model)

predict.area <- result_model$net.result
cor(predict.area , test$area) 

## using the hidden layers to increase the accuracy
set.seed(1234)
forest_model2 <- neuralnet(formula = area ~. ,data = train,hidden = 5)
plot(forest_model2)

result_model2 <- list()
result_model2 <- compute(forest_model2 , test)
str(result_model2)

predict.area2 <- result_model2$net.result
cor(predict.area2 , test$area) 

result_model2
