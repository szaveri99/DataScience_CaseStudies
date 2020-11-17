library(randomForest)
library(caret)
library(gmodels)

data(iris)
View(iris)

iris_setosa <- iris[iris$Species == "setosa",]
iris_versicolor <- iris[iris$Species == "versicolor",]
iris_virginica <- iris[iris$Species == "virginica",]

iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

fit.forest <- randomForest(Species~.,data=iris,na.action = na.roughfix ,importance = TRUE)
pred.train <- predict(fit.forest , iris_train)

mean(pred.train == iris_train$Species)
confusionMatrix(iris_train$Species,pred.train)

pred.test <- predict(fit.forest , newdata = iris_test)
mean(pred.test == iris_test$Species)
confusionMatrix(iris_test$Species,pred.test)

plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

fraud <- read.csv(file.choose())
View(fraud)

sum(is.na(fraud))
tax_class <- ifelse(fraud$Taxable.Income <=30000 , "Low","High")

data <- data.frame(fraud[,-3],tax_class)
View(data)

split <- createDataPartition(data$tax_class , p=0.7 , list = FALSE)
f.train <- data[split,]
f.test <- data[-split,]

fit.for <- randomForest(data$tax_class ~ . , data=data , na.action = na.roughfix , importance = TRUE)
pred.f.trn <- predict(fit.for , f.train)
mean(pred.f.trn == f.train$tax_class)
confusionMatrix(pred.f.trn , f.train$tax_class)

pred.f.test <- predict(fit.for , newdata = f.test)
mean(pred.f.test == f.test$tax_class)
confusionMatrix(pred.f.test , f.test$tax_class , dnn = c('predict','actual'))

plot(fit.for ,lwd =2)
legend("topright", colnames(fit.for$err.rate),col=1:3,cex=0.8,fill=1:3)

company <- read.csv(file.choose())
View(company)
str(company)
sum(is.na(company))

sales_class <- ifelse(company$Sales <= 9 , "Risky","Good")

c.data <- data.frame(company[,-1],sales_class)

c.split <- createDataPartition(c.data$sales_class , p=0.7 , list = FALSE)
c.train <- c.data[c.split,]
c.test <- c.data[-c.split,]

c.forest <- randomForest(c.data$sales_class~. , data = c.data ,importance = TRUE)
pred.c.train <- predict(c.forest , c.train)
mean(pred.c.train == c.train$sales_class)
confusionMatrix(pred.c.train , c.train$sales_class)

pred.c.test <- predict(c.forest , newdata = c.test)
mean(pred.c.test == c.test$sales_class)
confusionMatrix(pred.c.test , c.test$sales_class)

plot(c.forest ,lwd=2)
legend("topright",colnames(c.forest$err.rate) , col =1:3 ,cex=0.8 , fill = 1:3)
