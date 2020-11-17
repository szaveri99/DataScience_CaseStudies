library(tm)
library(e1071)
library(gmodels)
library(caret)

sal_data_tst <- read.csv("D:\\DataScience\\NaiveBayes Assignment\\SalaryData_Test.csv")
sal_data_trn <- read.csv("D:\\DataScience\\NaiveBayes Assignment\\SalaryData_Train.csv")

str(sal_data_trn)
str(sal_data_tst)

dim(sal_data_tst)
dim(sal_data_trn)

plot(sal_data_trn$sex , sal_data_trn$Salary )
plot(sal_data_trn$workclass , sal_data_trn$Salary)
plot(sal_data_trn$education , sal_data_trn$Salary)
plot(sal_data_trn$maritalstatus , sal_data_trn$Salary)

barplot(table(sal_data_trn$Salary),col = rainbow(2), xlab = "salary" , ylab = "total amount")


CrossTable(pred , sal_data_tst$Salary ,
           prop.chisq = FALSE , prop.r = FALSE , prop.t = FALSE,
           dnn = c('predicted' , 'actual'))

classifier <- naiveBayes(sal_data_trn$Salary ~ . ,data = sal_data_trn)
classifier
pred <- predict(classifier , sal_data_tst)

mean(pred == sal_data_tst$Salary)
confusionMatrix(evaluate , sal_data_tst$Salary)

