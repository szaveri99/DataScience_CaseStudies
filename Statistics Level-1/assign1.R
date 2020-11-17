#Q7
data<-read.csv('D:\DataScience\Assignment-1\cars.csv')
data

#MEAN

mean(data$Points,na.rm = T)
mean(data$Score,na.rm = T)
mean(data$Weigh,na.rm = T)

#MODE

table(data$Points)
t[which.max(table(data$Points))]

table(data$Score)
t[which.max(table(data$Score))]

t<-table(data$Weigh)
t[which.max(table(data$Weigh))]

#MEDIAN
median(data$Points)
median(data$Score)
median(data$Weigh)

#VARIANCE
var(data$Points,na.rm = T)
var(data$Score,na.rm = T)
var(data$Weigh,na.rm = T)

#SD
sd(data$Points,na.rm = T)
sd(data$Score,na.rm = T)
sd(data$Weigh,na.rm = T)

Range<- function(x)
{
  max(x,na.rm = T)-min(x,na.rm = T)
}

Range(data$Points)
Range(data$Score)
Range(data$Weigh)

#Q9_a
data<-read.csv(file.choose())
data

install.packages("moments")
library(moments)

skewness(data$speed)
kurtosis(data$speed)

#from above we can conclude that the skewness of speed defines the negatively 
#skewed as the result of the skewness is negative
#and the result of kurtosis is platykurtic


skewness(data$dist)
kurtosis(data$dist)

#from above we can conclude that the skewness of dist. defines the positively 
#skewed as the result of the skewness is positive
#and the result of kurtosis is leptokurtic

#Q9_b

data
dim(data)
nrow(data)

install.packages("moments")
library(moments)

skewness(data$SP)
kurtosis(data$SP)

#from above we can conclude that the skewness of SP defines the positively 
#skewed as the result of the skewness is positive
#and the result of kurtosis is leptokurtic

skewness(data$WT)
kurtosis(data$WT)

#from above we can conclude that the skewness of dist. defines the negatively 
#skewed as the result of the skewness is negative
#and the result of kurtosis is leptokurtic

#Q11

error<-function(x){
  n<-2000
  s<-30
  error<- qnorm(x)*s/sqrt(n)
  error
} 

a<-200

#for 94%
left<- a-error(0.97)
left
right<- a+error(0.97)
right

#for 98%
left<- a-error(0.99)
left
right<- a+error(0.99)
right

#for 96%
left<- a-error(0.98)
left
right<- a+error(0.98)
right

#Q12

marks<-c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
mean(marks)
median(marks)
var(marks)
sd(marks)

boxplot(marks)


