library(tm)

sms.raw <- read.csv("D:\\DataScience\\NaiveBayes Assignment\\sms_raw_NB.csv")
View(sms.raw)

sms.raw$type <- factor(sms.raw$type)

str(sms.raw$type)
table(sms.raw$type)

sms_corpus <- Corpus(VectorSource(sms.raw$text))
clean_corpus <- tm_map(sms_corpus , tolower)
clean_corpus <- tm_map(clean_corpus , removeNumbers)
clean_corpus <- tm_map(clean_corpus , removePunctuation)
clean_corpus <- tm_map(clean_corpus , removeWords , stopwords("english"))
clean_corpus <- tm_map(clean_corpus , stripWhitespace)

inspect(clean_corpus[1:5])
#Sys.setlocale('LC_ALL','C')
sms_dtm <- DocumentTermMatrix(clean_corpus)

sms_dtm

set.seed(111)
sms_train <- sms.raw[1:4169,]
sms_test <- sms.raw[4270:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4270:5559,]

sms_corpus_train <- clean_corpus[1:4169]
sms_corpus_test <- clean_corpus[4270:5559]

prop.table(table(sms_train$type))
prop.table(table(sms_test$type))

sms_dict <- findFreqTerms(sms_dtm_train,5)

sms_trn <- DocumentTermMatrix(sms_corpus_train , list(dictionary = sms_dict))
sms_tst <- DocumentTermMatrix(sms_corpus_test , list(dictionary = sms_dict))

counts <- function(x){
  x <- ifelse(x > 0 ,1,0)
  x <- factor(x , levels = c(0,1) , labels = c("No","Yes"))
}

sms_trn <- apply(sms_trn, 2, counts)
sms_tst <- apply(sms_tst, 2, counts)

View(sms_trn)

#install.packages("gmodels")
library(e1071)

sms_classifier <- naiveBayes(sms_trn , sms_train$type)
sms_classifier

model_evaluation <- predict(sms_classifier , sms_tst)

library(gmodels)
CrossTable(model_evaluation , sms_test$type ,
           prop.chisq = FALSE , prop.r = FALSE , prop.t = FALSE,
           dnn = c('predicted' , 'actual'))

sms_classifier2 <- naiveBayes(sms_trn , sms_train$type ,laplace = 1)
model_evaluation <- predict(sms_classifier2 , sms_tst)
CrossTable(model_evaluation , sms_test$type ,
           prop.chisq = FALSE , prop.r = FALSE , prop.t = FALSE,
           dnn = c('predicted' , 'actual'))

library(wordcloud)
windows()
wordcloud(sms_corpus_train,min.freq = 10,max.words = 100,colors = ifelse(sms.raw$type=="spam","red","green"),
          random.order = F)

