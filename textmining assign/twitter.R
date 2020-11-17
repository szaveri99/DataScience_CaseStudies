#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("tm")

library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)

cred <- OAuthFactory$new(consumerKey = "bulfxeoWP4j3F7XDWlLINO7wz",
                         consumerSecret = "Galb5yfg1yG5R881ZyDwkPBbRGG00SQpCZXpAmM25zMHZYyTlT",
                         requestURL = "https://api.twitter.com/oauth/request_token",
                         accessURL = "https://api.twitter.com/oauth/access_token",
                         authURL = "https://api.twitter.com/oauth/authorize")

save(cred, file = "twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth("bulfxeoWP4j3F7XDWlLINO7wz",
                    "Galb5yfg1yG5R881ZyDwkPBbRGG00SQpCZXpAmM25zMHZYyTlT",
                    "1236596605123354624-9K487CFGLBQqx4dnlbBeVuq5V3O5LW",
                    "S4sbkFk4bt4BEm35IZT8Tg3oD8Q9RLWEAU5TMbBsnVfbA")
Tweets <- userTimeline("Cumberbitches",n=300)
TweetsDF <- twListToDF(Tweets)

write.csv(TweetsDF , "Tweets.csv")
getwd()

View(TweetsDF)

my_tweets <- read.csv(file.choose())
corpus <- my_tweets$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

## clean the text
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus , removeWords, stopwords("english"))
corpus <- tm_map(corpus,PlainTextDocument)

removeURL <- function(x) {gsub('http\\S*', "", x)}
corpus <- tm_map(corpus, content_transformer(removeURL))

tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)

tdm[1:10,1:20]

?wordcloud

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
v <- data.frame(word=names(w),freq = w)

head(v,5)

set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  2, 
          scale = c(5,0.3),
          rot.per = 0.6)

### Sentiment Analysis


install.packages("syuzhet")
library(syuzhet)

barplot(v[1:5,]$freq, las = 2, names.arg = v[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

text <- write.table(my_tweets ,"tweet.txt")
View(text)

tt <- readLines(file.choose())

s_vec <- get_sentiment(tt,method = "syuzhet")
senti_data <- get_nrc_sentiment(tt)
head(senti_data,10)

td <- data.frame(t(senti_data))
td_new <- data.frame(rowSums(td[2:253]))

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)

rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
View(td_new)

library(ggplot2)
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(senti_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
