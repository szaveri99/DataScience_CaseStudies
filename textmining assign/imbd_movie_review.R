library(rvest)
library(magrittr)
library(tm)
library(slam)
library(wordcloud)
library(syuzhet)
library(ggplot2)

imbd_url <- "https://www.imdb.com/title/tt1872181/reviews?ref_=tt_ql_"
spiderman <- NULL


for(i in 1:10){
  murl <- read_html(as.character(paste(imbd_url,i,sep="")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  spiderman <- c(spiderman,rev)
}

write.table(file = "movie.txt",spiderman , row.names = F)
imbd <- read.delim("movie.txt")

View(imbd)
corpus <- imbd[-1,]
head(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus , removeWords, stopwords("english"))
#corpus <- tm_map(corpus,PlainTextDocument)
inspect(corpus[1:5])

tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

w <- sort(rowSums(tdm) , decreasing = TRUE)
v <- data.frame(word = names(w) , freq = w)
head(v)

set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = T,
          min.freq =  2, 
          scale = c(5,0.3),colors = w,
          rot.per = 0.6)

barplot(v[1:5,]$freq, las = 2, names.arg = v[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")


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

quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(senti_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)

