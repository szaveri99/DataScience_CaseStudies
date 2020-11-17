library(rvest)
library(magrittr)
library(tm)
library(slam)
library(wordcloud)

aurl <- "https://www.amazon.in/gp/product/B07DJHXTLJ?pf_rd_p=649eac15-05ce-45c0-86ac-3e413b8ba3d4&pf_rd_r=K1972DRE0N8QAF5KYQMC/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pagenumber"
amazon_reviews <- NULL

for(i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep = "=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(file = "one.txt",amazon_reviews , row.names = F)
data <- read.delim("one.txt")

corpus <- data[-1,]
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
##corpus <- tm_map(corpus,PlainTextDocument)
inspect(corpus[1:5])

tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)

tdm[1:10,1:20]

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
v <- data.frame(word=names(w),freq = w)

head(v,5)

set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  2, 
          scale = c(5,0.3),
          rot.per = 0.6)

library(syuzhet)

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

library(ggplot2)
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(senti_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


