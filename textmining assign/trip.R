library(rvest)
library(magrittr)
library(tm)
library(slam)
library(wordcloud)
library(ggplot2)
library(syuzhet)

url1 <- "https://www.tripadvisor.in/Attraction_Review-g297668-d311631-Reviews-or"
url2 <- "-Balsamand_Lake_and_Garden-Jodhpur_Jodhpur_District_Rajasthan.html#REVIEWS"

trip_reviews <- NULL

for(i in 0:8){
 turl <- read_html(as.character(paste(url1 , i*10 ,url2,sep="")))
 rev <- turl %>%
   html_nodes(".cPQsENeY .IRsGHoPm span") %>%
   html_text()
 trip_reviews <- c(trip_reviews,rev)
}

View(trip_reviews)

write.table(trip_reviews , file = "trip.txt" , row.names = F)
trip_data <- readLines("trip.txt")
View(trip_data)
corpus <- trip_data
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus , tolower)
corpus <- tm_map(corpus , removePunctuation)
corpus <- tm_map(corpus , removeNumbers)
#corpus <- tm_map(corpus , removeWords , stopwords("english"),c("can","this","and","you","is",
#"are","or","if","the","then","where","what"))
stop <- readLines(file.choose())
corpus <- tm_map(corpus , removeWords , stop)
corpus <- tm_map(corpus , stripWhitespace)
inspect(corpus[1:5])

tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)
tdm[1:20,1:10]

w <- sort(rowSums(tdm) , decreasing = TRUE)
v <- data.frame(word = names(w) , freq = w)

wordcloud(words = names(w) , freq = w,
          max.words = 500 , min.freq = 2,
          scale = c(6,0.5),rot.per = 0.7,
          colors = w)

tt <- readLines(file.choose())

s_vec <- get_sentiment(tt,method = "syuzhet")
senti_data <- get_nrc_sentiment(tt)
head(senti_data,10)

td <- data.frame(t(senti_data))
td_new <- data.frame(rowSums(td[2:10]))

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)

rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
View(td_new)
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

barplot(
   sort(colSums(prop.table(senti_data[, 1:8]))), 
   horiz = TRUE, 
   cex.names = 0.5, 
   las = 1, 
   main = "Emotions in Text", xlab="Percentage"
)

View(sort(colSums(prop.table(senti_data[, 1:8]))))

