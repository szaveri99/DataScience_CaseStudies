## read the data for books!!

books <- read.csv("D:\\DataScience\\Association_Rules\\book.csv")
View(books)

##load the libraries

library(arules)
library(arulesViz)

## applying the apriori algo
book_rules <- apriori(as.matrix(books) , parameter = list(supp = 0.02 , conf = 0.5 , minlen = 4))

head(quality(sort(book_rules,by="lift")))

## inspecting the data by lift
inspect(tail(sort(book_rules , by="lift")))

## plotting the graph with the help of lift to see support and confidence for each item.
plot(book_rules , jitter = 0 ,by="lift")

## for better visualization and understanding
plot(book_rules , method = "grouped")
plot(book_rules , method = "scatterplot")
plot(book_rules , method = "graph")


## read the data for movies!!

movies <- read.csv("D:\\DataScience\\Association_Rules\\my_movies.csv",header = TRUE)
View(movies)

movies <- movies[,6:15]

mov_rules <- apriori(as.matrix(movies) , parameter = list(supp = 0.05 , conf = 0.7 , minlen = 3))

head(quality(sort(mov_rules,by="lift")))

inspect(head(sort(mov_rules , by="lift")))

## gives the better detailing of the graph.
plotly_arules(mov_rules,jitter=0)

plot(mov_rules , jitter = 0 ,by="lift")
plot(mov_rules , method = "grouped")
plot(mov_rules , method = "scatterplot")
plot(mov_rules , method = "graph")


## read the data for groceries !!

grocery <- read.transactions("D:\\DataScience\\Association_Rules\\groceries.csv",format = "basket",
                             sep = ",")
inspect(grocery[1:5])
grocery@itemInfo
itemFrequencyPlot(x=grocery,topN=10)

gro_rules <- apriori(grocery , parameter = list(supp = 0.005 , conf = 0.5 , minlen = 2))
plot(gro_rules , method = "grouped matrix")

plotly_arules(gro_rules,jitter=0)
