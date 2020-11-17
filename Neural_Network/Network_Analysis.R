install.packages("igraph")
library(igraph)

karate <- read.graph("http://cneurocvs.rmki.kfki.hu./igraph/karate.net",format = "pajek")
plot(karate)
plot(karate , layout = layout.circle)
plot(karate , layout = layout.grid)

g1 <- graph(c(1,2, 1,3 , 2,3 ,3,5),n=5,directed = F)
plot(g1)

V(g1)
E(g1)

mat <- matrix(rep(c(1,0),10) , 5 , 5)
gh <- graph.adjacency(mat , mode = "undirected", weighted = TRUE)
plot(gh)
