
wine <- read.csv("D:/DataScience/PCA/wine.csv",header = TRUE)
View(wine)

wine <- wine[,-1]
attach(wine)

?princomp()
cor(wine)

pcobj <- princomp(wine , cor = TRUE , scores = TRUE , covmat = NULL) 

str(pcobj)
summary(pcobj) ## how many components will be giving the information.
loadings(pcobj) ## this gives you weights

plot(pcobj) ## to see which components gives the max. value
biplot(pcobj)

pc <- scale(pcobj$scores[,1:3]) ## taking 1st three PC's for clustering analysis.

View(pc)

## the data is already standadrdize

## distance matrix
d <- dist(pc , method = "euclidean")

### heirarchial clustering ###

## creating the dendrogram
fit <- hclust(d , method = "complete")
plot(fit)
plot(fit , hang = -1)
rect.hclust(fit , k=3)

groups <- cutree(fit , k=3)
membership <- as.matrix(groups)
final <- data.frame(wine , membership)
View(final)
aggregate(final , by = list(final$membership), FUN = mean )

### k-means clustering ###

## elbow curve for deciding the k-value

wss = (nrow(pc)-1)*sum(apply(pc, 2,var))
for (i in 1:8) wss[i] = sum(kmeans(pc,centers = i)$withinss)
plot(1:8 ,wss, type = "b",xlab = "no. of clusters",ylab = "within group sums of squares",
     main = "k-means clustering scree plot")

## from the scree-plot we can select the k-value = 3


## the hubert and D index method also gives the optimal no. of clusters is k = 3
nb_clust <- NbClust(data = pc, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15,method = "kmeans", index="all")

fviz_nbclust(nb_clust)

km <- kmeans(pc , 3)
str(km)

##install.packages("factoextra")
##install.packages("NbClust")

library(NbClust)
library(factoextra)
fviz_cluster(km,data = wine)

final1 <- data.frame(wine,km$cluster)
View(final1)

aggregate(final1 , by = list(km$cluster),FUN = mean)

### Conclusion ###

##here in this data we can see that the no.of clusters are same as they were given before in the 
##data and heirarchial and kmeans values differ with a minimum values.
