
library(datasets)
library(GGally)
library(magrittr)
library(e1071)
library(caret)

set.seed(65414)
iris
ggpairs(iris)
i2 <- iris[,-5]
i2

iris.pca <- preProcess(i2, 
           method=c("BoxCox", "center", 
                    "scale", "pca"))
i3 <- cbind(predict(iris.pca,i2),especie=iris$Species)
i3
head(i3)
ggplot(i3) + geom_point(aes(x=PC1,y=PC2,color=especie,alpha=.5))

#K-means
n.k <- 3
kmeans.i3 <- kmeans(i3[,-3],n.k,iter.max = 10)
ggplot() + geom_point(aes(x=i3$PC1,y=i3$PC2,color=factor(kmeans.i3$cluster),alpha=.5))
n.k <- 5
kmeans.i3 <- kmeans(i3[,-3],n.k,iter.max = 10)
ggplot() + geom_point(aes(x=i3$PC1,y=i3$PC2,color=factor(kmeans.i3$cluster),alpha=.5))
n.k <- 3
kmeans.i3 <- kmeans(i3[,-3],n.k,iter.max = 100)
ggplot() + geom_point(aes(x=i3$PC1,y=i3$PC2,color=factor(kmeans.i3$cluster),alpha=.5))

#C-means
n.c <- 3
cmeans.i3 <- cmeans (i3[-3], n.c, iter.max=10, verbose=FALSE, dist="euclidean")
ggplot() + geom_point(aes(x=i3$PC1,y=i3$PC2,color=factor(cmeans.i3$cluster),alpha=.5))

n.c <- 3
cmeans.i3 <- cmeans (i3[-3], n.c, iter.max=100, verbose=FALSE, dist="euclidean",m = 5)
ggplot() + geom_point(aes(x=i3$PC1,y=i3$PC2,color=factor(cmeans.i3$cluster),alpha=.5))

compara <- data.frame(especie=iris[,5],km=kmeans.i3$cluster,cm=cmeans.i3$cluster)
head(compara)
table(compara[,c('especie','km')])
table(compara[,c('especie','cm')])





