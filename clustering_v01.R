
library(datasets)
library(GGally)
library(magrittr)
library(e1071)
library(caret)

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

kmeans.i3 <- kmeans(i3[,-3],3,iter.max = 10)

ggplot() + geom_point(aes(x=i3$PC1,y=i3$PC2,color=kmeans.i3$cluster,alpha=.5))
