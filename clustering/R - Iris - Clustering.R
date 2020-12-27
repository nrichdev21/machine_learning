library(ISLR)
library(ggplot2)
library(cluster)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(size=5, alpha=0.5)

ggplot(iris, aes(Petal.Length,Petal.Width, color=Species)) +
  geom_point(size=5, alpha=0.5)

iris.cluster <- kmeans(iris[,1:4], center=3, nstart = 20)
print(iris.cluster)

# Compare to actual labels
table(iris.cluster$cluster, iris$Species)

# Plot with 2 principal components
clusplot(iris, iris.cluster$cluster, color=T, shade=T, labels=0, lines=0)
