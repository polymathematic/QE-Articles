#Clear environment
rm(list = ls())
gc()

#Set seed
set.seed(3)


#Load packages
require(mclust)
require(ggplot2)
require(BreweryDB)
require(dplyr)

#Load Example data
data(iris)

#Set number of centroids
g <- 3

#Load iris data, create example data
data(iris)

example2 <- data.frame(x = c(rnorm(300, 0, 10), rnorm(100, 5, 2), rnorm(100, -5, 2)), 
                 y = c(rnorm(300, 0, 10), rnorm(100, 0, 2), rnorm(100, 0, 2)),
                 class = c(rep("A",300), rep("B", 100), rep("C", 100)))

#Beer Data (30 is American IPA, 25 is American Pale, 31 is Imperial IPA)
beer <- list()
beer$AmericanIPA <- getBeer(n = 400, params = list(styleId = "30"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer$AmericanPale <- getBeer(n = 400, params = list(styleId = "25"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer$ImperialIPA <- getBeer(n = 400, params = list(styleId = "31"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer <- bind_rows(beer)

p0 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point() + ggtitle("The Iris Dataset")
png(filename = "iris.png")
p0
dev.off()



p1 <- ggplot(df, aes(x = x, y = y, color = class)) + geom_point() + ggtitle("The Notional Dataset")
png(filename = "df.png")
p1
dev.off()


#K means on Iris
iris_kmeans <- kmeans(iris[,1:4], centers = g)
iris$kmeans <- as.factor(LETTERS[iris_kmeans$cluster])
table(actual = iris$Species, cluster = iris$kmeans)
p2<- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = kmeans)) + geom_point() + ggtitle("K-means on the Iris Dataset")
png(filename = "iris_kmeans.png")
p2
dev.off()

#Gaussian mixed model (GMM) on Iris
iris_mclust <- Mclust(iris[,1:4], G = g)
iris$gmm<- LETTERS[as.factor(iris_mclust$classification)]
table(actual = iris$Species, cluster = iris$gmm)
p3 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = gmm)) + geom_point() + ggtitle("GMM on the Iris Dataset")
png(filename = "iris_gmm.png")
p3
dev.off()

#GMM has a small edge in this case, but ultimately the result looks very similar.
#What's really going on? Let's consider a harder task:

#K means on example df (balcluster =  balloons in a box)
df_kmeans <- kmeans(df[,1:2], centers = g)
df$kmeans <- as.factor(LETTERS[df_kmeans$cluster])
table(actual = df$class, cluster = df$kmeans)
p4 <- ggplot(df, aes(x = x, y = y, color = kmeans)) + geom_point() + ggtitle("K-means on a Notional Dataset")
png(filename = "df_kmeans.png")
p4
dev.off()

#Gaussian mixed model (GMM) on example df
df_mclust <- Mclust(df[,1:2], G = g)
df$gmm<- LETTERS[as.factor(df_mclust$classification)]
table(actual = df$class, cluster = df$gmm)
p5 <- ggplot(df, aes(x = x, y = y, color = gmm)) + geom_point()+ ggtitle("GMM on a Notional Dataset")
png(filename = "df_gmm.png")
p5
dev.off()

#Gaussian mixed model (GMM) on beer df
beer_sub <- beer[!is.na(beer$ibu) & !is.na(beer$abv) & beer$ibu < 180 & beer$abv < 13,]
beer_mclust <- Mclust(beer_sub[,6:7], G = 3)
beer_sub$gmm<- LETTERS[as.factor(beer_mclust$classification)]
table(actual = beer_sub$style_name, cluster = beer_sub$gmm)
p5 <- ggplot(beer_sub, aes(x = abv, y = ibu, color = gmm)) + geom_point()+ ggtitle("Beer GMM")
png(filename = "beer_gmm.png")
p5
dev.off()

#K-means on beer df
beer_sub <- beer[!is.na(beer$ibu) & !is.na(beer$abv) & beer$ibu < 180 & beer$abv < 13,]

beer_mclust <- Mclust(beer_sub[,6:7], G = g)
beer_sub$gmm<- LETTERS[as.factor(beer_mclust$classification)]
table(actual = beer_sub$style_name, cluster = beer_sub$gmm)

beer_kmeans <- kmeans(beer_sub[,6:7], centers = g)
beer_sub$kmeans <- as.factor(LETTERS[beer_kmeans$cluster])
table(actual = beer_sub$style_name, cluster = beer_sub$kmeans)



table(actual = beer_sub$style_name, cluster = beer_sub$gmm)
p5 <- ggplot(beer_sub, aes(x = abv, y = ibu, color = gmm)) + geom_point()+ ggtitle("Beer GMM")
png(filename = "beer_gmm.png")
p5
dev.off()



