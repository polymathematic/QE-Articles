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
require(NbClust)
require(dbscan)

#Beer Data (30 is American IPA, 25 is American Pale, 31 is Imperial IPA)
beer <- list()
beer$AmericanIPA <- getBeer(n = 400, params = list(styleId = "30"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer$AmericanPale <- getBeer(n = 400, params = list(styleId = "25"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer$ImperialIPA <- getBeer(n = 400, params = list(styleId = "31"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer$Amber <- getBeer(n = 400, params = list(styleId = "32"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer$Saison <- getBeer(n = 400, params = list(styleId = "72"), key = "683b056a8ef0a784770dfeb66c6460ce")
beer <- bind_rows(beer)

#Example Clustering

example <- data.frame(x = c(rnorm(300, 0, 10), rnorm(100, 5, 2), rnorm(100, -5, 2)), 
                      y = c(rnorm(300, 0, 10), rnorm(100, 0, 2), rnorm(100, 0, 2)),
                      class = c(rep("A",300), rep("B", 100), rep("C", 100)))

#Show advantages/disadvantages to clustering by kmeans vs gmm using this tortured dataset




#Beer Clustering
g = 5
beer_sub <- beer[!is.na(beer$ibu) & !is.na(beer$abv) & beer$ibu < 180 & beer$abv < 13,]

beer_mclust <- Mclust(log(beer_sub[,6:7]))
beer_sub$gmm<- LETTERS[as.factor(beer_mclust$classification)]
table(actual = beer_sub$style_name, cluster = beer_sub$gmm)

beer_kmeans <- kmeans(log(beer_sub[,6:7]), centers = g)
beer_sub$kmeans <- as.factor(LETTERS[beer_kmeans$cluster])
table(actual = beer_sub$style_name, cluster = beer_sub$kmeans)

table(actual = beer_sub$style_name, cluster = beer_sub$gmm)
p5 <- ggplot(beer_sub, aes(x = abv, y = ibu, color = gmm)) + geom_point()+ ggtitle("Beer GMM")
p5

#Remove the incomplete records
SampleBeer_complete <- SampleBeer %>% filter(!is.na(abv), !is.na(ibu))

#NB_clust
test <- NbClust(SampleBeer[,6:7], method = "centroid", index = 'silhouette', max.nc = 15)

#dbscan
beer_dbscan <- dbscan(SampleBeer_complete[,6:7], eps = 1)
SampleBeer_complete$dbscan_clust <- beer_dbscan$cluster

