# Gaussian Mixed Model Clustering Example

A Gaussian Mixture Model(GMM) is an interesting alternative to more familiar clustering techniques such as k-means or heirarchical clustering. This approach is not usually included in lists of basic techniques that should be in every data scientst's toolbox. This is a lost opportunity. Though GMM is more sophisticated from a mathematical point of view, it is an incredibly flexible technique with many advantages that can be easily grasped by anyone with a basic familiarity of data science and statistics.

Like k-means, GMM is an unsupervised classification technique that takes a number of centers and a n-dimensional feature space as essential inputs. It differs from k-means in that instead of defining discrete regions, GMM models fit joint gaussian distributions within the feature space. Consequently, the raw output of a GMM model is a probability that every observation belongs to a particular observation, rather than a single classification as in k-means. As a result, GMM modeling is generally more flexible and usually a better fit for most straight clustering applications assuming the dimensions of the feature space follow a normal distribution. For an in-depth tutorial see [here](https://www.youtube.com/watch?v=qMTuMa86NzU).

The code in this repository is an example implementation of GMM across two datasets: the iris dataset and notional data created for this example. The iris dataset should be very familiar as it is used as the textbook example of a dataset that clusters well with all methods. The second is a set of three normal distributions, one of which envelops the other two. Both datasets have three true classes, a fact we will take to be known. Choosing the right number of classes is a different chore.

# The Iris Dataset 

For the Iris dataset, both methods perform well:

```
#Clear environment
rm(list = ls())
gc()

#Set seed
set.seed(3)

#Set number of centroids
g <- 3

#Load packages
require(mclust)
require(ggplot2)

#Load iris data
data(iris)

#Plot iris data
p0 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = species)) +
      geom_point() + ggtitle("The Iris Dataset")
png(filename = "iris.png")
p0
dev.off()
```
![iris](/GMM-Tutorial/iris.png)
```
#K means on Iris
iris_kmeans <- kmeans(iris[,1:4], centers = g)
iris$kmeans <- as.factor(LETTERS[iris_kmeans$cluster])
table(actual = iris$Species, cluster = iris$kmeans) #Confusion matrix
p1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = kmeans)) +
      geom_point() + ggtitle("K-means on the Iris Dataset")
png(filename = "iris_kmeans.png")
p1
dev.off()
```
![iris_kmeans](/GMM-Tutorial/iris_kmeans.png)

```
#Gaussian mixed model (GMM) on Iris
iris_mclust <- Mclust(iris[,1:4], G = g)
iris$gmm<- LETTERS[as.factor(iris_mclust$classification)]
table(actual = iris$Species, cluster = iris$gmm)
p2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = gmm)) +
      geom_point() + ggtitle("GMM on the Iris Dataset")
png(filename = "iris_gmm.png")
p2
dev.off()
```
![iris_gmm](/GMM-Tutorial/iris_gmm.png)

The code above produces the following charts. These plus the confustion matrices generated above show that both techniques create roughly similar estimates, both in line with the true classification.

The usefulness of this example is undermined by the ease with which it's clustered. It does not show the distinctions between the two approaches and when one would want to favor one over the other. This requires a different example.

# An Alternate Example

To illustrate the difference between these approaches, we create a dataset that k-means will have difficulty evaluating:

```
#Clear environment
rm(list = ls())
gc()

#Set seed
set.seed(3)

#Set number of centroids
g <- 3

#Load packages
require(mclust)
require(ggplot2)

#Create example data
df <- data.frame(x = c(rnorm(1000, 0, 10), rnorm(500, 5, 2), rnorm(500, -5, 2)), 
                 y = c(rnorm(1000, 0, 10), rnorm(500, 0, 2), rnorm(500, 0, 2)),
                 class = c(rep("A",1000), rep("B", 500), rep("C", 500)))

p1 <- ggplot(df, aes(x = x, y = y, color = class)) +
      geom_point() + ggtitle("The Notional Dataset")
png(filename = "df.png")
p1
dev.off()
```
![iris](/GMM-Tutorial/df.png)
```
#K means on notional data
df_kmeans <- kmeans(df[,1:2], centers = g)
df$kmeans <- as.factor(LETTERS[df_kmeans$cluster])
table(actual = df$class, cluster = df$kmeans)
p4 <- ggplot(df, aes(x = x, y = y, color = kmeans)) +
      geom_point() + ggtitle("K-means on a Notional Dataset")
png(filename = "df_kmeans.png")
p4
dev.off()
```
![df_kmeans](/GMM-Tutorial/df_kmeans.png)

```
#Gaussian mixed model (GMM) on notional data
df_mclust <- Mclust(df[,1:2], G = g)
df$gmm<- LETTERS[as.factor(df_mclust$classification)]
table(actual = df$class, cluster = df$gmm)
p5 <- ggplot(df, aes(x = x, y = y, color = gmm)) +
      geom_point()+ ggtitle("GMM on a Notional Dataset")
png(filename = "df_gmm.png")
p5
dev.off()
```
![iris_gmm](/GMM-Tutorial/df_gmm.png)

Here the material difference between the two techniques starts to become clear. Traditional k-means "pushes" the class centroids away from each other, creating a Voronoi structure in the feature space. Picture three baloons pressed together in a box and you have a pretty good idea of how k-means splits data. In this case, Kmeans observes the macro structure of the overall point cloud and splits it radially. While this provides good recall for the smaller cluster, it misses the larger class. 

GMM attempts to fit multivariate Gaussian distributions to the data. There's nothing that precludes these distributions from overlapping. In this case, GMM identifies that there are three bivariate gaussian distributions, and correctly estimates the parameters for each, resulting in a far more  accurate estimation for all three classes. 

Another benefit of GMM is that because it estimates distributions, each observation is associated with a discrete probability that it is in a given class, rather than an "all or nothing" classification. Thus, a point can be said to be 60% - A, 30% - B, and 10% - C rather than simply "A". This adds flexibility for the user to adjust the threshold for declaring a class. This may be useful in applications where a high degree of certainty is important for decision making. The ability to say "I don't know" is an unappreciated feature.

# Credit
* Chris Fraley, Adrian E. Raftery, T. Brendan Murphy, and Luca Scrucca (2012) mclust Version 4 for R: Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation Technical Report No. 597, Department of Statistics, University of Washington
* Chris Fraley and Adrian E. Raftery (2002) Model-based Clustering, Discriminant Analysis and Density Estimation Journal of the American Statistical Association 97:611-631
