# snedata

SNE Dataset Functions for R

This package provides functions for generating simple simulation datasets 
for use in Stochastic Neighbor Embedding and related dimensionality reduction
methods, most obviously the very popular 
[t-SNE](https://lvdmaaten.github.io/tsne/). The datasets included are:

* Synthetic data similar to those used by Lee and co-workers in their 
[JSE](http://dx.doi.org/10.1016/j.neucom.2012.12.036) and 
[multi-JSE](http://dx.doi.org/10.1016/j.neucom.2014.12.095) papers.

* Also, Martin Wattenberg, Fernanda Viégas and Ian Johnson published an 
interactive article 
[How to use t-SNE Effectively](http://distill.pub/2016/misread-tsne/).
The JavaScript functions used to create the simulation datasets
(which can also be found at https://github.com/distillpub/post--misread-tsne),
have been translated into R and are also hosted in this package.

* Additionally, if you have the 
[RnavGraphImageData](https://cran.r-project.org/package=RnavGraphImageData)
package installed, there are also functions to convert the Olivetti and Frey 
faces datasets into a row-based data frame and functions to visualize them.

* Last but not least is code to download and visualize the 
[MNIST database](http://yann.lecun.com/exdb/mnist/), based on 
[a gist by Brendan O'Connor](https://gist.github.com/brendano/39760), who 
graciously allowed it to be MIT-licensed.

## Installing:

```R
install.packages("devtools")
devtools::install_github("jlmelville/snedata")
```

## Documentation:

```R
package?snedata # lists all the functions
?snedata::gaussian_data # contains links to all the other distill.pub functions
```

# Examples
```R
library(snedata)

# 3000 points sampled from the surface of a sphere
sphere3000 <- sphere(n = 3000)

# 1500 points sampled from a toroidal helix with 30 coils:
helix1500 <- helix(n = 1500, nwinds = 30)

# 1500 points from a filled sphere:
ball1500 <- ball(n = 1500)

# 1000 points from a "Swiss Roll" distribution:
swiss1000 <- swiss_roll(n = 1000)

# 1000 points from a five-dimensional gaussian:
gauss1000 <- gaussian_data(n = 1000, dim = 5)

# Load RnavGraphImageData
library(RnavGraphImageData)

# Load the Frey faces dataset with each image as a row
frey <- frey_faces()
# Display the first pose
show_frey_face(frey, 1)

# PCA scores plot, with color indicating the frame index
frey_pca <- prcomp(frey[, -561], retx = TRUE, rank. = 2)
plot(frey_pca$x, col = frey$color, pch = 16, cex = 0.75)

# Load the Olivetti faces dataset with each image as a row
olivetti <- olivetti_faces()
# Show the second pose of the first face
show_olivetti_face(olivetti, 1, 2)

# Generate datasets similar to those used in the main text of "How to Use t-SNE Effectively"
misread_tsne <- list(
	two_clusters = two_clusters_data(n = 50, dim = 2),
	two_different_sized_clusters = two_different_clusters_data(n = 75, dim = 2),
	three_clusters_50 = three_clusters_data(n = 50, dim = 2),
	three_clusters_200 = three_clusters_data(n = 200, dim = 2),
	gaussian_cloud = gaussian_data(n = 500, dim = 100),
	ellipsoidal_gaussian_cloud = long_gaussian_data(n = 100, dim = 50),
	two_long_linear_clusters = long_cluster_data(n = 75),
	cluster_in_cluster = subset_clusters_data(n = 75, dim = 50),
	linked_rings = link_data(n = 100),
	trefoil_knot = trefoil_data(n = 150)
)

# fetch the MNIST data set from the MNIST website
mnist <- download_mnist()

# view the fifth digit
show_mnist_digit(mnist, 5)

# first 60,000 instances are the training set
mnist_train <- head(mnist, 60000)
# the remaining 10,000 are the test set
mnist_test <- tail(mnist, 10000)

# PCA on 1000 random training examples
mnist_r1000 <- mnist_train[sample(nrow(mnist_train), 1000), ]

pca <- prcomp(mnist_r1000[, 1:784], retx = TRUE, rank. = 2)
# plot the scores of the first two components
plot(pca$x[, 1:2], type = 'n')
text(pca$x[, 1:2], labels = mnist_r1000$Label, cex = 0.5,
  col = rainbow(length(levels(mnist_r1000$Label)))[mnist_r1000$Label])

# save to disk
save(mnist, file = "mnist.Rda")
```

## See also

* I have a similar R package for downloading the [COIL-20](https://github.com/jlmelville/coil20) dataset (under a different 
license).
* For downloading the MNIST digits database, there is a [similar project](https://github.com/xrobin/mnist) by [Xavier Robin](https://github.com/xrobin).
* Shamless plug: while the [Rtsne](https://cran.r-project.org/package=Rtsne) package should probably be your first stop to play with embedding these datasets with t-SNE in R, I have used these datasets successfully with my own experimental (translation: slow) package 
[sneer](https://github.com/jlmelville/sneer), which has its roots &mdash; now mangled beyond recognition &mdash; in Justin Donaldson's [tsne](https://cran.r-project.org/package=tsne) package.

## License

This package is licensed under 
[the MIT License](http://opensource.org/licenses/MIT).
