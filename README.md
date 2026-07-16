# snedata

SNE Dataset Functions for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/jlmelville/snedata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jlmelville/snedata/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/jlmelville/snedata/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jlmelville/snedata?branch=master)
<!-- badges: end -->

This package provides functions for generating simple simulation datasets for use in Stochastic
Neighbor Embedding and related dimensionality reduction methods, most obviously the very popular
[t-SNE](https://lvdmaaten.github.io/tsne/).

## Datasets

The package includes simulation datasets, the datasets from
[How to use t-SNE Effectively](http://distill.pub/2016/misread-tsne/), optional Frey and Olivetti
face helpers, MNIST-like datasets, CIFAR-10, Small NORB, mammoth point clouds, 20 Newsgroups,
COIL object-image datasets, historical Isomap datasets, and a few other examples used in
dimensionality reduction papers.

The pkgdown site has a fuller
[datasets article](https://jlmelville.github.io/snedata/articles/datasets.html) with a table,
notes, and longer examples.

## Install

```R
install.packages("pak")
pak::pak("jlmelville/snedata")
```

## Documentation

```R
package?snedata # lists all the functions
?snedata::gaussian_data # contains links to all the other distill.pub functions
```

## Examples

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

# To avoid a very wide data frame and retain explicit split metadata:
mnist_list <- download_mnist(as = "list")
mnist_train <- mnist_list$data[mnist_list$meta$split == "training", ]

# download the original Isomap Swiss Roll dataset from the Internet Archive
# requires the optional R.matlab package
isomap_swiss <- download_isomap_swiss_roll()

# download the original Isomap faces dataset from the Internet Archive
# requires R.matlab and an external gzip or uncompress command
isomap_faces <- download_isomap_faces()
show_isomap_face(isomap_faces, 1)

# download COIL-20 object images
# requires the optional png package
coil20 <- download_coil20(as = "list")
show_coil_object(coil20, object = 5, pose = 4)
```

## See also

* The [mlbench](https://cran.r-project.org/package=mlbench) package.
* For downloading the MNIST digits database, there is a
[similar project](https://github.com/xrobin/mnist) by [Xavier Robin](https://github.com/xrobin).

## License

This package is licensed under [the MIT License](http://opensource.org/licenses/MIT).
