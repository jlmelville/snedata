# snedata

SNE Dataset Functions for R

This package provides functions for generating simple simulation datasets for
use in Stochastic Neighbor Embedding and related dimensionality reduction
methods, most obviously the very popular
[t-SNE](https://lvdmaaten.github.io/tsne/).

## News

*March 2 2025* New link to download the MNIST digits. The original pages on 
Yann LeCun's website seem to have gone kaput. Fortunately 
<https://github.com/fgnt/mnist> maintained a mirror.

*November 25 2023* Added the [20 Newsgroups](http://qwone.com/~jason/20Newsgroups/)
text dataset. You'll need to look at packages like [tm](https://cran.r-project.org/package=tm)
or [tidytext](https://cran.r-project.org/package=tidytext) to process this into 
a form suitable for embedding.

*May 14 2022* Added the `taspheres` function to generate the "spheres" dataset
of high dimensional spheres nested inside a larger sphere, as used in the 
[Topological Autoencoders](https://arxiv.org/abs/1906.00722) paper.

*December 24 2021*. Added the S-curve with a hole and 2D curve datasets used in
the [PaCMAP](https://arxiv.org/abs/2012.04456) paper.

*December 21 2021*. Added the mammoth datasets from [Understanding UMAP](https://pair-code.github.io/understanding-umap/).

*June 29 2019*. Added [QMNIST](https://github.com/facebookresearch/qmnist).

*March 6 2019*. Added [CIFAR-10](https://www.cs.toronto.edu/~kriz/cifar.html).

*February 23 2019*. Added [Small NORB](https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/).

*December 14 2018*. Added [Kuzushiji-MNIST](https://github.com/rois-codh/kmnist).

## Datasets

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

* Code to download and visualize the 
[MNIST database](http://yann.lecun.com/exdb/mnist/), based on 
[a gist by Brendan O'Connor](https://gist.github.com/brendano/39760), who 
graciously allowed it to be MIT-licensed.

* [QMNIST](https://github.com/facebookresearch/qmnist) extends the MNIST test
dataset to 60,000 digits.

* The [Fashion-MNIST](https://github.com/zalandoresearch/fashion-mnist) dataset 
is intended as drop-in replacement for the MNIST digits database, but using 
images of fashion items, and to be harder to perform well with machine learning
benchmarks. Items can be visualized with the same function intended for the 
MNIST digits.

* The [Kuzushiji-MNIST](https://github.com/rois-codh/kmnist) dataset, another
drop-in replacement for the MNIST digits, but this time of types of cursive
Japanese characters.

* The [Small NORB](https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/) dataset,
which consists of pairs of images of 50 toys from different angles and under
different lighting conditions.

* The [CIFAR-10](https://www.cs.toronto.edu/~kriz/cifar.html) dataset, which
consists of 60,000 32 x 32 color images in ten different classes.

* A 3D point cloud of a [mammoth at the
Smithsonian](https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12),
from [Understanding UMAP](https://pair-code.github.io/understanding-umap/),
based on work originally done by [Max
Noichl](https://github.com/MNoichl/UMAP-examples-mammoth-).

* A 3D S-curve with a hole data set, used to validate the [PaCMAP
method](https://arxiv.org/abs/2012.04456) (see also the [github
repo](https://github.com/YingfanWang/PaCMAP)). Bonus: a translation from Python
of the [sklearn.datasets.make_s_curve
function](https://scikit-learn.org/stable/modules/generated/sklearn.datasets.make_s_curve.html).
Also: a simple 2D curve dataset, useful for testing initialization strategies.

## Installing:

```R
install.packages("remotes")
remotes::install_github("jlmelville/snedata")
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

# fetch the Fashion-MNIST dataset
fashion <- download_fashion_mnist()

# Works as a drop-in replacement for the MNIST digits, can repeat the above
# view the fifth item etc.
show_mnist_digit(fashion, 5)

# similarly for Kuzushiji-MNIST dataset of Japanese cursive characters
# (set verbose flag to see download progress)
kuzushiji <- download_kuzushiji_mnist(verbose = TRUE)
# View the tenth character
show_mnist_digit(kuzushiji, 10)

# Download the small NORB dataset
norb <- download_norb_small(verbose = TRUE)
# View an image, compare with example at https://github.com/ndrplz/small_norb
show_norb_object(norb, category = 2, instance = 6, elevation = 6, azimuth = 24, lighting = 2)
```

## See also

* The [mlbench](https://cran.r-project.org/package=mlbench) package.
* I maintain a similar [R package](https://github.com/jlmelville/coil20) (under 
a different license) for downloading the 
[COIL-20](http://www.cs.columbia.edu/CAVE/software/softlib/coil-20.php) and
[COIL-100](http://www.cs.columbia.edu/CAVE/software/softlib/coil-100.php)
datasets.
* For downloading the MNIST digits database, there is a 
[similar project](https://github.com/xrobin/mnist) by 
[Xavier Robin](https://github.com/xrobin).
* A gist for [downloading the Isomap Swiss Roll and Faces as R dataframes](https://gist.github.com/jlmelville/339dfeb80c3e836e887d70a37679b244)

## License

This package is licensed under 
[the MIT License](http://opensource.org/licenses/MIT).
