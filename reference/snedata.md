# snedata: Dataset functions for Stochastic Neighbor Embedding and related dimensionality reduction methods.

Functions to download or generate some datasets commonly used for
benchmarking various dimensionality reduction methods.

## Note

The faces datasets originate from Sam Roweis' dataset web page:
[http://www.cs.nyu.edu/~roweis/data.html](http://www.cs.nyu.edu/~roweis/data.md).

Code to download and visualize the MNIST digits and fashion datasets
originates from a gist by Brendan O'Connor:
<https://gist.github.com/brendano/39760>.

## Simulation functions

Very simple datasets for characterizing the behavior of various
embedding algorithms some, from Lee et al (2016) and Agrafiotis and Xu
(2002).

- [`swiss_roll()`](https://jlmelville.github.io/snedata/reference/swiss_roll.md).
  A 2D plane curled into 3D.

- [`sphere()`](https://jlmelville.github.io/snedata/reference/sphere.md).
  Points sampled from the surface of a 3D sphere.

- [`ball()`](https://jlmelville.github.io/snedata/reference/ball.md).
  Points sampled from the interior of a 3D sphere.

- [`helix()`](https://jlmelville.github.io/snedata/reference/helix.md).
  Points sampled from a 3D toroidal helix with the ends jointed
  together.

## Simulation functions from "How to use t-SNE Effectively"

An online article from Wattenberg et al (2016), this runs t-SNE live in
your browser. Much of the data pertains to investigating the behavior of
clusters of gaussians of varying dimensionality, heterogeneity and
relative separation. There's quite a lot of functions:

- [`circle_data()`](https://jlmelville.github.io/snedata/reference/circle_data.md).
  A 2D circle.

- [`cube_data()`](https://jlmelville.github.io/snedata/reference/cube_data.md).
  A cube.

- [`gaussian_data()`](https://jlmelville.github.io/snedata/reference/gaussian_data.md).
  A Gaussian.

- [`grid_data()`](https://jlmelville.github.io/snedata/reference/grid_data.md).
  A 2D grid.

- [`link_data()`](https://jlmelville.github.io/snedata/reference/link_data.md).
  Two linked circles.

- [`long_cluster_data()`](https://jlmelville.github.io/snedata/reference/long_cluster_data.md).
  Two long parallel clusters.

- [`long_gaussian_data()`](https://jlmelville.github.io/snedata/reference/long_gaussian_data.md).
  A gaussian with unequal standard deviation in its dimensions.

- [`ortho_curve()`](https://jlmelville.github.io/snedata/reference/ortho_curve.md).
  Points related by mutually orthogonal steps.

- [`random_circle_cluster_data()`](https://jlmelville.github.io/snedata/reference/random_circle_cluster_data.md).
  Points sampled from a circle and jittered.

- [`random_circle_data()`](https://jlmelville.github.io/snedata/reference/random_circle_data.md).
  Points randomly sampled from the circumference of a circle.

- [`random_jump()`](https://jlmelville.github.io/snedata/reference/random_jump.md).
  Points generated from a random walk with an extra gaussian
  perturbation.

- [`random_walk()`](https://jlmelville.github.io/snedata/reference/random_walk.md).
  Points generated from a random walk.

- [`simplex_data()`](https://jlmelville.github.io/snedata/reference/simplex_data.md).
  Points arranged as a rough simplex.

- [`subset_clusters_data()`](https://jlmelville.github.io/snedata/reference/subset_clusters_data.md).
  A small cluster inside a larger one.

- [`three_clusters_data()`](https://jlmelville.github.io/snedata/reference/three_clusters_data.md).
  Points sampled from three gaussian clusters with unequal relative
  distances.

- [`trefoil_data()`](https://jlmelville.github.io/snedata/reference/trefoil_data.md).
  Points sampled in the shape of 3D trefoil knot.

- [`two_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_clusters_data.md).
  Two gaussian clusters.

- [`two_different_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_different_clusters_data.md).
  Two gaussian clusters, with differing standard deviations.

- [`unlink_data()`](https://jlmelville.github.io/snedata/reference/unlink_data.md).
  Two unlinked circles.

## Faces dataset functions

If you have the `RnavGraphImageData` package
<https://cran.r-project.org/package=RnavGraphImageData> installed and
loaded, then there are functions to convert the Olivetti and Frey faces
datasets into a row-based format, and functions to visualize the images.

- [`frey_faces()`](https://jlmelville.github.io/snedata/reference/frey_faces.md).
  Loads the Frey faces as a row-based data frame.

- [`show_frey_face()`](https://jlmelville.github.io/snedata/reference/show_frey_face.md).
  Display one of the poses from the Frey faces.

- [`olivetti_faces()`](https://jlmelville.github.io/snedata/reference/olivetti_faces.md).
  Loads the Olivetti faces as a row-based data frame.

- [`show_olivetti_face()`](https://jlmelville.github.io/snedata/reference/show_olivetti_face.md).
  Display one of the poses from the Olivetti faces.

## Historical Isomap dataset functions

The original Isomap Swiss Roll and face-pose datasets can be downloaded
from the Internet Archive. The download functions require optional
packages: `R.matlab` for Matlab files. The compressed face data also
requires an external `gzip` or `uncompress` command.

- [`download_isomap_swiss_roll()`](https://jlmelville.github.io/snedata/reference/download_isomap_swiss_roll.md).
  Downloads the original Isomap Swiss-roll data as a row-based data
  frame.

- [`download_isomap_faces()`](https://jlmelville.github.io/snedata/reference/download_isomap_faces.md).
  Downloads the original Isomap face-pose data as a row-based data
  frame.

- [`show_isomap_face()`](https://jlmelville.github.io/snedata/reference/show_isomap_face.md).
  Display one of the Isomap faces.

## MNIST dataset functions

Another (much larger) image dataset is the MNIST digits dataset. The
original data set is described at <http://yann.lecun.com/exdb/mnist/>,
and this package downloads from the <https://github.com/fgnt/mnist>
mirror. Functions are available to download the dataset as a data frame
or canonical list result and visualize individual digits:

- [`download_mnist()`](https://jlmelville.github.io/snedata/reference/download_mnist.md).
  Downloads the MNIST dataset files as a row-based data frame by
  default, or as a canonical list result with `as = "list"`.

- [`show_mnist_digit()`](https://jlmelville.github.io/snedata/reference/show_mnist_digit.md).
  Display one of the MNIST digits.

## Fashion-MNIST functions

The Fashion-MNIST dataset (Xiao et al. 2017) is designed as a drop-in
replacement

## References

Agrafiotis, D. K., & Xu, H. (2002). A self-organizing principle for
learning nonlinear manifolds. *Proceedings of the National Academy of
Sciences*, *99*(25), 15869-15872.

Lee, J. A., Peluffo-Ordo'nez, D. H., & Verleysen, M. (2015). Multi-scale
similarities in stochastic neighbour embedding: Reducing dimensionality
while preserving both local and global structure. *Neurocomputing*,
*169*, 246-261.

Wattenberg, M., Vie'gas, F., & Johnson, I. (2016) How to Use t-SNE
Effectively. *Distill* <http://doi.org/10.23915/distill.00002>

Xiao, H., Kashif, R., & Vollgraf, R. (2017). Fashion-MNIST: a Novel
Image Dataset for Benchmarking Machine Learning Algorithms. *arXiv
preprint* *arXiv:1708.07747*.
<https://github.com/zalandoresearch/fashion-mnist/>

## See also

Useful links:

- <https://jlmelville.github.io/snedata/>

- <https://github.com/jlmelville/snedata>

- Report bugs at <https://github.com/jlmelville/snedata/issues>

## Author

**Maintainer**: James Melville <jlmelville@gmail.com>

Authors:

- James Melville <jlmelville@gmail.com>

## Examples

``` r
# 300 points sampled from the surface of a sphere
sphere300 <- sphere(n = 300)

# 150 points sampled from a toroidal helix with 30 coils:
helix150 <- helix(n = 150, nwinds = 30)

# 150 points from a filled sphere:
ball150 <- ball(n = 150)

# 100 points from a "Swiss Roll" distribution:
swiss100 <- swiss_roll(n = 100)

# 50 points from a 2D gaussian
g2d <- gaussian_data(n = 50, dim = 2)

if (FALSE) { # \dontrun{
# Load RnavGraphImageData
library(RnavGraphImageData)

# Load the Frey faces dataset with each image as a row
frey <- frey_faces()
# Display the first pose
show_frey_face(frey, 1)

# Load the Olivetti faces dataset with each image as a row
olivetti <- olivetti_faces()
# Show the second pose of the first face
show_olivetti_face(olivetti, 1, 2)
} # }

if (FALSE) { # \dontrun{
# fetch the MNIST data set from the configured mirror
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
plot(pca$x[, 1:2], type = "n")
text(pca$x[, 1:2],
  labels = mnist_r1000$Label, cex = 0.5,
  col = rainbow(length(levels(mnist_r1000$Label)))[mnist_r1000$Label]
)

# save to disk
save(mnist, file = "mnist.Rda")
} # }

if (FALSE) { # \dontrun{
# fetch the Fashion-MNIST data
fashion <- download_fashion_mnist()

# You can repeat the same example as with the MNIST digits example.
# Shows the fifth Fashion-MNIST "digit" (actually a dress)
show_mnist_digit(fashion, 5)

# and so on...
} # }
```
