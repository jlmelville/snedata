# Datasets

This page has a compact table of the available data and some longer
notes and examples.

## Summary

| Dataset family | Main functions | Rows | Notes |
|----|----|---:|----|
| Synthetic data | [`sphere()`](https://jlmelville.github.io/snedata/reference/sphere.md), [`ball()`](https://jlmelville.github.io/snedata/reference/ball.md), [`helix()`](https://jlmelville.github.io/snedata/reference/helix.md), [`swiss_roll()`](https://jlmelville.github.io/snedata/reference/swiss_roll.md), [`s_curve()`](https://jlmelville.github.io/snedata/reference/s_curve.md) | User-chosen | Classic synthetic manifold and simple curve datasets. |
| Distill t-SNE simulations | [`gaussian_data()`](https://jlmelville.github.io/snedata/reference/gaussian_data.md), [`two_clusters_data()`](https://jlmelville.github.io/snedata/reference/two_clusters_data.md), [`random_walk()`](https://jlmelville.github.io/snedata/reference/random_walk.md), [`trefoil_data()`](https://jlmelville.github.io/snedata/reference/trefoil_data.md) | User-chosen | R translations of the data used in ‘How to Use t-SNE Effectively’. |
| PaCMAP and topological examples | [`s_curve_hole()`](https://jlmelville.github.io/snedata/reference/s_curve_hole.md), [`curve2d()`](https://jlmelville.github.io/snedata/reference/curve2d.md), [`synthetic_hierarchical_data()`](https://jlmelville.github.io/snedata/reference/synthetic_hierarchical_data.md), [`taspheres()`](https://jlmelville.github.io/snedata/reference/taspheres.md) | User-chosen | More synthetic data used in the PaCMAP and Topological Autoencoders papers. |
| Mammoth point clouds | [`download_mammoth10k()`](https://jlmelville.github.io/snedata/reference/download_mammoth10k.md), [`download_mammoth50k()`](https://jlmelville.github.io/snedata/reference/download_mammoth50k.md) | 10,000 or 50,000 | Downloads JSON point-cloud data. |
| MNIST and relatives | [`download_mnist()`](https://jlmelville.github.io/snedata/reference/download_mnist.md), [`download_fashion_mnist()`](https://jlmelville.github.io/snedata/reference/download_fashion_mnist.md), [`download_kuzushiji_mnist()`](https://jlmelville.github.io/snedata/reference/download_kuzushiji_mnist.md), [`download_qmnist()`](https://jlmelville.github.io/snedata/reference/download_qmnist.md) | 70,000 or 120,000 | Images often in ten equally-sized classes. |
| CIFAR-10 | [`download_cifar10()`](https://jlmelville.github.io/snedata/reference/download_cifar10.md), [`show_cifar()`](https://jlmelville.github.io/snedata/reference/show_cifar.md) | 60,000 | The well-known image dataset for machine learning. |
| Small NORB | [`download_norb_small()`](https://jlmelville.github.io/snedata/reference/download_norb_small.md), [`show_norb_object()`](https://jlmelville.github.io/snedata/reference/show_norb_object.md) | 48,600 | Stereo images of 50 toys under different angles and lighting conditions. |
| 20 Newsgroups | [`download_twenty_newsgroups()`](https://jlmelville.github.io/snedata/reference/download_twenty_newsgroups.md) | 18,846 | Text data; use packages such as `tm` or `tidytext` for preprocessing. |
| Frey and Olivetti faces | [`frey_faces()`](https://jlmelville.github.io/snedata/reference/frey_faces.md), [`olivetti_faces()`](https://jlmelville.github.io/snedata/reference/olivetti_faces.md) | 1,965 and 400 | Images of faces. Requires the suggested `RnavGraphImageData` package. |
| Isomap datasets | [`download_isomap_swiss_roll()`](https://jlmelville.github.io/snedata/reference/download_isomap_swiss_roll.md), [`download_isomap_faces()`](https://jlmelville.github.io/snedata/reference/download_isomap_faces.md) | 20,000 and 698 | Downloads archived Matlab data; requires the suggested `R.matlab` package, and `gzip` or `uncompress` for faces. |

For MNIST, QMNIST, CIFAR-10, and Small NORB, you can return a list
containing the matrix of the numeric data and any labels separately.
Supply `as = "matrix"`.

## Notes

- Synthetic data similar to those used by Lee and co-workers in their
  [JSE](http://dx.doi.org/10.1016/j.neucom.2012.12.036) and
  [multi-JSE](http://dx.doi.org/10.1016/j.neucom.2014.12.095) papers.

- Martin Wattenberg, Fernanda Viégas and Ian Johnson published an
  interactive article [How to use t-SNE
  Effectively](http://distill.pub/2016/misread-tsne/). The JavaScript
  functions used to create the simulation datasets (which can also be
  found at <https://github.com/distillpub/post--misread-tsne>), have
  been translated into R.

- If you have the
  [RnavGraphImageData](https://cran.r-project.org/package=RnavGraphImageData)
  package installed, there are also functions to convert the Olivetti
  and Frey faces datasets into a row-based data frame and functions to
  visualize them.

- The original Isomap Swiss Roll and face-pose datasets are available
  via the Internet Archive. These functions require the
  [R.matlab](https://cran.r-project.org/package=R.matlab) package to
  read Matlab files, and the face-pose dataset also requires an external
  `gzip` or `uncompress` command to read the Unix `compress` file.

- Code to download and visualize the [MNIST
  database](http://yann.lecun.com/exdb/mnist/), based on [a gist by
  Brendan O’Connor](https://gist.github.com/brendano/39760), who
  graciously allowed it to be MIT-licensed.

- [QMNIST](https://github.com/facebookresearch/qmnist) extends the MNIST
  test dataset to 60,000 digits (so 120,000 digits in total).

- The [Fashion-MNIST](https://github.com/zalandoresearch/fashion-mnist)
  dataset is intended as a drop-in replacement for the MNIST digits
  database, but using images of fashion items, and to be harder to
  perform well with machine learning benchmarks. Items can be visualized
  with the same function intended for the MNIST digits.

- The [Kuzushiji-MNIST](https://github.com/rois-codh/kmnist) dataset,
  another drop-in replacement for the MNIST digits, but this time of
  types of cursive Japanese characters.

- The [Small NORB](https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/)
  dataset, which consists of pairs of images of 50 toys from different
  angles and under different lighting conditions.

- The [CIFAR-10](https://www.cs.toronto.edu/~kriz/cifar.html) dataset,
  which consists of 60,000 32 x 32 color images in ten different
  classes.

- A 3D point cloud of a [mammoth at the
  Smithsonian](https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12),
  from [Understanding
  UMAP](https://pair-code.github.io/understanding-umap/), based on work
  originally done by [Max
  Noichl](https://github.com/MNoichl/UMAP-examples-mammoth-).

- A 3D S-curve with a hole data set, used to validate the [PaCMAP
  method](https://arxiv.org/abs/2012.04456) (see also the [github
  repo](https://github.com/YingfanWang/PaCMAP)). Bonus: a translation
  from Python of the
  [sklearn.datasets.make_s_curvefunction](https://scikit-learn.org/stable/modules/generated/sklearn.datasets.make_s_curve.html).
  Also: a simple 2D curve dataset, useful for testing initialization
  strategies. The synthetic hierarchical data set from the same paper is
  also available via
  [`synthetic_hierarchical_data()`](https://jlmelville.github.io/snedata/reference/synthetic_hierarchical_data.md).

## Examples

``` r

library(snedata)

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
