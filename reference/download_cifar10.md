# Download CIFAR-10

Download CIFAR-10 database of images.

## Usage

``` r
download_cifar10(
  url = "https://www.cs.toronto.edu/~kriz/cifar-10-binary.tar.gz",
  destfile = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix")
)
```

## Format

A data frame with 3074 variables:

- `r1`, `r2`, `r3` ... `r1024`: Integer pixel value of the red channel
  of the image, from 0 to 255.

- `g1`, `g2`, `g3` ... `g1024`: Integer pixel value of the green channel
  of the image, from 0 to 255.

- `b1`, `b2`, `b3` ... `b1024`: Integer pixel value of the blue channel
  of the image, from 0 to 255.

- `Label`: The image category, represented by a factor in the range 0-9.

- `Description`: The name of the image category associated with `Label`,
  represented by a factor.

The pixel features are organized row-wise from the top left of each
image. The `Label` levels correspond to the following class names
(stored in the `Description` column):

- `0`: Airplane

- `1`: Automobile

- `2`: Bird

- `3`: Cat

- `4`: Deer

- `5`: Dog

- `6`: Frog

- `7`: Horse

- `8`: Ship

- `9`: Truck

There are 60,000 items in the data set. The first 50,000 are the
training set, and the remaining 10,000 are the testing set.

Items in the dataset can be visualized with the
[`show_cifar()`](https://jlmelville.github.io/snedata/reference/show_cifar.md)
function.

For more information see <https://www.cs.toronto.edu/~kriz/cifar.html>.

## Arguments

- url:

  URL of the CIFAR-10 data.

- destfile:

  Filename for where to download the CIFAR-10 tarfile. If `NULL`, a file
  in a temporary work directory is used. It will be untarred and
  processed in the same directory.

- cleanup:

  If `TRUE`, then `destfile` and the untarred data will be deleted
  before the function returns. Only worth setting to `FALSE` to debug
  problems.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

- as:

  Return format. Use `"data.frame"` for the original data frame shape,
  or `"matrix"` for a list with `data`, `labels`, and `descriptions`.

## Value

If `as = "data.frame"`, a data frame containing the CIFAR-10 dataset. If
`as = "matrix"`, a list with `data`, an integer matrix with one image
per row, `labels`, a factor of numeric class labels, and `descriptions`,
a factor of class names.

## Details

Downloads the image and label files for the training and test datasets
and converts them to a data frame or a matrix/list result.

The CIFAR-10 dataset contains 60000 32 x 32 color images, divided into
ten different classes, with 6000 images per class.

## References

The CIFAR-10 dataset <https://www.cs.toronto.edu/~kriz/cifar.html>

Krizhevsky, A., & Hinton, G. (2009). *Learning multiple layers of
features from tiny images* (Vol. 1, No. 4, p. 7). Technical report,
University of Toronto.

## Examples

``` r
if (FALSE) { # \dontrun{
# download the data set
cifar10 <- download_cifar10(verbose = TRUE)

# first 50,000 instances are the training set
cifar10_train <- head(cifar10, 50000)
# the remaining 10,000 are the test set
cifar10_test <- tail(cifar10, 10000)

# PCA on 1000 examples
cifar10_r1000 <- cifar10[sample(nrow(cifar10), 1000), ]
pca <- prcomp(cifar10_r1000[, 1:(32 * 32)], retx = TRUE, rank. = 2)
# plot the scores of the first two components
plot(pca$x[, 1:2], type = "n")
text(pca$x[, 1:2],
  labels = cifar10_r1000$Label,
  col = rainbow(length(levels(cifar10$Label)))[cifar10_r1000$Label]
)
} # }
```
