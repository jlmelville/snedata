# Download MNIST

Download MNIST database of handwritten digits.

## Usage

``` r
download_mnist(
  base_url = mnist_url,
  verbose = FALSE,
  as = c("data.frame", "list"),
  timeout = 1800
)
```

## Format

A data frame with 785 variables:

- `px1`, `px2`, `px3` ... `px784`: Integer pixel value, from 0 (white)
  to 255 (black).

- `Label`: The digit represented by the image, in the range 0-9.

Pixels are organized row-wise. The `Label` variable is stored as a
factor.

There are 70,000 digits in the data set. The first 60,000 are the
training set, as found in the `train-images-idx3-ubyte.gz` file. The
remaining 10,000 are the test set, from the `t10k-images-idx3-ubyte.gz`
file.

Items in the dataset can be visualized with
[`show_mnist_digit()`](https://jlmelville.github.io/snedata/reference/show_mnist_digit.md).

For more information about the original dataset see
<https://yann.lecun.com/exdb/mnist/>.

## Arguments

- base_url:

  Base URL that the MNIST files are located at.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

- as:

  Return format. Use `"data.frame"` for the original data frame shape,
  or `"list"` for the canonical image result.

- timeout:

  Minimum download timeout in seconds. The default is 30 minutes; a
  larger existing global R timeout is preserved.

## Value

If `as = "data.frame"`, a data frame containing the MNIST digits. If
`as = "list"`, a canonical image result with an integer matrix in `data`
and factor digit labels in `meta$label`.

## Details

Downloads the image and label files for the training and test datasets
from the <https://github.com/fgnt/mnist> mirror of the original MNIST
files and converts them to a data frame or canonical list result.

## Note

Originally based on a function by Brendan O'Connor.

## Canonical list results

`as = "list"` returns a shallow list with `data` (one image per row),
`meta` (one metadata row per image), `image_dim`, `channel_order`, and
`source`. Metadata uses lower-case invariant names when applicable:
`label`, `description`, `split`, `id`, `object`, and `pose`;
dataset-specific fields are retained in `meta`. `split` is explicit, so
train/test identity does not depend on row position. `source` records
the dataset and acquisition URL.

## Examples

``` r
if (FALSE) { # \dontrun{
# download the MNIST data set
mnist <- download_mnist()

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
  labels = mnist_r1000$Label,
  col = rainbow(length(levels(mnist$Label)))[mnist_r1000$Label]
)
} # }
```
