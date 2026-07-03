# Download QMNIST

Download QMNIST database of handwritten digits. This is the same as the
MNIST digits, but with an extended test dataset of 60,000 digits.

## Usage

``` r
download_qmnist(
  base_url = qmnist_url,
  verbose = FALSE,
  as = c("data.frame", "matrix")
)
```

## Format

A data frame with 785 variables:

- `px1`, `px2`, `px3` ... `px784`: Integer pixel value, from 0 (white)
  to 255 (black).

- `Label`: The digit represented by the image, in the range 0-9.

Pixels are organized row-wise. The `Label` variable is stored as a
factor.

There are 120,000 digits in the data set. The first 60,000 are the
training set, as found in the `qmnist-train-images-idx3-ubyte.gz` file.
The remaining 60,000 are the test set, from the
`qmnist-test-images-idx3-ubyte.gz` file.

Items in the dataset can be visualized with
[`show_mnist_digit()`](https://jlmelville.github.io/snedata/reference/show_mnist_digit.md).

For more information see <https://github.com/facebookresearch/qmnist>.

## Arguments

- base_url:

  Base URL that the QMNIST files are located at.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

- as:

  Return format. Use `"data.frame"` for the original data frame shape,
  or `"matrix"` for a list with `data` and `labels`.

## Value

If `as = "data.frame"`, a data frame containing the QMNIST digits. If
`as = "matrix"`, a list with `data`, an integer matrix with one image
per row, and `labels`, a factor of digit labels.

## Details

Downloads the image and label files for the training and test datasets
from <https://github.com/facebookresearch/qmnist> and converts them to a
data frame or a matrix/list result.

## References

Yadav, C., & Bottou, L. (2019, May). Cold Case: The Lost MNIST Digits
*arXiv preprint* *arXiv:1905.10498*.
<https://github.com/facebookresearch/qmnist>

## Examples

``` r
if (FALSE) { # \dontrun{
# download the QMNIST data set
qmnist <- download_qmnist()

# first 60,000 instances are the training set
qmnist_train <- head(qmnist, 60000)
# the remaining 60,000 are the test set
qmnist_test <- tail(qmnist, 60000)
} # }
```
