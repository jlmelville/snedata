# Download Small NORB

Download Small NORB database of images of toys.

## Usage

``` r
download_norb_small(
  base_url = "https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/",
  verbose = FALSE,
  split = c("all", "training", "testing"),
  as = c("data.frame", "matrix")
)
```

## Format

A data frame with 18,439 variables:

- `c0px1`, `c0px2`, `c0px3` ... `c0px9216`: Integer pixel value, from 0
  (white) to 255 (black) for the first image in the pair

- `c1px1`, `c1px2`, `c1px3` ... `c1px9216`: Integer pixel value, from 0
  (white) to 255 (black) for the second image in the pair

- `Instance`: The index of the toy in a particular category, represented
  by a factor in the range 0-9. The training set consists of instances
  0, 1, 2, 3 and 5, and the test set consists of 4, 6, 7, 8 and 9.

- `Elevation`: The elevation of the camera represented by a factor in
  the range 0-8. These represent elevations of 30 to 70 degrees from the
  horizontal, in increments of 5 degrees.

- `Azimuth`: The azimuth, represented by a factor in the range 0, 2, 4
  .. 34. Multiply by ten to get the value in degrees.

- `Lighting`: The lighting condition, represened by a factor in the
  range 0-5.

- `Label`: The toy category, represented by a factor in the range 0-4.

- `Split`: Whether the toy in is in the `training` or `testing` set,
  represented by a factor

- `Description`: The name of the toy category associated with `Label`,
  represented by a factor.

The pixel features are organized row-wise from the top left of each
image. The `Label` levels correspond to:

- `0`: Four-legged animal

- `1`: Human figure

- `2`: Airplane

- `3`: Truck

- `4`: Car

There are 48,600 items in the data set. The first 24,300 are the
training set, and the remaining 24,300 are the testing set, but you can
also use the `Split` column to determine which split a given row is in.

Items in the dataset can be visualized with the
[`show_norb_object()`](https://jlmelville.github.io/snedata/reference/show_norb_object.md)
function.

For more information see
<https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/>.

## Arguments

- base_url:

  Base URL that the files are located at.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

- split:

  Which split to download. Use `"all"` for both the training and testing
  sets, or `"training"` or `"testing"` for one split.

- as:

  Return format. Use `"data.frame"` for the original data frame shape,
  or `"matrix"` for a list with `data` and `meta`.

## Value

If `as = "data.frame"`, a data frame containing the Small NORB dataset.
If `as = "matrix"`, a list with `data`, an integer matrix with one image
pair per row, and `meta`, a data frame with the non-pixel metadata
columns.

## Details

Downloads the image and label files for the training and test datasets
and converts them to a data frame or a matrix/list result.

The Small NORB dataset contains images of 50 toys. The toys are divided
into five categories (animal, human, airplane, truck, car) with ten
examples per category. Each object was then images under 6 different
lighting conditions, 9 elevations and 18 different azimuths, so there
are 972 images per toy. The process was then repeated with a different
camera, so there are actually 1,944 images per toy. This dataset stores
each pair of images for a given toy, lighting, elevation and azimuth as
a single row. Each image is 96 by 96 pixels, so the first 9,216 columns
contain the pixels of the first image, and the second 9,216
(`9217:18432`) columns contain the pixels of the second image. The other
information (lighting and so on) are also stored as factors.

## References

The Small NORB Dataset, v1.0
<https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/>

LeCun, Y., Huang, F. J., & Bottou, L. (2004, June). Learning methods for
generic object recognition with invariance to pose and lighting. In
*IEEE Computer Society Conference on Computer Vision and Pattern
Recognition (CVPR) 2004* (pp. 97-104). IEEE.
<http://doi.ieeecomputersociety.org/10.1109/CVPR.2004.144>

## Examples

``` r
if (FALSE) { # \dontrun{
# download the data set
norb <- download_norb_small(verbose = TRUE)

# first 24,300 instances are the training set
norb_train <- head(norb, 24300)
# the remaining 24,300 are the test set
norb_test <- tail(norb, 24300)

# Or equivalently
norb_train2 <- norb[norb$Split == "training", ]
norb_test2 <- norb[norb$Split == "testing", ]

identical(norb_train, norb_train2) # TRUE
identical(norb_test, norb_test2) # also TRUE

# PCA on 1000 examples
norb_r1000 <- norb[sample(nrow(norb), 1000), ]
pca <- prcomp(norb_r1000[, 1:(96 * 96 * 2)], retx = TRUE, rank. = 2)
# plot the scores of the first two components
plot(pca$x[, 1:2], type = "n")
text(pca$x[, 1:2],
  labels = norb_r1000$Label,
  col = rainbow(length(levels(norb$Label)))[norb_r1000$Label]
)
} # }
```
