# Download Isomap Faces

Download the original Isomap face-pose dataset.

## Usage

``` r
download_isomap_faces(url = isomap_faces_url, verbose = FALSE, timeout = 1800)
```

## Format

A data frame with 698 rows and 4,098 variables:

- `px1`, `px2`, `px3` ... `px4096`: Grayscale pixel values for a 64 x 64
  image.

- `pose1`, `pose2`: Pose values supplied with the original dataset.

## Arguments

- url:

  URL of the compressed Matlab data file.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

- timeout:

  Minimum download timeout in seconds. The default is 30 minutes; a
  larger existing global R timeout is preserved.

## Value

Data frame containing the Isomap face-pose dataset.

## Details

The original Stanford URL for this dataset is no longer available, so
the default URL uses an archived copy from the Internet Archive. The
archived data is stored as a Unix `compress` file, and this function
uses an external `gzip` or `uncompress` command for decompression.

## Note

Requires the [R.matlab](https://cran.r-project.org/package=R.matlab)
package to be installed. The face data also requires an external `gzip`
or `uncompress` command that can decompress Unix `compress` files.

## References

Tenenbaum, J. B., de Silva, V., & Langford, J. C. (2000). A global
geometric framework for nonlinear dimensionality reduction. *Science*,
*290*(5500), 2319-2323.

## See also

Each row can be visualized using
[`show_isomap_face()`](https://jlmelville.github.io/snedata/reference/show_isomap_face.md).

## Examples

``` r
if (FALSE) { # \dontrun{
faces <- download_isomap_faces()
show_isomap_face(faces, 1)
} # }
```
