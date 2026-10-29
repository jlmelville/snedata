# Download Isomap Faces

Download the original Isomap face-pose dataset.

## Usage

``` r
download_isomap_faces(
  url = isomap_faces_url,
  verbose = FALSE,
  timeout = 1800,
  compression = c("compress", "none")
)
```

## Format

A data frame with 698 rows and 4,098 variables:

- `px1`, `px2`, `px3` ... `px4096`: Grayscale pixel values for a 64 x 64
  image.

- `pose1`, `pose2`: Pose values supplied with the original dataset.

## Arguments

- url:

  URL of the Matlab data file. If omitted, uses the uncompressed mirror
  by default. If `compression` is supplied without `url`, uses the
  archived Stanford file instead.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

- timeout:

  Minimum download timeout in seconds. The default is 30 minutes; a
  larger existing global R timeout is preserved.

- compression:

  Compression used by `url`: `"compress"` for Unix `compress` files, or
  `"none"` for uncompressed Matlab files. When omitted, uses `"none"`
  with the default mirror and `"compress"` with a supplied `url`.
  Supplying `"none"` without a `url` selects the compressed archived
  file and will fail; provide the URL of an uncompressed file instead.

## Value

Data frame containing the Isomap face-pose dataset.

## Details

By default, downloads an uncompressed MAT file from a commit-pinned
[Mashimo/datascience](https://github.com/Mashimo/datascience) mirror. On
October 29, 2026, this file matched the decompressed [archived Stanford
file](https://web.archive.org/web/20150922051706id_/http://isomap.stanford.edu/face_data.mat.Z)
byte for byte. To use the archived Unix `compress` file, see the example
below.

No other source is tried if a download fails.

## Note

Requires the [R.matlab](https://cran.r-project.org/package=R.matlab)
package to be installed. When `compression = "compress"`, an external
`gzip` or `uncompress` command is also required.

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

# To use the archived Stanford file instead (requires gzip or uncompress):
faces <- download_isomap_faces(compression = "compress")
} # }
```
