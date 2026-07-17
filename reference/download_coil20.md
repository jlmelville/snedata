# Download COIL-20

Download the processed Columbia Object Image Library COIL-20 dataset.

## Usage

``` r
download_coil20(
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "list"),
  timeout = 1800
)
```

## Format

If `as = "data.frame"`, the default, a data frame with 16,385 variables:

- `x1_y1`, `x1_y2`, ... `x1_y128`, `x2_y1`, ... `x128_y128`: Pixel
  values ranging from 0 to 1. The `x` index increases from left to
  right, and the `y` index increases from top to bottom, so `x1_y1` is
  top left and `x128_y128` is bottom right.

- `Label`: The object id, stored as a factor with levels 1 to 20.

Row names are `"<object>_<pose>"`, where `<object>` is the object id and
`<pose>` is the pose id, from 0 to 71.

## Arguments

- file:

  File path to download the ZIP archive to. If `NULL`, a file in a
  temporary work directory is used.

- cleanup:

  If `TRUE`, delete temporary download and extraction files before
  returning. If `file` is supplied, only the downloaded ZIP and the
  dedicated extraction directory are removed.

- verbose:

  If `TRUE`, log download and extraction progress.

- as:

  Return format. Use `"data.frame"` for the original wide data frame
  shape, or `"list"` for the canonical image result described in
  [`download_mnist()`](https://jlmelville.github.io/snedata/reference/download_mnist.md).

- timeout:

  Minimum download timeout in seconds. The default is 30 minutes; a
  larger existing global R timeout is preserved.

## Value

If `as = "data.frame"`, a data frame containing the COIL-20 dataset. If
`as = "list"`, a canonical image result with object ids, poses, and
factor labels in `meta`.

## Details

Downloads and expands the processed COIL-20 ZIP archive and converts the
PNG files to a data frame or canonical list result. COIL-20 contains 20
objects, each represented by 72 grayscale poses. Each image has
resolution 128 x 128.

The optional [png](https://cran.r-project.org/package=png) package is
required when reading the downloaded PNG files.

## References

The Columbia Object Image Library COIL-20
<https://cave.cs.columbia.edu/repository/COIL-20>

## Examples

``` r
if (FALSE) { # \dontrun{
coil20 <- download_coil20(verbose = TRUE)
show_coil_object(coil20, object = 5, pose = 4)
} # }
```
