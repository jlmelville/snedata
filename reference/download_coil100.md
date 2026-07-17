# Download COIL-100

Download the Columbia Object Image Library COIL-100 dataset.

## Usage

``` r
download_coil100(
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "list"),
  timeout = 1800
)
```

## Format

If `as = "data.frame"`, the default, a data frame with 49,153 variables:

- `r_x1_y1`, `r_x1_y2`, ... `r_x128_y128`: Red-channel pixel values.

- `g_x1_y1`, `g_x1_y2`, ... `g_x128_y128`: Green-channel pixel values.

- `b_x1_y1`, `b_x1_y2`, ... `b_x128_y128`: Blue-channel pixel values.

- `Label`: The object id, stored as a factor with levels 1 to 100.

Pixel values range from 0 to 1. Within each channel, the `x` index
increases from left to right and the `y` index increases from top to
bottom. The numeric pixel matrix uses about 2.64 GiB; the wide
data-frame result needs additional memory. Use `as = "list"` if that
result is sufficient.

Row names are `"<object>_<angle>"`, where `<object>` is the object id
and `<angle>` is the viewing angle in degrees, from 0 to 355 in
five-degree increments.

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

If `as = "data.frame"`, a data frame containing the COIL-100 dataset. If
`as = "list"`, a canonical image result with object ids, viewing angles,
and factor labels in `meta`.

## Details

Downloads and expands the COIL-100 ZIP archive and converts the PNG
files to a data frame or canonical list result. COIL-100 contains 100
objects, each represented by 72 RGB poses at five-degree viewing-angle
increments. Each image has resolution 128 x 128.

The optional [png](https://cran.r-project.org/package=png) package is
required when reading the downloaded PNG files.

## References

The Columbia Object Image Library COIL-100
<https://cave.cs.columbia.edu/repository/COIL-100>

## Examples

``` r
if (FALSE) { # \dontrun{
coil100 <- download_coil100(verbose = TRUE, as = "list")
show_coil_object(coil100, object = 5, pose = 150)
} # }
```
