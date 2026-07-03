# Download Isomap Swiss Roll

Download the original Swiss-roll dataset used in Isomap examples.

## Usage

``` r
download_isomap_swiss_roll(url = isomap_swiss_roll_url, verbose = FALSE)
```

## Format

A data frame with 20,000 rows and 6 variables:

- `x`, `y`, `z`:

  Coordinates of the points in the rolled three-dimensional space.

- `u`, `v`:

  Coordinates of the points in the unrolled two-dimensional space.

- `color`:

  A string representing a color in hex format, generated from `u`.

## Arguments

- url:

  URL of the Matlab data file.

- verbose:

  If `TRUE`, then download progress will be logged as a message.

## Value

Data frame containing the Isomap Swiss-roll dataset.

## Details

The live MIT URL for this dataset is no longer available, so the default
URL uses an archived copy from the Internet Archive.

## Note

Requires the [R.matlab](https://cran.r-project.org/package=R.matlab)
package to be installed and an external gzip or uncompress command to be
available on the system path.

## References

Tenenbaum, J. B., de Silva, V., & Langford, J. C. (2000). A global
geometric framework for nonlinear dimensionality reduction. *Science*,
*290*(5500), 2319-2323.

## Examples

``` r
if (FALSE) { # \dontrun{
swiss <- download_isomap_swiss_roll()
plot(swiss$x, swiss$y, col = swiss$color, pch = 20, cex = 0.1)
} # }
```
