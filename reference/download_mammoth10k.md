# Mammoth 10K

Downloads the 10,000 point 'Mammoth' dataset, a 3D point cloud of a
mammoth skeleton.

## Usage

``` r
download_mammoth10k(timeout = 1800)
```

## Format

A data frame with 10,000 rows and 3 variables, `X`, `Y`, and `Z`. The
source coordinate vectors are ordered X, Z, Y; this function reorders
them to the canonical X, Y, Z order. The Z-coordinate is the "height",
i.e. the height of the mammoth varies with Z.

For more information see
<https://pair-code.github.io/understanding-umap/>.

## Arguments

- timeout:

  Minimum download timeout in seconds. The default is 30 minutes; a
  larger existing global R timeout is preserved.

## Value

Data frame containing the Mammoth coordinates.

## Details

Downloads a data frame containing the 10,000 3D coordinates of a mammoth
skeleton, digitized by [the Smithsonian
Institution](https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12).

This dataset is from [Understanding
UMAP](https://pair-code.github.io/understanding-umap/), based on work
originally done by [Max
Noichl](https://github.com/MNoichl/UMAP-examples-mammoth-). It consists
of 10,000 points randomly sampled from the 50,000 point data set.

## Note

Requires the [rjson](https://cran.r-project.org/package=rjson) package
to be installed.

## See also

- Max Noichl's page:
  <https://github.com/MNoichl/UMAP-examples-mammoth->.

- Understanding UMAP: <https://pair-code.github.io/understanding-umap/>.

- The Smithsonian page:
  <https://3d.si.edu/object/3d/mammuthus-primigenius-blumbach:341c96cd-f967-4540-8ed1-d3fc56d31f12>.

- [`download_mammoth50k()`](https://jlmelville.github.io/snedata/reference/download_mammoth50k.md)
  to download the original 50,000 points, from which this dataset was
  randomly sampled.

## Examples

``` r
if (FALSE) { # \dontrun{
mammoth <- download_mammoth10k()
# The mammoth in profile, facing left
plot(mammoth$X, mammoth$Z)
} # }
```
