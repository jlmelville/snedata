# Frey Faces dataset

Converts Frey Faces Image dataset.

## Usage

``` r
frey_faces()
```

## Format

A data frame with 1965 rows and 561 variables.

## Value

The Frey Faces dataset as a dataframe.

## Details

Returns the Frey Faces dataset in a data frame where each row is one
image. This is a series of 1965 images (with dimension 20 x 28) of
Brendan Frey's face taken from sequential frames of a video.

The variables are as follows:

- `px1`, `px2`, `px3` ... `px560` 8-bit grayscale pixel values (0-255).
  The pixel index starts at the top right of the image (`px1`) and are
  then stored row-wise.

- `color` A string representing a color in hex format. It can be used
  directly with e.g. the `col` parameter in the
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) function. The
  color goes from hsl(0, 50, 50) (red) at frame 1 to hsl(300, 50, 50)
  (purple) at frame 1965 and on a muted rainbow scale.

## Note

Requires the
[RnavGraphImageData](https://cran.r-project.org/package=RnavGraphImageData)
package to be installed and loaded.

## See also

- Sam Roweis' dataset web page:
  [http://www.cs.nyu.edu/~roweis/data.html](http://www.cs.nyu.edu/~roweis/data.md).

- Each row can be visualized as an image using
  [`show_frey_face`](https://jlmelville.github.io/snedata/reference/show_frey_face.md).

## Examples

``` r
if (FALSE) { # \dontrun{
frey <- frey_faces()
# PCA Scores plot, with color indicating the frame index
frey_pca <- prcomp(frey[, -561], retx = TRUE, rank. = 2)
plot(frey_pca$x, col = frey$color, pch = 16, cex = 0.75)
} # }
```
