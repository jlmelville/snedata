# Olivetti Faces dataset

SNE benchmarking data.

## Usage

``` r
olivetti_faces()
```

## Format

A data frame with 400 rows and 4097 variables.

## Value

The Olivetti Faces dataset as a dataframe.

## Details

Returns the Olivetti Faces dataset in a data frame reformatted to have
one face per row, rather than column. This is a series of 400 images
(with dimension 64 x 64) of 40 individual's faces, with ten different
poses per person.

The variables are as follows:

- `px1`, `px2`, `px3` ... `px4096` 8-bit grayscale pixel values (0-255).
  The pixel index starts at the top right of the image (`px1`) and are
  then stored column-wise.

- `Label`: An integer in the range (1-40) indicating the person.

Each row has a name with the format `face_pose`, where `face` is the
index of the face, and `pose` is the index of the pose, e.g. the row
with name `20_10` is the tenth pose of the twentieth face.

## Note

Requires the
[RnavGraphImageData](https://cran.r-project.org/package=RnavGraphImageData)
package to be installed and loaded.

## See also

- Sam Roweis' dataset web page:
  [http://www.cs.nyu.edu/~roweis/data.html](http://www.cs.nyu.edu/~roweis/data.md).

- Each row can be visualized as an image using
  [`show_olivetti_face()`](https://jlmelville.github.io/snedata/reference/show_olivetti_face.md).
