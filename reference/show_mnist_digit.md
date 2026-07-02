# Visualize MNIST digit.

Display an MNIST digit as an image the hand-written digit represented by
the nth row in a data frame.

## Usage

``` r
show_mnist_digit(df, n, col = grDevices::gray(1:255/255), ...)
```

## Arguments

- df:

  Data frame containing MNIST digits.

- n:

  Row index of the digit to display.

- col:

  List of colors to use in the display.

- ...:

  Other arguments passed onto the
  [`image`](https://rdrr.io/r/graphics/image.html) function.

## Note

Originally based on a function by Brendan O'Connor, which can be found
at <https://gist.github.com/brendano/39760>.

## Examples

``` r
if (FALSE) { # \dontrun{
# show the fifth digit
mnist <- download_mnist()
show_mnist_digit(mnist, 5)
} # }
```
