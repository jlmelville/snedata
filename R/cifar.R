#' Download CIFAR-10
#' 
#' Download CIFAR-10 database of images.
#'
#' Downloads the image and label files for the training and test datasets and
#' converts them to a data frame.
#' 
#' The CIFAR-10 dataset contains 60000 32 x 32 color images, divided into ten
#' different classes, with 6000 images per class.
#'
#' @format A data frame with 3074 variables:
#'
#' \describe{
#' \item{\code{r1}, \code{r2}, \code{r3} ... \code{r1024}}{Integer pixel value
#' of the red channel of the image, from 0 to 255.} 
#' \item{\code{g1}, \code{g2}, \code{g3} ... \code{g1024}}{Integer pixel value
#' of the green channel of the image, from 0 to 255.}
#' \item{\code{b1}, \code{b2}, \code{b3} ... \code{b1024}}{Integer pixel value
#' of the blue channel of the image, from 0 to 255.}
#' \item{\code{Label}}{The image category, represented by a factor in the 
#'   range 0-9.}
#' \item{\code{Description}}{The name of the image category associated with 
#'   \code{Label}, represented by a factor.}
#' }
#'
#' The pixel features are organized row-wise from the top left of each image. 
#' The \code{Label} levels correspond to the following class names (stored in
#' the \code{Description} column):
#' \describe{
#'   \item{\code{0}}{Airplane}
#'   \item{\code{1}}{Automobile}
#'   \item{\code{2}}{Bird}
#'   \item{\code{3}}{Cat}
#'   \item{\code{4}}{Deer}
#'   \item{\code{5}}{Dog}
#'   \item{\code{6}}{Frog}
#'   \item{\code{7}}{Horse}
#'   \item{\code{8}}{Ship}
#'   \item{\code{9}}{Truck}
#' }
#' 
#' There are 60,000 items in the data set. The first 50,000 are the training
#' set, and the remaining 10,000 are the testing set.
#'
#' Items in the dataset can be visualized with the
#' \code{\link{show_cifar}} function.
#'
#' For more information see
#' \url{https://www.cs.toronto.edu/~kriz/cifar.html}.
#'
#' @param url URL of the CIFAR-10 data.
#' @param destfile Filename for where to download the CIFAR-10 tarfile. It will
#'   be untarred and processed in the same directory.
#' @param cleanup If \code{TRUE}, then \code{destfile} and the untarred data
#'  will be deleted before the function returns. Only worth setting to 
#'  \code{FALSE} to debug problems.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing the CIFAR-10 dataset.
#' @export
#' @examples
#' \dontrun{
#' # download the data set
#' cifar10 <- download_cifar10(verbose = TRUE)
#'
#' # first 50,000 instances are the training set
#' cifar10_train <- head(cifar10, 50000)
#' # the remaining 24,300 are the test set
#' cifar10_test <- tail(cifar10, 10000)
#' 
#' # PCA on 1000 examples
#' cifar10_r1000 <- cifar10[sample(nrow(cifar10), 1000), ]
#' pca <- prcomp(cifar10_r1000[, 1:(32 * 32)], retx = TRUE, rank. = 2)
#' # plot the scores of the first two components
#' plot(pca$x[, 1:2], type = 'n')
#' text(pca$x[, 1:2], labels = cifar10_r1000$Label,
#'      col = rainbow(length(levels(cifar10$Label)))[cifar10_r1000$Label])
#'}
#' @references
#' The CIFAR-10 dataset
#' \url{https://www.cs.toronto.edu/~kriz/cifar.html}
#' 
#' Krizhevsky, A., & Hinton, G. (2009). 
#' \emph{Learning multiple layers of features from tiny images}
#' (Vol. 1, No. 4, p. 7). 
#' Technical report, University of Toronto.
#' @export
download_cifar10 <- function(url = "https://www.cs.toronto.edu/~kriz/cifar-10-binary.tar.gz",
                             destfile = tempfile(),
                             cleanup = TRUE,
                             verbose = FALSE) {
  
  if (!endsWith(destfile, "tar.gz")) {
    destfile <- paste0(destfile, ".tar.gz")
  }
  if (verbose) {
    message("Downloading ", url, " to ", destfile)
    utils::flush.console()
  }
  utils::download.file(url, destfile, quiet = !verbose)
  
  exdir <- dirname(destfile)
  utils::untar(tarfile = destfile, exdir = exdir)
 
  # inside exdir is cifar-10-batches-bin
  # inside cifar-10-batches-bin is data_batch_bin.1 .. 10
  # and test_batch.bin
  # and batches.meta.txt
  
  res <- matrix(nrow = 3072, ncol = 60000)
  labels <- rep(0, 60000)
  
  for (i in 1:6) {
    if (i != 6) {
      file <- paste0("data_batch_", i, ".bin")
    }
    else {
      file <- paste0("test_batch.bin")
    }
    path <- base::file.path(exdir, "cifar-10-batches-bin", file)
    
    batch_res <- read_cifar_bin(path, verbose = verbose)
    batch_range <- ((i - 1) * 10000 + 1):(i * 10000)
    res[, batch_range] <- batch_res$images
    labels[batch_range] <- batch_res$labels
  }
  
  res <- t(res)
  colnames(res) <- c(
    paste0("r", 1:(32 * 32)),
    paste0("g", 1:(32 * 32)),
    paste0("b", 1:(32 * 32))
  )
  res <- data.frame(res, Label = as.factor(labels))
  description_levels <- c("airplane", "automobile", "bird", "cat", "deer", 
                          "dog", "frog", "horse", "ship", "truck")
  res$Description <- factor(description_levels[as.numeric(res$Label)],
                            levels = description_levels)
  
  if (cleanup) {
    if (verbose) {
      message("Deleting ", file)
    }
    unlink(file)
    
    if (verbose) {
      message("Deleting ", exdir)
    }
    unlink(exdir, recursive = TRUE)
  }

  res
}

#' Visualize CIFAR-10 image.
#'
#' Display a CIFAR-10 image.
#'
#' @param df Data frame containing the CIFAR-10 dataframe.
#' @param n Row index of the image to display.
#' @param interpolate If \code{TRUE}, use linear interpolation to smooth the
#'   image. This can help when trying to confirm that you really are looking at
#'   a tiny image of a frog.
#' @examples
#' \dontrun{
#' # show the image at position 27001 (it's a plane)
#' show_cifar(cifar10, 27001)
#' # bit easier to see it's a plane
#' show_cifar(cifar10, 27001, interpolate = TRUE)
#' }
#' @export
show_cifar <- function(df, n, interpolate = FALSE) {
  show_cifarv(as.numeric(df[n, 1:3072]), interpolate = interpolate)
}

read_cifar_bin <- function(file, verbose = FALSE) {
  if (verbose) {
    message("Reading ", file)
  }
  f <- base::file(file, "rb")
  
  n_images <- 10000
  n_pixels <- 3072
  images <- matrix(nrow = n_pixels, ncol = n_images)
  labels <- rep(0, n_images)
  
  for (i in 1:n_images) {
    # first byte is a label between 0-9
    labels[i] <- readBin(f, what = "integer", n = 1, size = 1, signed = FALSE, 
            endian = "little")
    # Next 3072 bytes are the red channel pixels, then green channel, then blue
    # 1024 per channel (32 x 32) in row order
    img <- readBin(f, what = "integer", n = n_pixels, size = 1, signed = FALSE, 
            endian = "little")
    if (length(img) != n_pixels) {
      close(f)
      stop("Ran out of data at image ", i)
    }
    images[, i] <- img
  }
  close(f)
  
  list(
    images = images,
    labels = labels
  )
}

show_cifarv <- function(v, x1 = 100, x2 = 250, y1 = 300, y2 = 450,
                        interpolate = FALSE) {
  r <- v[1:1024]
  g <- v[1025:2048]
  b <- v[2049:3072]
  
  img <- grDevices::rgb(r, g, b, maxColorValue = 255)
  dim(img) <- c(32, 32)
  

  graphics::plot(c(x1, x2), c(y1, y2), type = "n", xlab = "", ylab = "",
                 axes = FALSE)
  graphics::rasterImage(t(img), x1, y1, x2, y2, interpolate = interpolate)
}
