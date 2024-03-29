% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cifar.R
\name{download_cifar10}
\alias{download_cifar10}
\title{Download CIFAR-10}
\format{
A data frame with 3074 variables:

\describe{
\item{\code{r1}, \code{r2}, \code{r3} ... \code{r1024}}{Integer pixel value
of the red channel of the image, from 0 to 255.}
\item{\code{g1}, \code{g2}, \code{g3} ... \code{g1024}}{Integer pixel value
of the green channel of the image, from 0 to 255.}
\item{\code{b1}, \code{b2}, \code{b3} ... \code{b1024}}{Integer pixel value
of the blue channel of the image, from 0 to 255.}
\item{\code{Label}}{The image category, represented by a factor in the
  range 0-9.}
\item{\code{Description}}{The name of the image category associated with
  \code{Label}, represented by a factor.}
}

The pixel features are organized row-wise from the top left of each image.
The \code{Label} levels correspond to the following class names (stored in
the \code{Description} column):
\describe{
  \item{\code{0}}{Airplane}
  \item{\code{1}}{Automobile}
  \item{\code{2}}{Bird}
  \item{\code{3}}{Cat}
  \item{\code{4}}{Deer}
  \item{\code{5}}{Dog}
  \item{\code{6}}{Frog}
  \item{\code{7}}{Horse}
  \item{\code{8}}{Ship}
  \item{\code{9}}{Truck}
}

There are 60,000 items in the data set. The first 50,000 are the training
set, and the remaining 10,000 are the testing set.

Items in the dataset can be visualized with the
\code{\link{show_cifar}} function.

For more information see
\url{https://www.cs.toronto.edu/~kriz/cifar.html}.
}
\usage{
download_cifar10(
  url = "https://www.cs.toronto.edu/~kriz/cifar-10-binary.tar.gz",
  destfile = tempfile(),
  cleanup = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{url}{URL of the CIFAR-10 data.}

\item{destfile}{Filename for where to download the CIFAR-10 tarfile. It will
be untarred and processed in the same directory.}

\item{cleanup}{If \code{TRUE}, then \code{destfile} and the untarred data
will be deleted before the function returns. Only worth setting to
\code{FALSE} to debug problems.}

\item{verbose}{If \code{TRUE}, then download progress will be logged as a
message.}
}
\value{
Data frame containing the CIFAR-10 dataset.
}
\description{
Download CIFAR-10 database of images.
}
\details{
Downloads the image and label files for the training and test datasets and
converts them to a data frame.

The CIFAR-10 dataset contains 60000 32 x 32 color images, divided into ten
different classes, with 6000 images per class.
}
\examples{
\dontrun{
# download the data set
cifar10 <- download_cifar10(verbose = TRUE)

# first 50,000 instances are the training set
cifar10_train <- head(cifar10, 50000)
# the remaining 24,300 are the test set
cifar10_test <- tail(cifar10, 10000)

# PCA on 1000 examples
cifar10_r1000 <- cifar10[sample(nrow(cifar10), 1000), ]
pca <- prcomp(cifar10_r1000[, 1:(32 * 32)], retx = TRUE, rank. = 2)
# plot the scores of the first two components
plot(pca$x[, 1:2], type = "n")
text(pca$x[, 1:2],
  labels = cifar10_r1000$Label,
  col = rainbow(length(levels(cifar10$Label)))[cifar10_r1000$Label]
)
}
}
\references{
The CIFAR-10 dataset
\url{https://www.cs.toronto.edu/~kriz/cifar.html}

Krizhevsky, A., & Hinton, G. (2009).
\emph{Learning multiple layers of features from tiny images}
(Vol. 1, No. 4, p. 7).
Technical report, University of Toronto.
}
