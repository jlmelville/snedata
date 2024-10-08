% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ng20.R
\name{download_twenty_newsgroups}
\alias{download_twenty_newsgroups}
\title{Download 20 Newsgroups}
\format{
A data frame with 5 variables:

\describe{
\item{\code{Id}}{A unique identifier for the document, consisting of the
 subset concatenated with the position in the subset, e.g. \code{train_1}.}
\item{\code{FileId}}{The integer identifier of the document, from the
 filename of the downloaded data. Be aware that these are \emph{not} unique.}
\item{\code{Text}}{The full text of the message including any header, footer,
 and quotes. Newlines are preserved.}
\item{\code{Subset}}{A factor with two levels: \code{train} and \code{test},
 indicating whether the document is from the training or test subset.}
\item{\code{Label}}{The newsgroup represented by an integer id, in the range
 0-19.}
\item{\code{Newsgroup}}{A factor with 20 levels, indicating the newsgroup
 that the document belongs to.}
}

The labels correspond to:
\describe{
  \item{\code{0}}{alt.atheism}
  \item{\code{1}}{comp.graphics}
  \item{\code{2}}{comp.os.ms-windows.misc}
  \item{\code{3}}{comp.sys.ibm.pc.hardware}
  \item{\code{4}}{comp.sys.mac.hardware}
  \item{\code{5}}{comp.windows.x}
  \item{\code{6}}{misc.forsale}
  \item{\code{7}}{rec.autos}
  \item{\code{8}}{rec.motorcycles}
  \item{\code{9}}{rec.sport.baseball}
  \item{\code{10}}{rec.sport.hockey}
  \item{\code{11}}{sci.crypt}
  \item{\code{12}}{sci.electronics}
  \item{\code{13}}{sci.med}
  \item{\code{14}}{sci.space}
  \item{\code{15}}{soc.religion.christian}
  \item{\code{16}}{talk.politics.guns}
  \item{\code{17}}{talk.politics.mideast}
  \item{\code{18}}{talk.politics.misc}
  \item{\code{19}}{talk.religion.misc}
}

and are also present as the \code{Newsgroup} factor.

There are 11,314 items in the \code{train} dataset and 7,532 items in the
\code{test} for a total of 18,846 items if you choose \code{subset = "all"}.
}
\usage{
download_twenty_newsgroups(
  subset = "all",
  verbose = FALSE,
  tmpdir = NULL,
  cleanup = TRUE
)
}
\arguments{
\item{subset}{A string specifying which subset of the dataset to download and
process. Acceptable values are \code{"train"} for the training set,
\code{"test"} for the test set, and \code{"all"} for both sets combined.
Default is \code{"all"}.}

\item{verbose}{If \code{TRUE}, log progress of download, extraction and
processing.}

\item{tmpdir}{A string specifying the directory where the dataset will be
downloaded and extracted. If \code{NULL} (default), a temporary directory
is used. If a path is provided and does not exist, it will be created.}

\item{cleanup}{A logical flag indicating whether to delete the downloaded and
extracted files after processing. If \code{TRUE} \emph{and} \code{tmpdir}
was created by this function, \code{tmpdir} will be deleted after
processing. Default is \code{FALSE}.}
}
\value{
Data frame containing 20 Newsgroups Data.
}
\description{
Downloads the 20 Newsgroups dataset, which contains approximately 20,000
newsgroup documents from 20 different newsgroups. The distribution is
approximately balanced.
}
\details{
To do any analysis on this text, you will want to use tools from packages
such as \href{https://cran.r-project.org/package=tm}{tm} and
\href{https://cran.r-project.org/package=tidytext}{tidytext}. The files
are read as \code{latin1} encoding, but there can still be some odd control
codes in some of the messages.
}
\examples{
\dontrun{

# Download and process the training set
ng_train <- download_twenty_newsgroups(subset = "train")

# Download and process both training and test sets, with verbose output
ng_all <- download_twenty_newsgroups(subset = "all", verbose = TRUE)

# Download and process the test set, using a specific directory and enabling
# cleanup
ng_test <- download_twenty_newsgroups(
  subset = "test",
  tmpdir = "path/to/dir", cleanup = TRUE
)
}

}
\references{
Lang, K. (1995).
Newsweeder: Learning to filter netnews.
In \emph{Proceedings of the Twelfth International Conference on Machine Learning} 1995 (pp. 331-339).
Morgan Kaufmann.
}
\seealso{
\url{http://qwone.com/~jason/20Newsgroups/}

Chapter 9 of \href{https://www.tidytextmining.com/usenet}{Tidy Text Mining with R}
for a case study using the same dataset.
}
