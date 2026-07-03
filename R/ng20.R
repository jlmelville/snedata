#' Download 20 Newsgroups
#'
#' Downloads the 20 Newsgroups dataset, which contains approximately 20,000
#' newsgroup documents from 20 different newsgroups. The distribution is
#' approximately balanced.
#'
#' To do any analysis on this text, you will want to use tools from packages
#' such as [tm](https://cran.r-project.org/package=tm) and
#' [tidytext](https://cran.r-project.org/package=tidytext). The files
#' are read as `latin1` encoding, but there can still be some odd control
#' codes in some of the messages.
#'
#' @format A data frame with 6 variables:
#'
#' * `Id`: A unique identifier for the document, consisting of the
#'   subset concatenated with the position in the subset, e.g. `train_1`.
#' * `FileId`: The integer identifier of the document, from the
#'   filename of the downloaded data. Be aware that these are *not* unique.
#' * `Text`: The full text of the message including any header, footer,
#'   and quotes. Newlines are preserved.
#' * `Subset`: A factor with two levels: `train` and `test`,
#'   indicating whether the document is from the training or test subset.
#' * `Label`: The newsgroup represented by an integer id, in the range
#'   0-19.
#' * `Newsgroup`: A factor with 20 levels, indicating the newsgroup
#'   that the document belongs to.
#'
#' The labels correspond to:
#' * `0`: alt.atheism
#' * `1`: comp.graphics
#' * `2`: comp.os.ms-windows.misc
#' * `3`: comp.sys.ibm.pc.hardware
#' * `4`: comp.sys.mac.hardware
#' * `5`: comp.windows.x
#' * `6`: misc.forsale
#' * `7`: rec.autos
#' * `8`: rec.motorcycles
#' * `9`: rec.sport.baseball
#' * `10`: rec.sport.hockey
#' * `11`: sci.crypt
#' * `12`: sci.electronics
#' * `13`: sci.med
#' * `14`: sci.space
#' * `15`: soc.religion.christian
#' * `16`: talk.politics.guns
#' * `17`: talk.politics.mideast
#' * `18`: talk.politics.misc
#' * `19`: talk.religion.misc
#'
#' and are also present as the `Newsgroup` factor.
#'
#' There are 11,314 items in the `train` dataset and 7,532 items in the
#' `test` for a total of 18,846 items if you choose `subset = "all"`.
#' @param subset A string specifying which subset of the dataset to download and
#'   process. Acceptable values are `"train"` for the training set,
#'   `"test"` for the test set, and `"all"` for both sets combined.
#'   Default is `"all"`.
#' @param tmpdir A string specifying the directory where the dataset will be
#'   downloaded and extracted. If `NULL` (default), a temporary directory
#'   is used. If a path is provided and does not exist, it will be created.
#' @param cleanup A logical flag indicating whether to delete the downloaded and
#'   extracted files after processing. If `TRUE` *and* `tmpdir`
#'   was created by this function, `tmpdir` will be deleted after
#'   processing. Default is `FALSE`.
#' @param verbose If `TRUE`, log progress of download, extraction and
#'   processing.
#' @return Data frame containing 20 Newsgroups Data.
#' @seealso
#' <http://qwone.com/~jason/20Newsgroups/>
#'
#' Chapter 9 of [Tidy Text Mining with R](https://www.tidytextmining.com/usenet)
#' for a case study using the same dataset.
#' @references
#' Lang, K. (1995).
#' Newsweeder: Learning to filter netnews.
#' In *Proceedings of the Twelfth International Conference on Machine Learning* 1995 (pp. 331-339).
#' Morgan Kaufmann.
#' @examples
#' \dontrun{
#'
#' # Download and process the training set
#' ng_train <- download_twenty_newsgroups(subset = "train")
#'
#' # Download and process both training and test sets, with verbose output
#' ng_all <- download_twenty_newsgroups(subset = "all", verbose = TRUE)
#'
#' # Download and process the test set, using a specific directory and enabling
#' # cleanup
#' ng_test <- download_twenty_newsgroups(
#'   subset = "test",
#'   tmpdir = "path/to/dir", cleanup = TRUE
#' )
#' }
#'
#' @export
download_twenty_newsgroups <-
  function(subset = "all", verbose = FALSE, tmpdir = NULL, cleanup = TRUE) {
    temp_info <- setup_temp_directory(tmpdir)
    tmpdir <- temp_info$tmpdir
    created_tmpdir <- temp_info$created_tmpdir

    download_twenty_newsgroups_data(tmpdir, verbose)

    df <- read_newsgroups_data(tmpdir, subset, verbose)

    # Cleanup if required
    if (cleanup && created_tmpdir) {
      tsmessage("Cleaning up temporary files in ", tmpdir)
      unlink(tmpdir, recursive = TRUE)
    }

    df
  }


download_twenty_newsgroups_data <-
  function(
    tmpdir = NULL,
    verbose = FALSE,
    url = "http://qwone.com/~jason/20Newsgroups/20news-bydate.tar.gz"
  ) {
    if (is.null(tmpdir)) {
      tmpdir <- tempdir()
    }
    if (!dir.exists(tmpdir)) {
      dir.create(tmpdir, recursive = TRUE)
    }

    tarfile <- tempfile("20news-bydate-", fileext = ".tar.gz", tmpdir = tmpdir)
    on.exit(unlink(tarfile), add = TRUE)

    tsmessage("Downloading ", url)
    utils::download.file(url, tarfile, quiet = !verbose, mode = "wb")
    tsmessage("Extracting to ", tmpdir)
    utils::untar(tarfile, exdir = tmpdir)
  }

extract_text_from_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  content <- readLines(file_path, warn = FALSE, encoding = "latin1")
  paste(content, collapse = "\n")
}

read_newsgroup_directory <- function(directory_path) {
  if (!dir.exists(directory_path)) {
    stop("Directory does not exist: ", directory_path)
  }

  file_names <- list.files(directory_path, full.names = TRUE)
  newsgroup_name <- basename(directory_path)

  ids <- character(length(file_names))
  texts <- character(length(file_names))
  for (i in seq_along(file_names)) {
    ids[i] <- basename(file_names[i])
    texts[i] <- extract_text_from_file(file_names[i])
  }

  data.frame(
    FileId = ids,
    Text = texts,
    Newsgroup = factor(newsgroup_name, levels = unique(newsgroup_name))
  )
}

read_newsgroups_subset <-
  function(root_dir, subset = "train", verbose = FALSE) {
    tsmessage("Fetching subset: ", subset)
    subset_dir <-
      file.path(root_dir, paste0("20news-bydate-", subset))
    if (!dir.exists(subset_dir)) {
      stop("Subset directory does not exist: ", subset_dir)
    }

    df_list <- list()
    newsgroup_dirs <- list.files(subset_dir, full.names = TRUE)
    for (ng_dir in newsgroup_dirs) {
      tsmessage("Processing ", basename(ng_dir))
      df_list[[basename(ng_dir)]] <- read_newsgroup_directory(ng_dir)
    }

    do.call(rbind, df_list)
  }

read_newsgroups_data <-
  function(root_dir, subset = "all", verbose = FALSE) {
    subset_levels <- c("train", "test")
    subset <- match.arg(subset, choices = c(subset_levels, "all"))

    if (subset == "all") {
      train_data <- read_newsgroups_subset(root_dir, "train", verbose)
      train_data$Subset <- factor("train", levels = subset_levels)

      test_data <- read_newsgroups_subset(root_dir, "test", verbose)
      test_data$Subset <- factor("test", levels = subset_levels)

      # Combine both subsets
      combined_data <- rbind(train_data, test_data)
    } else {
      combined_data <- read_newsgroups_subset(root_dir, subset, verbose)
      combined_data$Subset <- factor(subset, levels = subset_levels)
    }
    combined_data$Label <-
      factor((as.integer(combined_data$Newsgroup) - 1))
    combined_data$Id <-
      paste0(
        combined_data$Subset,
        "_",
        stats::ave(
          seq_along(combined_data$Subset),
          combined_data$Subset,
          FUN = seq_along
        )
      )

    combined_data <-
      combined_data[, c("Id", "FileId", "Text", "Subset", "Label", "Newsgroup")]
    combined_data
  }

setup_temp_directory <- function(tmpdir) {
  created_tmpdir <- FALSE

  if (is.null(tmpdir)) {
    tmpdir <- tempfile()
    dir.create(tmpdir)
    created_tmpdir <- TRUE
  } else {
    if (!dir.exists(tmpdir)) {
      dir.create(tmpdir, recursive = TRUE)
      created_tmpdir <- TRUE
    }
  }

  list(tmpdir = tmpdir, created_tmpdir = created_tmpdir)
}
