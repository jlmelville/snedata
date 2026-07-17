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
#' * `Id`: A stable source-derived identifier containing the subset,
#'   newsgroup, and original filename, e.g. `train/alt.atheism/49960`.
#' * `FileId`: The original source filename as a character value. These are
#'   *not* unique across subsets and newsgroups.
#' * `Text`: The full text of the message including any header, footer,
#'   and quotes. Newlines are preserved.
#' * `Subset`: A factor with two levels: `train` and `test`,
#'   indicating whether the document is from the training or test subset.
#' * `Label`: A factor with levels `0` through `19`, identifying the
#'   newsgroup using the canonical ordering below.
#' * `Newsgroup`: A factor with the 20 newsgroups as levels in the canonical
#'   ordering below.
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
#' `Newsgroup` contains the corresponding names.
#'
#' There are 11,314 items in the `train` dataset and 7,532 items in the
#' `test` for a total of 18,846 items if you choose `subset = "all"`.
#' @param subset A string specifying which subset of the dataset to download and
#'   process. Acceptable values are `"train"` for the training set,
#'   `"test"` for the test set, and `"all"` for both sets combined.
#'   Default is `"all"`.
#' @param tmpdir A string specifying the parent directory for a dedicated
#'   download and extraction work directory. If `NULL` (default), a temporary
#'   parent directory is used. If a path is provided and does not exist, it will
#'   be created.
#' @param cleanup A logical flag indicating whether to delete the downloaded and
#'   extracted files after processing. If `TRUE` *and* `tmpdir`
#'   was created by this function, `tmpdir` will be deleted after
#'   processing. Default is `TRUE`.
#' @param verbose If `TRUE`, log progress of download, extraction and
#'   processing.
#' @param timeout Minimum download timeout in seconds. The default is 30
#'   minutes; a larger existing global R timeout is preserved.
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
download_twenty_newsgroups <- function(
  subset = "all",
  verbose = FALSE,
  tmpdir = NULL,
  cleanup = TRUE,
  timeout = 1800
) {
  subset <- match.arg(subset, choices = c("train", "test", "all"))
  temp_info <- setup_temp_directory(tmpdir)
  tmpdir <- temp_info$tmpdir
  workdir <- tempfile("20news-", tmpdir = tmpdir)
  dir.create(workdir)

  if (cleanup) {
    on.exit(cleanup_owned_paths(workdir, verbose = verbose), add = TRUE)
    if (temp_info$created_tmpdir) {
      on.exit(cleanup_owned_paths(tmpdir, verbose = verbose), add = TRUE)
    }
  }

  download_twenty_newsgroups_data(workdir, verbose, timeout = timeout)
  result <- read_newsgroups_data(workdir, subset, verbose)
  validate_newsgroups_dataset(result, subset)
  result
}

newsgroups_url <- "http://qwone.com/~jason/20Newsgroups/20news-bydate.tar.gz"

newsgroup_levels <- c(
  "alt.atheism",
  "comp.graphics",
  "comp.os.ms-windows.misc",
  "comp.sys.ibm.pc.hardware",
  "comp.sys.mac.hardware",
  "comp.windows.x",
  "misc.forsale",
  "rec.autos",
  "rec.motorcycles",
  "rec.sport.baseball",
  "rec.sport.hockey",
  "sci.crypt",
  "sci.electronics",
  "sci.med",
  "sci.space",
  "soc.religion.christian",
  "talk.politics.guns",
  "talk.politics.mideast",
  "talk.politics.misc",
  "talk.religion.misc"
)

download_twenty_newsgroups_data <- function(
  tmpdir = NULL,
  verbose = FALSE,
  url = newsgroups_url,
  timeout = 1800
) {
  if (is.null(tmpdir)) {
    tmpdir <- tempdir()
  }
  if (!dir.exists(tmpdir)) {
    dir.create(tmpdir, recursive = TRUE)
  }

  tarfile <- tempfile("20news-bydate-", fileext = ".tar.gz", tmpdir = tmpdir)
  on.exit(cleanup_owned_paths(tarfile, verbose = verbose), add = TRUE)

  download_asset(url, tarfile, verbose = verbose, timeout = timeout)
  extract_tar_safely(
    tarfile,
    tmpdir,
    asset = "20 Newsgroups archive",
    validate_layout = validate_newsgroups_archive_layout
  )
}

validate_newsgroups_archive_layout <- function(entries, asset) {
  roots <- sub("/.*", "", entries)
  expected_roots <- c("20news-bydate-train", "20news-bydate-test")
  unexpected_roots <- setdiff(unique(roots), expected_roots)
  missing_roots <- setdiff(expected_roots, unique(roots))
  invalid_depth <- !grepl(
    "^20news-bydate-(train|test)(/[^/]+){0,2}$",
    entries
  )

  if (
    length(missing_roots) > 0L ||
      length(unexpected_roots) > 0L ||
      any(invalid_depth)
  ) {
    stop(
      asset,
      " has an invalid layout: missing ",
      length(missing_roots),
      " expected roots, unexpected ",
      length(unexpected_roots),
      " roots, and ",
      sum(invalid_depth),
      " invalid paths",
      call. = FALSE
    )
  }
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

  newsgroup_name <- basename(directory_path)
  if (!newsgroup_name %in% newsgroup_levels) {
    stop("Unknown 20 Newsgroups directory: ", newsgroup_name, call. = FALSE)
  }

  file_names <- sort(
    list.files(directory_path, full.names = TRUE),
    method = "radix"
  )

  ids <- character(length(file_names))
  texts <- character(length(file_names))
  for (i in seq_along(file_names)) {
    ids[i] <- basename(file_names[i])
    texts[i] <- extract_text_from_file(file_names[i])
  }

  data.frame(
    FileId = ids,
    Text = texts,
    Newsgroup = factor(newsgroup_name, levels = newsgroup_levels)
  )
}

read_newsgroups_subset <-
  function(root_dir, subset = "train", verbose = FALSE) {
    tsmessage("Fetching subset: ", subset, verbose = verbose)
    subset_dir <-
      file.path(root_dir, paste0("20news-bydate-", subset))
    if (!dir.exists(subset_dir)) {
      stop("Subset directory does not exist: ", subset_dir)
    }

    df_list <- list()
    newsgroup_dirs <- sort(
      list.files(subset_dir, full.names = TRUE),
      method = "radix"
    )
    for (ng_dir in newsgroup_dirs) {
      tsmessage("Processing ", basename(ng_dir), verbose = verbose)
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
    label_values <- match(
      as.character(combined_data$Newsgroup),
      newsgroup_levels
    ) -
      1L
    combined_data$Label <- factor(label_values, levels = 0:19)
    combined_data$Id <- paste(
      combined_data$Subset,
      combined_data$Newsgroup,
      combined_data$FileId,
      sep = "/"
    )

    combined_data <-
      combined_data[, c("Id", "FileId", "Text", "Subset", "Label", "Newsgroup")]
    combined_data
  }

validate_newsgroups_dataset <- function(
  data,
  subset,
  expected_counts = c(train = 11314L, test = 7532L),
  expected_groups = newsgroup_levels
) {
  dataset <- "20 Newsgroups"
  selected_subsets <- if (subset == "all") c("train", "test") else subset

  for (split in selected_subsets) {
    split_data <- data[as.character(data$Subset) == split, , drop = FALSE]
    observed_groups <- sort(unique(as.character(split_data$Newsgroup)))
    missing_groups <- setdiff(expected_groups, observed_groups)
    unexpected_groups <- setdiff(observed_groups, expected_groups)
    if (length(missing_groups) > 0L || length(unexpected_groups) > 0L) {
      stop_dataset_field(
        dataset,
        split,
        "newsgroup",
        allowed = expected_groups,
        observed = observed_groups
      )
    }
    if (nrow(split_data) != expected_counts[[split]]) {
      stop_dataset_field(
        dataset,
        split,
        "row_count",
        allowed = expected_counts[[split]],
        observed = nrow(split_data)
      )
    }
  }

  if (anyDuplicated(data$Id)) {
    duplicate_ids <- unique(data$Id[duplicated(data$Id)])
    stop_dataset_field(
      dataset,
      subset,
      "Id",
      allowed = "unique values",
      observed = duplicate_ids
    )
  }
  invisible(NULL)
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
