parse_args <- function(args) {
  opts <- list(
    manifest = ".github/source-urls.tsv",
    source_dir = "R"
  )
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--manifest") {
      i <- i + 1L
      opts$manifest <- args[[i]]
    } else if (arg == "--source-dir") {
      i <- i + 1L
      opts$source_dir <- args[[i]]
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1L
  }
  opts
}

manifest_error <- function(...) {
  stop(..., call. = FALSE)
}

read_manifest <- function(path) {
  if (!file.exists(path)) {
    manifest_error("Manifest does not exist: ", path)
  }

  manifest <- utils::read.delim(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required <- c("dataset", "asset", "url")
  missing <- setdiff(required, names(manifest))
  if (length(missing) > 0L) {
    manifest_error(
      "Manifest is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
  if (nrow(manifest) == 0L) {
    manifest_error("Manifest must contain at least one URL row")
  }
  manifest <- manifest[, required, drop = FALSE]
  validate_manifest_rows(manifest)
  manifest
}

validate_manifest_rows <- function(manifest) {
  for (column in names(manifest)) {
    values <- manifest[[column]]
    invalid <- is.na(values) | !nzchar(trimws(values))
    if (any(invalid)) {
      manifest_error(
        "Manifest column '",
        column,
        "' has empty values at row(s): ",
        paste(which(invalid) + 1L, collapse = ", ")
      )
    }
  }

  malformed <- !grepl("^https?://\\S+$", manifest[["url"]])
  if (any(malformed)) {
    manifest_error(
      "Manifest has malformed URL values at row(s): ",
      paste(which(malformed) + 1L, collapse = ", ")
    )
  }

  row_keys <- manifest_key(manifest)
  duplicate_rows <- duplicated(row_keys) | duplicated(row_keys, fromLast = TRUE)
  if (any(duplicate_rows)) {
    manifest_error(
      "Manifest has duplicate dataset/asset rows: ",
      paste(unique(row_keys[duplicate_rows]), collapse = ", ")
    )
  }

  duplicate_urls <- duplicated(manifest[["url"]]) |
    duplicated(manifest[["url"]], fromLast = TRUE)
  if (any(duplicate_urls)) {
    manifest_error(
      "Manifest has duplicate URL values: ",
      paste(unique(manifest[["url"]][duplicate_urls]), collapse = ", ")
    )
  }
}

manifest_key <- function(rows) {
  paste(rows[["dataset"]], rows[["asset"]], sep = " / ")
}

source_package_files <- function(source_dir) {
  files <- file.path(
    source_dir,
    c(
      "util.R",
      "mnist.R",
      "fashion-mnist.R",
      "kuzushiji-mnist.R",
      "qmnist.R",
      "cifar.R",
      "norb.R",
      "coil.R",
      "ng20.R",
      "isomap.R",
      "mammoth.R"
    )
  )
  missing <- files[!file.exists(files)]
  if (length(missing) > 0L) {
    manifest_error(
      "Cannot validate source URL manifest; missing source file(s): ",
      paste(missing, collapse = ", ")
    )
  }

  env <- new.env(parent = baseenv())
  for (file in files) {
    sys.source(file, envir = env)
  }
  env
}

default_arg <- function(env, function_name, arg) {
  fun <- env[[function_name]]
  if (!is.function(fun)) {
    manifest_error("Missing function in package source: ", function_name)
  }
  eval(formals(fun)[[arg]], envir = env)
}

make_row <- function(dataset, asset, url) {
  data.frame(
    dataset = dataset,
    asset = asset,
    url = url,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mnist_asset_rows <- function(dataset, base_url) {
  files <- c(
    "train images" = "train-images-idx3-ubyte.gz",
    "train labels" = "train-labels-idx1-ubyte.gz",
    "test images" = "t10k-images-idx3-ubyte.gz",
    "test labels" = "t10k-labels-idx1-ubyte.gz"
  )
  rows <- lapply(
    names(files),
    function(asset) make_row(dataset, asset, paste0(base_url, files[[asset]]))
  )
  do.call(rbind, rows)
}

qmnist_asset_rows <- function(base_url) {
  files <- c(
    "train images" = "qmnist-train-images-idx3-ubyte.gz",
    "train labels" = "qmnist-train-labels-idx2-int.gz",
    "test images" = "qmnist-test-images-idx3-ubyte.gz",
    "test labels" = "qmnist-test-labels-idx2-int.gz"
  )
  rows <- lapply(
    names(files),
    function(asset) make_row("QMNIST", asset, paste0(base_url, files[[asset]]))
  )
  do.call(rbind, rows)
}

norb_asset_rows <- function(base_url) {
  split_ids <- c(training = "46789", testing = "01235")
  suffixes <- c(
    images = "-dat.mat.gz",
    categories = "-cat.mat.gz",
    metadata = "-info.mat.gz"
  )

  rows <- list()
  for (split in names(split_ids)) {
    file_base <- paste0(
      "smallnorb-5x",
      split_ids[[split]],
      "x9x18x6x2x96x96-",
      split
    )
    for (asset_type in names(suffixes)) {
      rows[[length(rows) + 1L]] <- make_row(
        "Small NORB",
        paste(split, asset_type),
        paste0(base_url, file_base, suffixes[[asset_type]])
      )
    }
  }
  do.call(rbind, rows)
}

expected_source_urls <- function(env) {
  rows <- list(
    mnist_asset_rows("MNIST", default_arg(env, "download_mnist", "base_url")),
    mnist_asset_rows(
      "Fashion-MNIST",
      default_arg(env, "download_fashion_mnist", "base_url")
    ),
    mnist_asset_rows(
      "Kuzushiji-MNIST",
      default_arg(env, "download_kuzushiji_mnist", "base_url")
    ),
    qmnist_asset_rows(default_arg(env, "download_qmnist", "base_url")),
    make_row(
      "CIFAR-10",
      "archive",
      default_arg(env, "download_cifar10", "url")
    ),
    norb_asset_rows(default_arg(env, "download_norb_small", "base_url")),
    make_row("COIL-20", "archive", env$coil_url("coil-20", "coil-20-proc.zip")),
    make_row("COIL-100", "archive", env$coil_url("coil-100", "coil-100.zip")),
    make_row("20 Newsgroups", "archive", env$newsgroups_url),
    make_row(
      "Isomap",
      "swiss roll",
      default_arg(env, "download_isomap_swiss_roll", "url")
    ),
    make_row(
      "Isomap",
      "faces",
      default_arg(env, "download_isomap_faces", "url")
    ),
    make_row(
      "Mammoth",
      "10k",
      env$gh_raw(
        repo = "PAIR-code/understanding-umap",
        filename = "raw_data/mammoth_3d.json"
      )
    ),
    make_row(
      "Mammoth",
      "50k",
      env$gh_raw(
        repo = "PAIR-code/understanding-umap",
        filename = "raw_data/mammoth_3d_50k.json"
      )
    )
  )
  expected <- do.call(rbind, rows)
  rownames(expected) <- NULL
  validate_manifest_rows(expected)
  expected
}

compare_manifest <- function(actual, expected) {
  actual_keys <- manifest_key(actual)
  expected_keys <- manifest_key(expected)
  missing <- setdiff(expected_keys, actual_keys)
  extra <- setdiff(actual_keys, expected_keys)

  if (length(missing) > 0L || length(extra) > 0L) {
    details <- character()
    if (length(missing) > 0L) {
      details <- c(
        details,
        paste0("missing: ", paste(missing, collapse = ", "))
      )
    }
    if (length(extra) > 0L) {
      details <- c(details, paste0("extra: ", paste(extra, collapse = ", ")))
    }
    manifest_error(
      "Manifest URL rows do not match package source: ",
      paste(details, collapse = "; ")
    )
  }

  actual_by_key <- actual[match(expected_keys, actual_keys), , drop = FALSE]
  mismatched <- actual_by_key[["url"]] != expected[["url"]]
  if (any(mismatched)) {
    keys <- expected_keys[mismatched]
    messages <- paste0(
      keys,
      " expected ",
      expected[["url"]][mismatched],
      " but found ",
      actual_by_key[["url"]][mismatched]
    )
    manifest_error(
      "Manifest URL values do not match package source: ",
      paste(messages, collapse = "; ")
    )
  }
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  actual <- read_manifest(opts$manifest)
  env <- source_package_files(opts$source_dir)
  expected <- expected_source_urls(env)
  compare_manifest(actual, expected)
  message(
    "Source URL manifest matches package source defaults: ",
    nrow(actual),
    " URL(s)."
  )
}

main()
