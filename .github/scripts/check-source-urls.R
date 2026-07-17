parse_args <- function(args) {
  opts <- list(
    manifest = ".github/source-urls.tsv",
    timeout = 10L,
    strict = FALSE
  )
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--manifest") {
      i <- i + 1L
      opts$manifest <- args[[i]]
    } else if (arg == "--timeout") {
      i <- i + 1L
      opts$timeout <- as.integer(args[[i]])
    } else if (arg == "--strict") {
      opts$strict <- TRUE
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1L
  }
  if (is.na(opts$timeout) || opts$timeout <= 0L) {
    stop("--timeout must be a positive integer", call. = FALSE)
  }
  opts
}

curl_check <- function(curl, url, method, timeout, user_agent) {
  stdout <- tempfile()
  stderr <- tempfile()
  on.exit(unlink(c(stdout, stderr)), add = TRUE)

  common <- c(
    "--location",
    "--silent",
    "--show-error",
    "--output",
    "/dev/null",
    "--write-out",
    "%{http_code}",
    "--max-time",
    as.character(timeout),
    "--user-agent",
    user_agent
  )

  args <- if (method == "HEAD") {
    c("--head", common, url)
  } else {
    c(
      common,
      "--range",
      "0-0",
      "--max-filesize",
      "1048576",
      url
    )
  }

  file.create(stdout, stderr)
  status <- system2(curl, shQuote(args), stdout = stdout, stderr = stderr)
  http_code <- paste(readLines(stdout, warn = FALSE), collapse = "")
  error <- paste(readLines(stderr, warn = FALSE), collapse = "\n")
  list(
    ok = identical(status, 0L) && grepl("^[23][0-9][0-9]$", http_code),
    status = status,
    http_code = if (nzchar(http_code)) http_code else "000",
    error = error,
    method = method
  )
}

check_url <- function(curl, url, timeout, user_agent) {
  result <- curl_check(curl, url, "HEAD", timeout, user_agent)
  if (isTRUE(result$ok)) {
    return(result)
  }

  fallback <- curl_check(curl, url, "RANGE_GET", timeout, user_agent)
  if (isTRUE(fallback$ok)) {
    return(fallback)
  }

  result$fallback_http_code <- fallback$http_code
  result$fallback_status <- fallback$status
  result$fallback_error <- fallback$error
  result
}

format_result <- function(row, result) {
  status <- if (isTRUE(result$ok)) "ok" else "fail"
  paste(
    status,
    result$method,
    result$http_code,
    row[["dataset"]],
    row[["asset"]],
    row[["url"]],
    sep = "\t"
  )
}

markdown_cell <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("\\|", "&#124;", x)
}

markdown_status <- function(result) {
  if (isTRUE(result$ok)) "OK" else "**FAIL**"
}

markdown_result_row <- function(item) {
  row <- item$row
  result <- item$result
  paste(
    "|",
    markdown_status(result),
    "|",
    markdown_cell(result$method),
    "|",
    markdown_cell(result$http_code),
    "|",
    markdown_cell(row[["dataset"]]),
    "|",
    markdown_cell(row[["asset"]]),
    "|",
    paste0("<", row[["url"]], ">"),
    "|"
  )
}

write_github_summary <- function(results) {
  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (!nzchar(summary_path)) {
    return(invisible(NULL))
  }

  failures <- vapply(results, function(item) !isTRUE(item$result$ok), logical(1))
  n_failures <- sum(failures)
  n_total <- length(results)
  n_ok <- n_total - n_failures

  lines <- c(
    "## Source URL health",
    "",
    paste0("Checked **", n_total, "** source URLs."),
    paste0("Reachable: **", n_ok, "**. Unreachable: **", n_failures, "**."),
    ""
  )

  if (n_failures > 0L) {
    lines <- c(
      lines,
      "### Unreachable URLs",
      "",
      "| Status | Method | HTTP | Dataset | Asset | URL |",
      "| --- | --- | ---: | --- | --- | --- |",
      vapply(results[failures], markdown_result_row, character(1)),
      ""
    )
  }

  lines <- c(
    lines,
    "### All URLs",
    "",
    "| Status | Method | HTTP | Dataset | Asset | URL |",
    "| --- | --- | ---: | --- | --- | --- |"
  )
  for (item in results) {
    lines <- c(lines, markdown_result_row(item))
  }
  write(lines, file = summary_path, append = TRUE)
  invisible(NULL)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  curl <- Sys.which("curl")
  if (!nzchar(curl)) {
    stop("curl is required for source URL checks", call. = FALSE)
  }

  urls <- utils::read.delim(
    opts$manifest,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required <- c("dataset", "asset", "url")
  missing <- setdiff(required, names(urls))
  if (length(missing) > 0L) {
    stop(
      "Manifest is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  user_agent <- paste0(
    "snedata-source-url-health/1.0 ",
    "(https://github.com/jlmelville/snedata)"
  )
  results <- vector("list", nrow(urls))
  for (i in seq_len(nrow(urls))) {
    row <- urls[i, , drop = FALSE]
    result <- check_url(curl, row[["url"]], opts$timeout, user_agent)
    results[[i]] <- list(row = row, result = result)
    cat(format_result(row, result), "\n", sep = "")
  }

  write_github_summary(results)

  failures <- vapply(results, function(item) !isTRUE(item$result$ok), logical(1))
  if (any(failures)) {
    message(sum(failures), " source URL(s) were not reachable.")
    if (opts$strict) {
      quit(status = 1L)
    }
  }
  invisible(results)
}

main()
