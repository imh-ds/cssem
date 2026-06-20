if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop("Install roxygen2 before regenerating package documentation.", call. = FALSE)
}

roxygen2::roxygenise()
