# Run the test suite against the package *source* rather than an installed copy,
# so a stale installation can never mask or fake a result. This is the dev-loop
# entry point; `R CMD check` (via tests/testthat.R) remains the release gate.
if (!requireNamespace("pkgload", quietly = TRUE) || !requireNamespace("testthat", quietly = TRUE)) {
  stop("Install pkgload and testthat before running the source test suite.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
