expect_cssem_provenance <- function(x, operation, parent_operation = NULL) {
  record <- cssem_provenance(x)
  expect_true(is.list(record))
  expect_identical(record$operation, operation)
  if (!is.null(parent_operation)) {
    expect_true(any(vapply(record$parent,
      function(parent) identical(parent$operation, parent_operation), logical(1))))
  }
  invisible(record)
}
