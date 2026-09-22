test_that("provenance records serialize configuration without observations", {
  data <- data.frame(x1 = c(1, NA, 3), x2 = c(4, 5, 6))
  rownames(data) <- paste0("respondent-id-", 1:3)
  data$private_note <- "SECRET-CSSEM-ROW-VALUE"
  record <- .cssem_provenance_record(
    "fit_states", quote(fit_states(seed = seed)), list(seed = 19L),
    input = .cssem_provenance_input_summary(data, retained_rows = 1L:2L),
    packages = "MASS"
  )
  expect_identical(unserialize(serialize(record, NULL)), record)
  expect_true(is.character(record$call))
  expect_identical(record$settings$seed, 19L)
  expect_true(nzchar(record$software$R))
  expect_true(nzchar(record$software$cssem))
  expect_true(nzchar(record$software$packages$MASS))
  expect_equal(record$input$n_input, 3L)
  expect_equal(record$input$n_retained, 2L)
  expect_identical(record$input$row_positions, 1:2)
  expect_false(any(c("data", "input_data", "environment") %in% names(record)))
  expect_false(any(c("respondent-id-1", "SECRET-CSSEM-ROW-VALUE") %in%
    unlist(record, recursive = TRUE, use.names = FALSE)))
})

test_that("cssem_provenance distinguishes supported legacy and unsupported objects", {
  legacy <- structure(list(), class = "fit_states")
  expect_null(cssem_provenance(legacy))
  expect_error(cssem_provenance(structure(list(), class = "unrecognized_result")),
    "supported")
})

