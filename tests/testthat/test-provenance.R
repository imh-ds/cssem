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

test_that("provenance call text can omit inline observations and identifiers", {
  call <- quote(fit_states(model,
    data = data.frame(x = "SECRET-INLINE-OBSERVATION"),
    cluster = c("SECRET-UNIT-A", "SECRET-UNIT-B")))
  record <- .cssem_provenance_record("fit_states",
    .cssem_provenance_call(call, c("data", "cluster")), settings = list())
  expect_false(any(c("SECRET-INLINE-OBSERVATION", "SECRET-UNIT-A", "SECRET-UNIT-B") %in%
    record$call))
  expect_match(record$call, 'data = "<omitted>"')
  expect_match(record$call, 'cluster = "<omitted>"')
})

test_that("fit and association provenance record resolved settings and parents", {
  data <- simulate_states(n = 48, seed = 401, missing = 0)
  rownames(data) <- paste0("respondent-id-", seq_len(nrow(data)))
  data$private_note <- "SECRET-CSSEM-ROW-VALUE"
  data$a1[[1L]] <- NA_integer_
  model <- specify_measurement(
    A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3
  )
  split <- make_splits(data, method = "random", folds = 3, seed = 18)
  fit <- withCallingHandlers(
    fit_states(model, data, seed = 17, iterations = 2, diagnostics = FALSE,
      missing_policy = "listwise", split = split),
    cssem_nonconvergence = function(w) invokeRestart("muffleWarning")
  )
  structure <- specify_structure(B ~ linear(A))
  provenance <- cssem_provenance(fit)
  expect_equal(provenance$settings$seed, 17)
  expect_equal(provenance$input$n_input, 48L)
  expect_equal(provenance$input$n_retained, 47L)
  expect_identical(provenance$input$row_positions, 2:48)
  expect_identical(provenance$settings$model$constructs$A$indicators, paste0("a", 1:4))
  expect_false(any(c("respondent-id-1", "SECRET-CSSEM-ROW-VALUE") %in%
    unlist(provenance, recursive = TRUE, use.names = FALSE)))
  association <- associate(fit, structure, structural_repeats = 1)
  expect_identical(cssem_provenance(association)$parent$fit$operation, "fit_states")
})
