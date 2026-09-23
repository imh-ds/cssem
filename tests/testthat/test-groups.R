test_that("measurement invariance keeps groups on the pooled score scale", {
  d <- simulate_states(n = 120, missing = 0, seed = 101)
  d$group <- rep(c("control", "treated"), each = 60)
  d$a1[d$group == "treated"] <- pmin(d$a1[d$group == "treated"] + 1L, 4L)
  model <- specify_measurement(
    A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3
  )
  fit <- fit_states(model, d, seed = 4, iterations = 2, diagnostics = FALSE)

  out <- measurement_invariance(fit, "group", reference = "control", min_group_size = 50)

  expect_s3_class(out, "cssem_measurement_invariance")
  invariance_provenance <- expect_cssem_provenance(out, "measurement_invariance", "fit_states")
  expect_false(any(c("control", "treated") %in%
    unlist(invariance_provenance, recursive = TRUE, use.names = FALSE)))
  expect_equal(out$status, "diagnostic_common_anchor")
  expect_equal(out$groups$n, c(60L, 60L))
  expect_true(all(c("construct", "group", "mean", "sd") %in% names(out$constructs)))
  expect_true(all(c("reference_mean", "difference") %in% names(out$constructs)))
  expect_true(any(out$item_contrasts$item == "a1"))
  expect_true(any(out$item_contrasts$parameter == "threshold_1"))
  expect_true(any(out$item_contrasts$parameter == "discrimination"))
  expect_true(all(out$item_contrasts$reference == "control"))
  expect_match(paste(out$limitations, collapse = " "), "common-anchor")
})

test_that("missing pooled ordinal categories are unavailable rather than pseudocounted", {
  d <- simulate_states(n = 80, missing = 0, seed = 106)
  d$group <- rep(c("A", "B"), each = 40)
  d$a1[d$group == "B"] <- 1L
  fit <- fit_states(specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3),
    d, seed = 14, iterations = 1, diagnostics = FALSE)
  out <- measurement_invariance(fit, "group", reference = "A", min_group_size = 20)
  rows <- out$item_parameters[out$item_parameters$item == "a1" & out$item_parameters$group == "B", , drop = FALSE]
  expect_true(all(!rows$available))
  expect_true(all(grepl("categor", rows$availability_reason, ignore.case = TRUE)))
})

test_that("measurement invariance resolves filtered rows and flags small groups", {
  d <- simulate_states(n = 72, missing = .10, seed = 102)
  d$group <- rep(c("A", "B", "C"), each = 24)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3)
  fit <- fit_states(model, d, seed = 5, iterations = 1, diagnostics = FALSE,
    missing_policy = "listwise")

  expect_warning(out <- measurement_invariance(fit, d$group, min_group_size = 30),
    "below min_group_size")
  expect_equal(sum(out$groups$n), nrow(fit$locked_scores))
  expect_true(any(out$groups$small_group))
  expect_error(measurement_invariance(fit, rep(NA_character_, nrow(d))), "missing")
  expect_error(measurement_invariance(fit, "not_a_column"), "column")
})

test_that("manifest constructs remain score-comparable but have unavailable item parameters", {
  d <- data.frame(m = seq_len(48) / 10, group = rep(c("A", "B"), each = 24))
  fit <- fit_states(specify_measurement(M = manifest("m"), folds = 3), d,
    seed = 9, iterations = 1, diagnostics = FALSE)
  out <- measurement_invariance(fit, "group", min_group_size = 20)
  expect_equal(unique(out$item_parameters$parameter), "manifest_scale")
  expect_true(all(!out$item_parameters$available))
  expect_true(all(is.na(out$item_contrasts$estimate)))
})

test_that("group structural comparison returns reproducible path contrasts", {
  d <- simulate_states(n = 96, missing = 0, seed = 103)
  d$group <- rep(c("A", "B"), each = 48)
  model <- specify_measurement(
    A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3
  )
  fit <- fit_states(model, d, seed = 6, iterations = 2, diagnostics = FALSE)
  association <- associate(fit, specify_structure(B ~ linear(A), order = c("A", "B")),
    structural_repeats = 1, seed = 8)
  group_labels <- ifelse(d$group == "A", "cohort-private-alpha", "cohort-private-beta")

  first <- group_comparison(association, group_labels, reference = "cohort-private-alpha", permutations = 9, seed = 11,
    min_group_size = 30)
  second <- group_comparison(association, group_labels, reference = "cohort-private-alpha", permutations = 9, seed = 11,
    min_group_size = 30)

  expect_s3_class(first, "cssem_group_comparison")
  group_provenance <- expect_cssem_provenance(first, "group_comparison", "associate")
  expect_false(any(c("cohort-private-alpha", "cohort-private-beta") %in%
    unlist(group_provenance, recursive = TRUE, use.names = FALSE)))
  expect_equal(first$status, "associational_group_contrast")
  expect_equal(first$contrasts, second$contrasts)
  expect_true(any(first$contrasts$outcome == "B" & first$contrasts$predictor == "A"))
  expect_true(all(first$contrasts$reference == "cohort-private-alpha"))
  expect_true(any(first$contrasts$available))
  expect_true(any(is.finite(first$contrasts$difference)))
  expect_true(all(c("null_low", "null_high", "n_group", "n_reference") %in% names(first$contrasts)))
  expect_true(all(first$permutation_settings$permutations == 9L))
})

test_that("multi-predictor group contrasts use the same adjusted estimator", {
  d <- simulate_states(n = 120, missing = 0, seed = 107)
  d$c1 <- pmin(pmax(d$b1 + sample(c(-1L, 0L, 1L), nrow(d), replace = TRUE), 1L), 4L)
  d$c2 <- pmin(pmax(d$b2 + sample(c(-1L, 0L, 1L), nrow(d), replace = TRUE), 1L), 4L)
  d$c3 <- pmin(pmax(d$b3 + sample(c(-1L, 0L, 1L), nrow(d), replace = TRUE), 1L), 4L)
  d$c4 <- pmin(pmax(d$b4 + sample(c(-1L, 0L, 1L), nrow(d), replace = TRUE), 1L), 4L)
  d$group <- rep(c("A", "B"), each = 60)
  model <- specify_measurement(
    A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)),
    C = ordinal(paste0("c", 1:4)), folds = 3
  )
  fit <- fit_states(model, d, seed = 15, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, specify_structure(C ~ linear(A) + linear(B), order = c("A", "B", "C")),
    structural_repeats = 1, shape_alpha = .001, shape_min_gain = 1, seed = 16)
  out <- group_comparison(association, "group", reference = "A", permutations = 5,
    seed = 17, min_group_size = 40)
  rows <- out$contrasts[out$contrasts$outcome == "C" & out$contrasts$available, , drop = FALSE]
  expect_true(nrow(rows) >= 1L)
  expect_equal(rows$difference, rows$estimate - rows$reference_estimate, tolerance = 1e-10)
})

test_that("group comparison rejects invalid references and preserves the RNG stream", {
  d <- simulate_states(n = 60, missing = 0, seed = 104)
  d$group <- rep(c("A", "B"), each = 30)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3)
  fit <- fit_states(model, d, seed = 7, iterations = 1, diagnostics = FALSE)
  before <- { set.seed(88); .Random.seed }
  expect_error(measurement_invariance(fit, "group", reference = "Z"), "reference")
  after <- { set.seed(88); .Random.seed }
  expect_equal(before, after)
})

test_that("permutation inference preserves the caller random stream", {
  d <- simulate_states(n = 72, missing = 0, seed = 105)
  d$group <- rep(c("A", "B"), each = 36)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3)
  fit <- fit_states(model, d, seed = 10, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, specify_structure(B ~ linear(A), order = c("A", "B")),
    structural_repeats = 1, seed = 12)
  set.seed(77); first <- runif(1)
  expected <- runif(1)
  set.seed(77); expect_equal(runif(1), first)
  group_comparison(association, "group", permutations = 5, seed = 13, min_group_size = 20)
  expect_equal(runif(1), expected)
})
