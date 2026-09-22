test_that("reusable bootstrap records draws, failures, and configurable intervals", {
  set.seed(61)
  n <- 70
  z <- rnorm(n)
  data <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a2 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a3 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5)
  )
  fit <- fit_states(specify_measurement(A = ordinal("a1", "a2", "a3"), folds = 3),
    data, seed = 4, iterations = 2, diagnostics = FALSE)
  statistic <- function(context) c(mean_A = mean(context$scores$A), n = nrow(context$scores))
  boot <- bootstrap_model(fit, statistic, reps = 8, level = .90, seed = 19)

  expect_s3_class(boot, "cssem_bootstrap")
  expect_equal(dim(boot$draws), c(8L, 2L))
  expect_equal(boot$level, .90)
  expect_equal(boot$successful_replicates, 8L)
  expect_true(all(boot$replicates$status == "success"))
  expect_true(all(c("estimate", "ci_low", "ci_high", "level", "successful_replicates",
    "failure_count") %in% names(boot$summary)))
  intervals <- confint(boot)
  expect_equal(dim(intervals), c(2L, 2L))
  expect_equal(colnames(intervals), c("5 %", "95 %"))

  repeated <- bootstrap_model(fit, statistic, reps = 8, level = .90, seed = 19)
  expect_equal(boot$draws, repeated$draws)
  expect_equal(boot$replicates, repeated$replicates)

  failing <- function(context) {
    if (context$replicate > 0L && context$replicate %% 2L == 0L)
      stop("intentional replicate failure")
    c(mean_A = mean(context$scores$A))
  }
  failed <- bootstrap_model(fit, failing, reps = 6, seed = 21)
  expect_true(failed$failure_count >= 2L)
  expect_true(any(grepl("intentional replicate failure", failed$replicates$failure_reason, fixed = TRUE)))
  expect_true(all(is.na(failed$draws[failed$replicates$status == "failed", , drop = FALSE])))
})

test_that("bootstrap measurement refits and resumability are explicit", {
  set.seed(62)
  n <- 55
  z <- rnorm(n)
  data <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a2 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a3 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5)
  )
  fit <- fit_states(specify_measurement(A = ordinal("a1", "a2", "a3"), folds = 3),
    data, seed = 4, iterations = 1, diagnostics = FALSE)
  statistic <- function(context) c(mean_A = mean(context$scores$A), refit = as.numeric(context$refit == "measurement"))
  partial <- bootstrap_model(fit, statistic, reps = 3, refit = "measurement", seed = 25)
  expect_identical(partial$refit, "measurement")
  expect_true(all(partial$replicates$status == "success"))
  resumed <- bootstrap_model(fit, statistic, reps = 5, refit = "measurement", seed = 25, resume = partial)
  expect_equal(resumed$draws[seq_len(3L), , drop = FALSE], partial$draws)
  expect_equal(nrow(resumed$replicates), 5L)
})

test_that("bootstrap refits remap explicit measurement splits", {
  data <- simulate_states(n = 54, seed = 63)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3)
  split <- make_splits(data, method = "random", folds = 3, seed = 7)
  fit <- fit_states(model, data, split = split, iterations = 1, diagnostics = FALSE)
  statistic <- function(context) c(mean_A = mean(context$scores$A))
  boot <- bootstrap_model(fit, statistic, reps = 4, refit = "measurement", seed = 26)
  expect_true(all(boot$replicates$status == "success"))

  data[3, "a1"] <- NA
  listwise <- fit_states(model, data, split = split, missing_policy = "listwise",
    iterations = 1, diagnostics = FALSE)
  updated <- update(listwise, iterations = 1, diagnostics = FALSE)
  expect_equal(nrow(updated$data), nrow(listwise$data))
})

test_that("cluster bootstrap samples complete unequal units and retains draw IDs", {
  data <- simulate_states(n = 54, seed = 907, missing = 0)
  data$subject <- rep(seq_len(12), times = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 18))
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3L)
  fit <- fit_states(model, data, cluster = "subject", iterations = 1L, diagnostics = FALSE)
  statistic <- function(context) c(rows = nrow(context$data), units = length(unique(context$cluster_ids)))

  set.seed(9907)
  before <- .Random.seed
  boot <- bootstrap_model(fit, statistic, reps = 6L, seed = 37L,
    resample = "cluster", cluster = "subject")
  after <- .Random.seed

  expect_identical(before, after)
  expect_identical(boot$resample, "cluster")
  expect_equal(boot$original_unit_n, 12L)
  expect_true(all(boot$replicates$resampled_unit_n <= boot$original_unit_n))
  expect_true(any(boot$replicates$resampled_unit_n < boot$original_unit_n))
  expect_true(all(boot$replicates$resampled_draw_n == 12L))
  expect_true(all(boot$replicates$resampled_row_n == boot$draws[, "rows"]))
  expect_true(all(vapply(boot$replicates$draw_unit_ids, function(x) length(x) == 12L, logical(1))))
  expect_true(all(vapply(seq_len(nrow(boot$replicates)), function(i)
    length(boot$replicates$row_draw_ids[[i]]) == boot$replicates$resampled_row_n[[i]], logical(1))))
  expect_true(any(vapply(boot$replicates$source_unit_ids, function(x) anyDuplicated(x) > 0L, logical(1))))
  expect_true(all(vapply(boot$replicates$source_unit_ids, function(x) length(x) == 12L, logical(1))))
  expect_true(all(vapply(boot$replicates$rows_per_unit, function(x) length(x) == 12L, logical(1))))
  expect_true(all(boot$draws[, "units"] <= boot$replicates$resampled_unit_n))
})

test_that("cluster bootstrap keeps source units together and honors explicit labels", {
  data <- simulate_states(n = 48, seed = 909, missing = 0)
  data$subject <- rep(seq_len(16), each = 3)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3L)
  fit <- fit_states(model, data, iterations = 1L, diagnostics = FALSE)
  statistic <- function(context) {
    if (context$replicate == 0L)
      return(c(point_units = length(unique(context$cluster_ids)), source_leak = 0))
    folds <- context$fit$measurement_split$assignment
    source_folds <- tapply(folds, context$cluster_ids, function(x) length(unique(x)))
    c(point_units = length(unique(context$cluster_ids)), source_leak = max(source_folds))
  }
  boot <- bootstrap_model(fit, statistic, reps = 3L, refit = "measurement",
    resample = "cluster", cluster = "subject", seed = 38L)
  expect_equal(unname(boot$point_estimate[["point_units"]]), 16)
  expect_true(all(boot$draws[, "source_leak"] == 1))
  expect_equal(boot$replicates$resampled_draw_n, rep(16L, 3L))
})

test_that("cluster bootstrap is deterministic and supports measurement refits", {
  data <- simulate_states(n = 48, seed = 908, missing = 0)
  data$subject <- rep(seq_len(16), each = 3)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3L)
  fit <- fit_states(model, data, cluster = data$subject, iterations = 1L, diagnostics = FALSE)
  statistic <- function(context) c(mean_A = mean(context$scores$A),
    cluster_mode = as.numeric(identical(context$refit, "measurement")))
  first <- bootstrap_model(fit, statistic, reps = 4L, refit = "measurement",
    resample = "cluster", seed = 38L)
  second <- bootstrap_model(fit, statistic, reps = 4L, refit = "measurement",
    resample = "cluster", seed = 38L)
  expect_equal(first$draws, second$draws)
  expect_equal(first$replicates, second$replicates)
  expect_true(all(first$replicates$status == "success"))
  expect_true(any(grepl("cluster_resampling_only", capture.output(print(first)), fixed = TRUE)))
})
