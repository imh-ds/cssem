test_that("random splits are deterministic and auditable", {
  data <- data.frame(id = seq_len(24), value = rnorm(24))
  first <- make_splits(data, method = "random", folds = 4, seed = 9)
  second <- make_splits(data, method = "random", folds = 4, seed = 9)
  expect_s3_class(first, "cssem_splits")
  expect_identical(first$assignment, second$assignment)
  expect_true(all(vapply(first$outer$train_ids, function(x) length(x) > 0, logical(1))))
  expect_true(all(vapply(first$outer$test_ids, function(x) length(x) > 0, logical(1))))
  expect_true(all(mapply(function(train, test) !any(train %in% test),
    first$outer$train_ids, first$outer$test_ids)))
})

test_that("group splits keep every group in one partition", {
  data <- data.frame(group = rep(letters[1:8], each = 3), value = rnorm(24))
  splits <- make_splits(data, method = "group", group = "group", folds = 4, seed = 3)
  group_fold <- tapply(splits$assignment, data$group, function(x) length(unique(x)))
  expect_true(all(group_fold == 1L))
  expect_error(make_splits(transform(data, group = NA_character_),
    method = "group", group = "group"), "missing")
})

test_that("time splits are forward-only", {
  data <- data.frame(time = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 20),
    value = rnorm(20))
  splits <- make_splits(data, method = "time", time = "time", folds = 4)
  expect_true(all(vapply(seq_len(nrow(splits$outer)), function(i) {
    max(data$time[splits$outer$train_ids[[i]]]) < min(data$time[splits$outer$test_ids[[i]]])
  }, logical(1))))
  data$time[1:2] <- data$time[1]
  tied <- make_splits(data[!is.na(data$time), ], method = "time", time = "time", folds = 4)
  expect_length(unique(tied$assignment[1:2]), 1L)
  data$time[5] <- NA
  expect_error(make_splits(data, method = "time", time = "time"), "time")
  data$time[5] <- Inf
  expect_error(make_splits(data, method = "time", time = "time"), "finite")
})

test_that("invalid split inputs fail before fitting", {
  data <- data.frame(x = seq_len(8))
  expect_error(make_splits(data, method = "random", folds = 1), "folds")
  expect_error(make_splits(data, method = "group", group = rep(1:2, each = 3)), "one value per row")
  expect_error(fit_states(specify_measurement(A = ordinal("x1", "x2"), folds = 2),
    data.frame(x1 = 1:8, x2 = 1:8), split = as.list(rep(1:2, each = 4)), diagnostics = FALSE), "split")
})

test_that("fit_states uses an explicit split assignment", {
  data <- simulate_states(n = 60, seed = 2)
  model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
                           B = list(indicators = paste0("b", 1:4), scales = "ordinal")), folds = 3)
  split <- make_splits(data, method = "random", folds = 3, seed = 12)
  fit <- fit_states(model, data, split = split, iterations = 1, diagnostics = FALSE)
  expect_identical(fit$folds, split$assignment)
  expect_identical(fit$measurement_split$row_ids, seq_len(nrow(data)))
  expect_identical(fit$measurement_split$assignment, split$assignment)
})

test_that("explicit assignments follow listwise row filtering", {
  data <- simulate_states(n = 60, seed = 3)
  data[5, "a1"] <- NA
  model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  split <- make_splits(data, method = "random", folds = 3, seed = 13)
  fit <- fit_states(model, data, split = split, missing_policy = "listwise", iterations = 1, diagnostics = FALSE)
  expect_false(5L %in% fit$measurement_split$row_ids)
  expect_equal(length(fit$measurement_split$assignment), nrow(fit$data))
  expect_output(print(fit$measurement_split), "CS-SEM splits")
  expect_error(fit_states(model, data, split = rep(1:2, length.out = 10), iterations = 1, diagnostics = FALSE), "split")
})
