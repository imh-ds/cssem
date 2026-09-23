test_that("fit locks scores and scoring rejects schema mismatch", {
  d <- simulate_states(n = 60, seed = 3)
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  f <- fit_states(m, d, seed = 4, iterations = 3)
  expect_equal(nrow(f$locked_scores), 60)
  expect_true(all(is.finite(f$locked_scores$A)))
  expect_identical(f$measurement_engine$A$estimator, "marginal_graded_response")
  expect_equal(nrow(residual_diagnostics(f, "A")), 6)
  expect_error(score_states(f, d[paste0("a", 4:1)]))
  expect_error(score_states(f, d[paste0("a", 1:3)]))
})

test_that("fit reports reliability, posterior SD, and respondent information", {
  d <- simulate_states(n = 120, seed = 7)
  m <- cssem_model(list(
    A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
    B = list(indicators = paste0("b", 1:4), scales = "ordinal")
  ), folds = 3)
  f <- fit_states(m, d, seed = 4, iterations = 4, draws = 5L, diagnostics = FALSE)
  expect_true(all(f$reliability > 0 & f$reliability <= 1))
  expect_equal(dim(f$score_posterior_sd), c(120L, 2L))
  expect_equal(dim(f$uncertainty_draws), c(120L, 2L, 5L))
  info <- respondent_information(f)
  expect_equal(nrow(info), 120L)
  expect_true("information_weight" %in% names(info))
  expect_equal(mean(info$information_weight), 1, tolerance = 1e-6)
  card <- construct_card(f, "A")
  expect_true(is.data.frame(card$respondent_information))
  expect_true(card$respondent_information$reliability > 0)
})

test_that("structural validation manifest exposes v0.4 realistic scenarios", {
  screening <- structural_manifest("screening")
  expect_true(all(c("loading", "careless", "skew") %in% names(screening)))
  expect_true(all(c("plateau", "threshold", "diminishing", "low_reliability", "careless", "skewed") %in% screening$scenario))
  expect_equal(screening$loading[screening$scenario == "low_reliability"], .55)
  expect_true(screening$careless[screening$scenario == "careless"] > 0)
})

test_that("cross-fitting retains a rare ordinal category schema", {
  d <- simulate_states(n = 45, seed = 12)
  d$a1 <- 2L
  d$a1[1] <- 1L
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  # A 2-iteration budget cannot converge; only that expected warning is muffled.
  expect_silent(f <- withCallingHandlers(fit_states(m, d, seed = 8, iterations = 2, diagnostics = FALSE),
    cssem_nonconvergence = function(w) invokeRestart("muffleWarning")))
  expect_true(all(is.finite(f$locked_scores$A)))
})

test_that("exploratory presets lighten model and fit defaults", {
  d <- simulate_states(n = 60, seed = 9)
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), preset = "exploratory")
  # The exploratory budget (4 -> an 8-iteration EM cap) is deliberately light and
  # says so rather than failing to converge silently.
  expect_warning(f <- fit_states(m, d, seed = 5, diagnostics = FALSE, preset = "exploratory"),
    class = "cssem_nonconvergence")
  expect_equal(m$folds, 2L)
  expect_equal(f$measurement_engine$A$iterations, 8L)
  expect_true(all(is.finite(f$locked_scores$A)))
})

test_that("continuous-only and mixed constructs get a real posterior and reliability", {
  set.seed(21)
  n <- 150
  z <- rnorm(n)
  d <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n)), -2), 2) + 3,
    a2 = pmin(pmax(round(z + rnorm(n)), -2), 2) + 3,
    a3 = pmin(pmax(round(z + rnorm(n)), -2), 2) + 3,
    c1 = 1.2 * z + rnorm(n, sd = .6),
    c2 = 1.0 * z + rnorm(n, sd = .6)
  )
  m <- specify_measurement(
    Ord = ordinal("a1", "a2", "a3"),
    Cont = continuous("c1", "c2"),
    folds = 3
  )
  f <- fit_states(m, d, seed = 3, iterations = 3, diagnostics = FALSE)
  expect_identical(f$measurement_engine$Ord$estimator, "marginal_graded_response")
  expect_identical(f$measurement_engine$Cont$estimator, "marginal_linear_factor")
  expect_true(is.finite(f$reliability[["Cont"]]))
  expect_true(f$reliability[["Cont"]] > 0 && f$reliability[["Cont"]] <= 1)
  expect_true(all(is.finite(f$score_posterior_sd$Cont)))
})

test_that("manifest() constructs pass through standardized (or raw) with asserted reliability", {
  set.seed(22)
  n <- 80
  d <- data.frame(
    a1 = sample(1:5, n, replace = TRUE), a2 = sample(1:5, n, replace = TRUE),
    age = round(rnorm(n, 40, 10))
  )
  m <- specify_measurement(A = ordinal("a1", "a2"), Age = manifest("age"), folds = 2)
  f <- fit_states(m, d, seed = 1, iterations = 2, diagnostics = FALSE)
  expect_equal(f$reliability[["Age"]], 1)
  expect_equal(mean(f$locked_scores$Age), 0, tolerance = 1e-8)
  expect_equal(sd(f$locked_scores$Age), 1, tolerance = 1e-6)
  expect_identical(f$measurement_engine$Age$estimator, "manifest")
  expect_true(all(is.na(f$score_posterior_sd$Age)))

  m_raw <- specify_measurement(A = ordinal("a1", "a2"), Age = manifest("age", standardize = FALSE), folds = 2)
  f_raw <- fit_states(m_raw, d, seed = 1, iterations = 2, diagnostics = FALSE)
  expect_equal(range(f_raw$locked_scores$Age), range(d$age))
})

test_that("fits can omit raw data while retaining score-only workflows", {
  data <- simulate_states(n = 60, seed = 403, missing = 0)
  rownames(data) <- paste0("respondent-id-", seq_len(nrow(data)))
  cluster <- rep(paste0("person-id-", seq_len(nrow(data) / 2L)), each = 2L)
  indicators <- c(paste0("a", 1:4), paste0("b", 1:4))
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3)
  retained <- fit_states(model, data, seed = 14, iterations = 1,
    diagnostics = FALSE, cluster = cluster)
  expect_true(is.data.frame(retained$data))
  fit <- fit_states(model, data, seed = 14, iterations = 1,
    diagnostics = FALSE, cluster = cluster, retain_data = FALSE)
  expect_null(fit$data)
  expect_null(fit$input_data)
  expect_false(any(grepl("respondent-id-", fit$sample_ledger$rows$row_name, fixed = TRUE)))
  expect_null(fit$input_cluster_ids)
  expect_null(fit$cluster_ids)
  expect_null(fit$measurement_split$cluster_values)
  expect_false(any(grepl("person-id-", unlist(fit, recursive = TRUE, use.names = FALSE), fixed = TRUE)))
  accounting <- sample_accounting(fit)
  expect_s3_class(accounting, "cssem_sample_accounting")
  expect_equal(nrow(accounting$unit_summary), length(unique(cluster)))
  expect_true("cluster_id" %in% names(accounting$unit_summary))
  expect_false(any(grepl("person-id-", accounting$unit_summary$cluster_id, fixed = TRUE)))
  expect_true(is.data.frame(score_states(fit, data[, indicators, drop = FALSE])))
  parameters <- measurement_parameters(fit)
  expect_true(all(is.finite(parameters$estimate[parameters$parameter != "manifest_scale"])))
  expect_true(all(is.na(parameters$observed_n)))
  expect_true(all(is.na(parameters$missing_n)))
  expect_s3_class(item_response_curve(fit, "A", "a1"), "cssem_item_response_curve")
  association <- associate(fit, specify_structure(B ~ linear(A)), structural_repeats = 1)
  expect_s3_class(association, "cssem_association")
  association_accounting <- sample_accounting(association)
  expect_equal(association_accounting$independent_unit_n, length(unique(cluster)))
  expect_false(any(grepl("person-id-", association_accounting$unit_summary$cluster_id, fixed = TRUE)))
  new_data <- simulate_states(n = 12, seed = 404, missing = 0)
  prediction <- predict(association, new_data, outcomes = "B")
  expect_s3_class(prediction, "cssem_prediction")
  comparison <- group_comparison(association, rep(c("g1", "g2"), each = 30),
    reference = "g1", permutations = 5, min_group_size = 10)
  expect_s3_class(comparison, "cssem_group_comparison")
  expect_error(group_comparison(association, "group_column", permutations = 0),
    "retain_data = TRUE or supply a group vector")
  grouped_splits <- make_splits(data, method = "group", folds = 3,
    group = cluster, seed = 15)
  split_fit <- fit_states(model, data, seed = 14, iterations = 1,
    diagnostics = FALSE, split = grouped_splits, retain_data = FALSE)
  expect_null(split_fit$fit_settings$split$group_values)
  expect_null(split_fit$fit_settings$split$unit_values)
  expect_null(split_fit$fit_settings$split$time_values)
  expect_false(any(grepl("person-id-", unlist(split_fit, recursive = TRUE, use.names = FALSE), fixed = TRUE)))
  expect_error(bootstrap_model(fit, function(context) mean(context$scores$A), reps = 2),
    "retain_data = TRUE")
  expect_error(measurement_assessment(fit), "retain_data = TRUE")
  expect_error(measurement_invariance(fit, rep(c("g1", "g2"), 30)),
    "retain_data = TRUE")
  expect_error(update(fit), "retain_data = TRUE")
  updated <- update(fit, model = model, data = data, iterations = 1)
  expect_null(updated$data)
})

test_that("a mixed construct fits through the existing measurement pipeline", {
  data <- simulate_states(n = 60, seed = 402, missing = 0)
  data$duration <- seq_len(nrow(data)) / 10
  model <- specify_measurement(
    Mixed = mixed_items(ordinal("a1", "a2"), continuous("duration")),
    Other = ordinal("b1", "b2"), folds = 3
  )
  fit <- withCallingHandlers(
    fit_states(model, data, seed = 12, iterations = 2, diagnostics = FALSE),
    cssem_nonconvergence = function(w) invokeRestart("muffleWarning")
  )
  expect_true(all(is.finite(fit$locked_scores$Mixed)))
  expect_true(all(is.finite(fit$reliability)))
})

test_that("continuous and manifest inputs preserve numeric labels", {
  # Regression: as.numeric(factor(...)) used internal level positions, so
  # numeric labels such as 10, 20, and 100 became 1, 2, and 3.
  ordered_labels <- factor(c("10", "20", "100"), levels = c("10", "20", "100"))
  reordered_labels <- factor(c("100", "10", "20"), levels = c("100", "10", "20"))
  expect_equal(.prepare_item(ordered_labels, "continuous", 1)$y, c(10, 20, 100))
  expect_equal(.prepare_item(reordered_labels, "continuous", 1)$y, c(100, 10, 20))
  expect_equal(.prepare_item(c(10, 20, 100), "continuous", 1)$y, c(10, 20, 100))
  expect_error(.prepare_item(factor(c("low", "high")), "continuous", 1), "numeric")
  expect_error(.prepare_for_encoder(c("10", "bad"), "continuous", 1, NULL), "numeric")
  expect_error(.prepare_item(c(10, Inf), "continuous", 1), "finite")

  manifest_spec <- list(indicators = "age", scales = "manifest", keys = 1L,
    manifest = TRUE, reliability = 1, standardize = FALSE)
  encoder <- .fit_encoder(data.frame(age = ordered_labels), manifest_spec)
  scored <- .predict_encoder(encoder,
    data.frame(age = factor(c("100", "10"), levels = c("100", "10"))))
  expect_equal(scored, c(100, 10))
})

test_that("score_states() returns scores on the locked_scores scale", {
  # Regression: score_states() returned raw posterior means (SD ~ sqrt(reliability))
  # while locked scores are standardized, so coefficients estimated on locked
  # scores did not apply to new-record scores.
  d <- simulate_states(n = 300, seed = 31)
  d$age <- round(stats::rnorm(300, 40, 10))
  m <- specify_measurement(A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)),
    Age = manifest("age", standardize = FALSE), folds = 3)
  f <- fit_states(m, d, seed = 3, iterations = 10, diagnostics = FALSE)
  indicators <- unlist(lapply(m$constructs, `[[`, "indicators"), use.names = FALSE)
  scored <- score_states(f, d[, indicators])
  for (nm in c("A", "B")) {
    expect_equal(sd(scored[[nm]]), sd(f$locked_scores[[nm]]), tolerance = .03)
    expect_equal(unname(stats::coef(stats::lm(f$locked_scores[[nm]] ~ scored[[nm]]))[2L]), 1, tolerance = .03)
  }
  # A manifest covariate kept in natural units stays in natural units.
  expect_equal(scored$Age, d$age)
  # A fit object without stored standardization falls back to the raw scale, loudly.
  legacy <- f; legacy$score_center <- NULL
  expect_warning(score_states(legacy, d[, indicators]), "predates stored score standardization")
})

test_that("non-convergence is reported, and the default budget converges on clean data", {
  # Regression: the old default (iterations = 6, a 12-iteration EM cap) stopped
  # before convergence on ordinary data with no warning; the only signal was
  # fit$measurement_engine.
  d <- simulate_states(n = 300, seed = 41)
  m <- specify_measurement(A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)), folds = 3)
  expect_warning(low <- fit_states(m, d, seed = 1, iterations = 1, diagnostics = FALSE),
    "reached its 8-iteration EM cap", class = "cssem_nonconvergence")
  expect_false(low$measurement_engine$A$converged)
  expect_lt(low$measurement_engine$A$folds_converged, 3L)
  expect_no_warning(default <- fit_states(m, d, seed = 1, diagnostics = FALSE), class = "cssem_nonconvergence")
  expect_true(all(vapply(default$measurement_engine, `[[`, logical(1), "converged")))
  expect_true(all(vapply(default$measurement_engine, function(e) e$folds_converged == e$folds, logical(1))))
  # Harness fits record convergence per job and stay quiet.
  expect_no_warning(cssem:::.fit_states_quiet(m, d, seed = 1, iterations = 1, diagnostics = FALSE),
    class = "cssem_nonconvergence")
})

test_that("ordinal() rejects non-integer category codes instead of truncating", {
  d <- simulate_states(n = 40, seed = 5)
  d$a1 <- d$a1 + 0.5
  m <- specify_measurement(A = ordinal("a1", "a2", "a3", "a4"))
  expect_error(fit_states(m, d, seed = 1, iterations = 2, diagnostics = FALSE), "whole-number")
})

test_that("text category labels are rejected, and an ordered factor keeps its declared order", {
  # Regression: character indicators were coded with factor(x, ordered = TRUE),
  # whose level order is alphabetical. A frequency scale then became
  # "always" < "never" < "often" < "rarely" < "sometimes", which scrambles the
  # response scale silently: recovery of the generating latent fell to -0.27
  # from 0.91 for the same data as integer codes.
  set.seed(1)
  n <- 300
  z <- stats::rnorm(n)
  labels <- c("never", "rarely", "sometimes", "often", "always")
  codes <- function(truth) as.data.frame(stats::setNames(lapply(1:4, function(j)
    cut(.8 * truth + stats::rnorm(n, 0, .6), c(-Inf, -1.2, -.4, .4, 1.2, Inf), labels = FALSE)),
    paste0(if (identical(truth, z)) "a" else "b", 1:4)))
  numeric_frame <- cbind(codes(z), codes(stats::rnorm(n)))
  text_frame <- as.data.frame(lapply(numeric_frame, function(column) labels[column]),
    stringsAsFactors = FALSE)
  # Alphabetical level order differs from scale order, so the two disagree
  # unless the declared order is honoured.
  factor_frame <- as.data.frame(lapply(numeric_frame, function(column)
    factor(labels[column], levels = labels, ordered = TRUE)))

  m <- specify_measurement(A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)), folds = 3)
  expect_error(fit_states(m, text_frame, seed = 1, iterations = 4, diagnostics = FALSE),
    "no inferable category order")
  # Numeric strings are unambiguous and still accepted.
  string_frame <- as.data.frame(lapply(numeric_frame, as.character), stringsAsFactors = FALSE)
  expect_no_error(fit_states(m, string_frame, seed = 1, iterations = 4, diagnostics = FALSE))

  numeric_fit <- fit_states(m, numeric_frame, seed = 1, iterations = 10, diagnostics = FALSE)
  factor_fit <- fit_states(m, factor_frame, seed = 1, iterations = 10, diagnostics = FALSE)
  expect_equal(factor_fit$locked_scores$A, numeric_fit$locked_scores$A, tolerance = 1e-8)
  expect_gt(abs(stats::cor(factor_fit$locked_scores$A, z)), .85)
})

test_that("scoring maps categories by label, not by position in the scoring frame", {
  # Regression: the stored category schema was the positions of the observed
  # categories, and scoring re-derived positions from whatever frame it was
  # given. New records built independently (so their factor carries a different
  # level set) were therefore mapped to the wrong categories, shifting scores by
  # up to half a standard deviation with no warning.
  set.seed(2)
  n <- 300
  z <- stats::rnorm(n)
  labels <- c("never", "rarely", "sometimes", "often", "always")
  block <- function(truth, prefix) as.data.frame(stats::setNames(lapply(1:4, function(j)
    factor(labels[cut(.8 * truth + stats::rnorm(n, 0, .6),
      c(-Inf, -1.2, -.4, .4, 1.2, Inf), labels = FALSE)], levels = labels, ordered = TRUE)),
    paste0(prefix, 1:4)))
  d <- cbind(block(z, "a"), block(stats::rnorm(n), "b"))
  m <- specify_measurement(A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)), folds = 3)
  f <- fit_states(m, d, seed = 1, iterations = 10, diagnostics = FALSE)
  indicators <- unlist(lapply(m$constructs, `[[`, "indicators"), use.names = FALSE)

  full <- score_states(f, d[, indicators])
  # New records arriving on their own: the factors are rebuilt from the rows in
  # hand, so their level set is whatever those rows happen to contain.
  rows <- which(d$a1 != "always" & d$b1 != "always")[1:25]
  fresh <- as.data.frame(lapply(d[rows, indicators], function(column)
    factor(as.character(column), levels = sort(unique(as.character(column))), ordered = TRUE)))
  subset_scores <- score_states(f, fresh)
  expect_equal(subset_scores$A, full$A[rows], tolerance = 1e-8)
  expect_equal(subset_scores$B, full$B[rows], tolerance = 1e-8)

  # The stored schema is the labels themselves.
  expect_equal(f$full_encoders$A$levels[[1L]], labels)
  # An unseen category is still refused rather than silently remapped.
  unseen <- d[1:10, indicators]
  levels(unseen$a1) <- c(levels(unseen$a1), "constantly")
  unseen$a1[1] <- "constantly"
  expect_error(score_states(f, unseen), "unseen ordinal category")
})

test_that("score_states rejects fractional ordinal codes before coercion", {
  # Regression: scoring converted numeric ordinal values with as.integer()
  # before validation, so 1.9 silently became category 1.
  d <- simulate_states(n = 180, seed = 23)
  m <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3)
  f <- fit_states(m, d, seed = 1, iterations = 8, diagnostics = FALSE)

  fractional <- d[1:12, paste0("a", 1:4), drop = FALSE]
  fractional$a1[[1L]] <- fractional$a1[[1L]] + .25
  expect_error(score_states(f, fractional), "whole-number")

  numeric_strings <- d[1:12, paste0("a", 1:4), drop = FALSE]
  numeric_strings[] <- lapply(numeric_strings, as.character)
  numeric_strings$a1[[1L]] <- "1.9"
  expect_error(score_states(f, numeric_strings), "whole-number")

  valid <- d[1:12, paste0("a", 1:4), drop = FALSE]
  valid$a1[[1L]] <- NA_integer_
  expect_no_error(score_states(f, valid))
})

test_that("seeded entry points restore the caller's random stream", {
  # Regression: fit_states(), associate(), the bootstraps and the harnesses all
  # called set.seed() and never restored it, so a user's own random draws
  # changed because they fitted a model. Determinism from the `seed` argument is
  # deliberate; consuming the caller's stream is not.
  d <- simulate_states(n = 200, seed = 3)
  m <- specify_measurement(A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)), folds = 3)
  fit <- fit_states(m, d, seed = 1, iterations = 8, diagnostics = FALSE)
  structure_spec <- specify_structure(B ~ A, order = c("A", "B"))

  unchanged <- function(expr) {
    set.seed(99); before <- .Random.seed
    force(expr)
    identical(before, .Random.seed)
  }
  expect_true(unchanged(fit_states(m, d, seed = 1, iterations = 4, diagnostics = FALSE)))
  expect_true(unchanged(associate(fit, structure_spec, seed = 2)))
  expect_true(unchanged(simulate_states(n = 50, seed = 7)))

  # The caller's sequence is the one it would have had, and the fit is still
  # reproducible from its own seed.
  set.seed(1); first_draws <- stats::runif(3)
  first_fit <- fit_states(m, d, seed = 5, iterations = 6, diagnostics = FALSE)
  set.seed(1); second_draws <- stats::runif(3)
  second_fit <- fit_states(m, d, seed = 5, iterations = 6, diagnostics = FALSE)
  expect_identical(first_draws, second_draws)
  expect_equal(first_fit$locked_scores, second_fit$locked_scores)

  # A session that has not drawn a random number yet keeps none.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    rm(".Random.seed", envir = globalenv())
  invisible(simulate_states(n = 20, seed = 2))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
