test_that("linear equality constraints pool selected structural coefficients", {
  n <- 80L; set.seed(19)
  A <- rnorm(n); B <- 1.5 * A + rnorm(n, sd = .2); C <- 1.5 * A + rnorm(n, sd = .2)
  fit <- structure(list(locked_scores = data.frame(A = A, B = B, C = C),
    folds = rep(1:4, length.out = n), reliability = c(A = NA_real_, B = NA_real_, C = NA_real_)), class = "fit_states")
  structure <- specify_structure(B ~ linear(A), C ~ linear(A), order = c("A", "B", "C"))
  result <- associate(fit, structure, structural_repeats = 1L, seed = 4L,
    constraints = cssem_constraint(equal = list(c("B~A", "C~A"))))
  expect_s3_class(result, "cssem_association")
  expect_true(isTRUE(result$constraints$active))
  b <- result$full_models$B$coefficient[result$full_models$B$maps$A][1L]
  c <- result$full_models$C$coefficient[result$full_models$C$maps$A][1L]
  expect_equal(b, c, tolerance = 1e-10)
  expect_true(is.finite(result$constraint_diagnostics$condition_number))
})

test_that("fixed constraints and unsupported settings fail explicitly", {
  n <- 40L; scores <- data.frame(A = seq_len(n) / n, B = seq_len(n)^2 / n)
  fit <- structure(list(locked_scores = scores, folds = rep(1:4, length.out = n),
    reliability = c(A = NA_real_, B = NA_real_)), class = "fit_states")
  structure <- specify_structure(B ~ linear(A), order = c("A", "B"))
  fixed <- associate(fit, structure, structural_repeats = 1L,
    constraints = cssem_constraint(fixed = c("B~A" = 0)))
  expect_equal(unname(fixed$full_models$B$coefficient[fixed$full_models$B$maps$A][1L]), 0, tolerance = 1e-10)
  expect_error(cssem_constraint(equal = list(c("B~A", "B~A"))), "duplicate")
  expect_error(associate(fit, structure, structural_repeats = 1L,
    constraints = cssem_constraint(fixed = c("B~A" = 0)), respondent_weighting = "information"), "information weighting")
  expect_error(associate(fit, structure, structural_repeats = 1L,
    constraints = cssem_constraint(fixed = c("B~A" = 0)), reliability = c(A = .8)), "EIV")
})
