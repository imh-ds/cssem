test_that("associational layer uses locked scores and reports a shadow gap", {
  set.seed(10)
  n <- 120
  trust <- rnorm(n)
  satisfaction <- .65 * trust + rnorm(n, sd = .7)
  loyalty <- .35 * trust + .60 * satisfaction + rnorm(n, sd = .7)
  fit <- structure(list(
    locked_scores = data.frame(Trust = trust, Satisfaction = satisfaction, Loyalty = loyalty),
    folds = rep(1:3, length.out = n)
  ), class = "fit_states")
  structure_spec <- cssem_structure(list(
    Satisfaction = "Trust",
    Loyalty = c("Trust", "Satisfaction")
  ), order = c("Trust", "Satisfaction", "Loyalty"))
  association <- associate(fit, structure_spec)
  expect_s3_class(association, "cssem_association")
  expect_equal(nrow(specification_gap(association)), 4)
  expect_equal(nrow(specification_gap(association, "temporal")), 2)
  expect_true(all(specification_gap(association)$specification_gap ==
    specification_gap(association)$theory_r_squared - specification_gap(association)$shadow_r_squared))
  expect_equal(sum(association$candidate_metrics$selected), 3)
  expect_true(all(c("mean_mse_improvement", "mse_improvement_se") %in% names(association$candidate_metrics)))
  expect_equal(association$structural_repeats, 5L)
  expect_equal(nrow(effect_ledger(association)), 3L)
  expect_identical(association$status, "associational")
})

test_that("a user-supplied reliability is the one the correction uses", {
  # Regression: with posterior variances on the fit, .eiv_coefficients()
  # re-estimated reliability from them and silently discarded associate()'s
  # `reliability` argument, while the ledger still reported the supplied value.
  d <- simulate_states(n = 300, seed = 21)
  fit <- fit_states(specify_measurement(A = ordinal(paste0("a", 1:4)), B = ordinal(paste0("b", 1:4)), folds = 3),
    d, seed = 2, iterations = 8, diagnostics = FALSE)
  structure_spec <- specify_structure(B ~ linear(A), order = c("A", "B"))
  default <- effect_ledger(associate(fit, structure_spec, structural_repeats = 2L, seed = 1))
  supplied <- effect_ledger(associate(fit, structure_spec, structural_repeats = 2L, seed = 1, reliability = c(A = .5)))
  # One standardized predictor: the correction is exactly naive / reliability.
  expect_equal(supplied$corrected_estimate, supplied$naive_estimate / .5, tolerance = 1e-8)
  expect_equal(supplied$predictor_reliability, .5)
  expect_false(isTRUE(all.equal(supplied$corrected_estimate, default$corrected_estimate)))
  expect_error(associate(fit, structure_spec, reliability = c(Z = .5)), "named by locked construct")
  expect_error(associate(fit, structure_spec, reliability = c(A = 1.5)), "\\(0, 1\\]")
})

test_that("edge declarations support policies while preserving character vectors", {
  declared <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("monotone_increasing")),
    Loyalty = c("Trust", "Quality")
  ), order = c("Trust", "Quality", "Loyalty"))
  expect_equal(declared$effects$Quality$Trust$shape, "monotone_increasing")
  expect_equal(declared$effects$Loyalty$Trust$shape, "auto")
  expect_error(cssem_effect("not_a_shape"))
})

test_that("monotone candidates are constrained and selected edge by edge", {
  set.seed(44)
  n <- 220
  trust <- rnorm(n)
  quality <- .70 * trust + .85 * pmax(trust, 0) + rnorm(n, sd = .30)
  loyalty <- .45 * trust + .35 * quality + rnorm(n, sd = .60)
  fit <- structure(list(locked_scores = data.frame(Trust = trust, Quality = quality, Loyalty = loyalty),
    folds = sample(rep(1:3, length.out = n))), class = "fit_states")
  specification <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("auto")),
    Loyalty = list(Trust = cssem_effect("auto"), Quality = cssem_effect("auto"))
  ), order = c("Trust", "Quality", "Loyalty"))
  association <- associate(fit, specification, structural_repeats = 3, seed = 44, shape_stability_min = .50)
  quality <- association$candidate_metrics[association$candidate_metrics$outcome == "Quality", , drop = FALSE]
  expect_true(any(quality$shape == "monotone_increasing"))
  expect_lte(sum(association$candidate_metrics$selected & association$candidate_metrics$outcome == "Loyalty" & association$candidate_metrics$shape != "linear"), 1L)
  expect_true(all(c("edge_drop_mse_increase", "selection_frequency", "status") %in% names(effect_ledger(association))))
})

test_that("default selector retains a clear monotone-increasing edge", {
  set.seed(144)
  n <- 320
  trust <- rnorm(n)
  quality <- .85 * trust + 1.10 * pmax(trust + .20, 0) + rnorm(n, sd = .22)
  loyalty <- .30 * trust + .55 * quality + rnorm(n, sd = .45)
  fit <- structure(list(
    locked_scores = data.frame(Trust = trust, Quality = quality, Loyalty = loyalty),
    folds = sample(rep(1:3, length.out = n))
  ), class = "fit_states")
  specification <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("monotone_increasing")),
    Loyalty = list(Trust = cssem_effect("auto"), Quality = cssem_effect("auto"))
  ), order = c("Trust", "Quality", "Loyalty"))
  association <- associate(fit, specification, structural_repeats = 5, seed = 144)
  quality <- association$candidate_metrics[association$candidate_metrics$outcome == "Quality" &
    association$candidate_metrics$predictor == "Trust" & association$candidate_metrics$selected, , drop = FALSE]
  expect_equal(quality$shape[[1L]], "monotone_increasing")
  expect_gte(quality$selection_frequency[[1L]], .70)
})

test_that("curvature is decided by the reported test, not by the cross-validated loss", {
  # Regression: acceptance used to require a tuned repeated-CV margin, which held
  # the false-curve rate near zero only by discarding real curvature. A robust
  # nested test now decides, and its p-value is reported alongside the shape.
  set.seed(77)
  n <- 300
  trust <- rnorm(n)
  kinked <- .55 * trust + .95 * pmax(trust, 0) + rnorm(n, sd = .35)
  straight <- .65 * trust + rnorm(n, sd = .70)
  specification <- cssem_structure(list(Quality = list(Trust = cssem_effect("auto"))),
    order = c("Trust", "Quality"))
  fit_of <- function(quality) structure(list(
    locked_scores = data.frame(Trust = trust, Quality = quality),
    folds = sample(rep(1:3, length.out = n))), class = "fit_states")
  selected_row <- function(association) {
    metrics <- association$candidate_metrics
    metrics[metrics$selected & metrics$predictor == "Trust", , drop = FALSE]
  }

  curved <- selected_row(associate(fit_of(kinked), specification, seed = 77))
  expect_true(curved$shape[[1L]] != "linear")
  expect_lt(curved$nonlinearity_p[[1L]], .05)

  linear <- selected_row(associate(fit_of(straight), specification, seed = 77))
  expect_equal(linear$shape[[1L]], "linear")
  expect_gt(linear$nonlinearity_p[[1L]], .05)

  # The same curved edge is reported as linear once the test is made strict
  # enough, so the shape follows the stated error rate.
  strict <- selected_row(associate(fit_of(kinked), specification, seed = 77,
    shape_alpha = max(curved$nonlinearity_p[[1L]] / 10, 1e-300)))
  expect_equal(strict$shape[[1L]], "linear")
  expect_error(associate(fit_of(kinked), specification, shape_alpha = 0), "shape_alpha")
})

test_that("detected but negligible curvature is reported as linear", {
  # At large n the test detects curvature that removes almost none of the
  # prediction error (skewed indicators produce exactly this: mildly curved
  # scores from a straight latent relation). shape_min_gain keeps those edges
  # linear without weakening the test itself.
  set.seed(79)
  n <- 3000
  trust <- rnorm(n)
  quality <- trust + .035 * trust^2 + rnorm(n, sd = 1)
  fit <- structure(list(locked_scores = data.frame(Trust = trust, Quality = quality),
    folds = sample(rep(1:3, length.out = n))), class = "fit_states")
  specification <- cssem_structure(list(Quality = list(Trust = cssem_effect("auto"))),
    order = c("Trust", "Quality"))
  selected <- function(association) {
    metrics <- association$candidate_metrics
    metrics[metrics$selected & metrics$predictor == "Trust", , drop = FALSE]
  }
  detected <- selected(associate(fit, specification, seed = 79, shape_min_gain = 0))
  expect_true(detected$shape[[1L]] != "linear")
  expect_lt(detected$nonlinearity_p[[1L]], .05)

  reported <- selected(associate(fit, specification, seed = 79))
  expect_equal(reported$shape[[1L]], "linear")
  # The edge is still flagged as curved; only the reported shape changes.
  expect_lt(reported$nonlinearity_p[[1L]], .05)
  expect_error(associate(fit, specification, shape_min_gain = -1), "shape_min_gain")
})

test_that("the curvature test holds its level under unequal error variance", {
  # The regressors are posterior means whose precision varies by respondent, so
  # the test uses HC3 standard errors; a classical F-test rejects far too often
  # when the residual variance grows with the predictor.
  set.seed(78)
  rejected <- vapply(seq_len(200), function(i) {
    x <- rnorm(250)
    y <- .5 * x + rnorm(250, sd = .4 + abs(x))
    cssem:::.nonlinearity_p(data.frame(Trust = x, Quality = y), "Quality", "Trust",
      c(Trust = "linear"), c(3L, 4L)) < .05
  }, logical(1))
  expect_lt(mean(rejected), .10)
})

test_that("winner selection prefers stable monotone candidates within uncertainty band", {
  candidate_keys <- c("Trust::monotone_increasing", "Trust::smooth_df3")
  candidate_meta <- list(
    `Trust::monotone_increasing` = list(predictor = "Trust", shape = "monotone_increasing"),
    `Trust::smooth_df3` = list(predictor = "Trust", shape = "smooth_df3")
  )
  improvement <- c(`Trust::monotone_increasing` = .020, `Trust::smooth_df3` = .022)
  improvement_se <- c(`Trust::monotone_increasing` = .004, `Trust::smooth_df3` = .004)
  frequency <- c(`Trust::monotone_increasing` = .80, `Trust::smooth_df3` = 1)
  winner <- cssem:::.pick_shape_winner(candidate_keys, candidate_meta, improvement, improvement_se,
    frequency, smooth_uncertainty = 1, shape_stability_min = .70)
  expect_equal(winner, "Trust::monotone_increasing")
})

test_that("a monotone candidate needs the same SE margin as a spline to count as supported", {
  # Regression: monotone candidates used to count as supported in a repeat
  # whenever their mean improvement was merely positive, which happens in about
  # half of all repeats with no signal at all and drove a ~30-40% null
  # false-nonlinear rate. Two repeats of three folds, improvement positive but
  # well inside one standard error in each.
  base <- list(fold_mse = rep(1, 6))
  noisy <- list(fold_mse = 1 - c(.05, -.04, .02, .05, -.04, .02))
  clear <- list(fold_mse = 1 - c(.05, .06, .055, .05, .06, .055))
  frequency <- cssem:::.selection_frequency(base, list(noisy = noisy, clear = clear), multiplier = 1, folds_per_repeat = 3L)
  expect_equal(unname(frequency[["noisy"]]), 0)
  expect_equal(unname(frequency[["clear"]]), 1)
})

test_that("monotone basis retains training-fold knots for scoring", {
  trained <- cssem:::.train_basis(c(-2, -1, 0, 1, 2), "monotone_increasing")
  scored <- cssem:::.predict_basis(c(-10, 10), trained$info)
  expect_equal(ncol(scored), ncol(trained$values))
  expect_equal(ncol(trained$values), 6L)
  expect_false(any(trained$info$knots %in% c(-10, 10)))
})

test_that("monotone fits can be concave or convex but never non-monotone", {
  # Regression: the increasing basis was the linear term plus (x - knot)_+
  # hinges with nonnegative coefficients, so its slope could only increase: it
  # could not represent diminishing returns. The concave hinges fix that while
  # the sign constraint still rules out non-monotone fits.
  x <- seq(-2, 2, length.out = 400)
  fit_curve <- function(y, shape) {
    model <- cssem:::.fit_shape_model(data.frame(x = x, y = y), "y", c(x = shape))
    cssem:::.predict_shape_model(model, data.frame(x = x))
  }
  concave <- log(x + 2.5)            # increasing, diminishing returns
  convex <- exp(x)                   # increasing, accelerating
  u_shape <- x^2                     # not monotone
  for (truth in list(concave, convex)) {
    fitted <- fit_curve(truth, "monotone_increasing")
    expect_true(all(diff(fitted) >= -1e-8))
    expect_lt(mean((fitted - truth)^2), .01 * stats::var(truth))
  }
  expect_true(all(diff(fit_curve(-concave, "monotone_decreasing")) <= 1e-8))
  expect_true(all(diff(fit_curve(u_shape, "monotone_increasing")) >= -1e-8))
})

test_that("structural repeated CV validates its repeat count", {
  fit <- structure(list(locked_scores = data.frame(A = rnorm(30), B = rnorm(30)), folds = rep(1:3, 10)), class = "fit_states")
  specification <- cssem_structure(list(B = "A"), order = c("A", "B"))
  expect_error(associate(fit, specification, structural_repeats = 0L), "at least 1")
})

test_that("structural declarations reject invalid self-effects", {
  expect_error(cssem_structure(list(Trust = "Trust")))
})

test_that("specify_structure() produces the same object as cssem_structure()", {
  via_list <- cssem_structure(list(
    Satisfaction = "Trust",
    Loyalty = c("Trust", "Satisfaction")
  ), order = c("Trust", "Satisfaction", "Loyalty"))
  via_formula <- specify_structure(
    Satisfaction ~ Trust,
    Loyalty ~ Trust + Satisfaction,
    order = c("Trust", "Satisfaction", "Loyalty")
  )
  expect_equal(via_list, via_formula)
})

test_that("specify_structure() supports interaction terms via formula colon syntax", {
  via_list <- cssem_structure(list(Hope = c("Planning", "FlexEx", "Planning:FlexEx")))
  via_formula <- specify_structure(Hope ~ Planning + FlexEx + Planning:FlexEx)
  expect_equal(via_list$effects, via_formula$effects)
})

test_that("specify_structure() shape wrappers declare non-default edge policies", {
  declared <- specify_structure(
    Quality ~ monotone_increasing(Trust),
    Loyalty ~ Trust + Quality,
    order = c("Trust", "Quality", "Loyalty")
  )
  expect_equal(declared$effects$Quality$Trust$shape, "monotone_increasing")
  expect_equal(declared$effects$Loyalty$Trust$shape, "auto")
  equivalent_list <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("monotone_increasing")),
    Loyalty = c("Trust", "Quality")
  ), order = c("Trust", "Quality", "Loyalty"))
  expect_equal(declared, equivalent_list)
})

test_that("specify_structure() rejects shape wrappers on interaction terms", {
  expect_error(specify_structure(Hope ~ smooth(Planning:FlexEx)))
})

test_that("specify_structure() requires formulas and unique outcomes", {
  expect_error(specify_structure(list(Hope = "Planning")))
  expect_error(specify_structure(Hope ~ Planning, Hope ~ Neuro))
})

test_that("temporal shadows require a valid ordering for cyclic declarations", {
  fit <- structure(list(locked_scores = data.frame(A = rnorm(30), B = rnorm(30)), folds = rep(1:3, 10)), class = "fit_states")
  cyclic <- cssem_structure(list(A = "B", B = "A"))
  expect_error(associate(fit, cyclic, shadow_scope = "temporal"))
  expect_s3_class(associate(fit, cyclic, shadow_scope = "unrestricted"), "cssem_association")
})

test_that("exploratory preset lightens structural defaults", {
  set.seed(21)
  fit <- structure(list(
    locked_scores = data.frame(Trust = rnorm(60), Quality = rnorm(60), Loyalty = rnorm(60)),
    folds = sample(rep(1:3, length.out = 60))
  ), class = "fit_states")
  specification <- cssem_structure(list(
    Quality = "Trust",
    Loyalty = c("Trust", "Quality")
  ), order = c("Trust", "Quality", "Loyalty"))
  association <- associate(fit, specification, preset = "exploratory")
  expect_equal(association$structural_repeats, 2L)
  expect_identical(association$shadow_scope, "temporal")
})

test_that("smooth basis degrades gracefully when quantile knots collapse", {
  # Enough mass on one value that every df-based quantile knot equals the left
  # boundary, which errors in splines::ns() and previously aborted the fit.
  x <- c(rep(0, 90), seq(.1, 1, length.out = 10))
  expect_error(splines::ns(x, df = 4L))
  trained <- cssem:::.train_basis(x, "smooth_df4")
  reconstructed <- cssem:::.predict_basis(x, trained$info)
  expect_equal(ncol(as.matrix(reconstructed)), ncol(as.matrix(trained$values)))
  # Fully degenerate scores degrade to the single linear column.
  constant <- cssem:::.train_basis(rep(1, 50), "smooth_df4")
  expect_identical(ncol(as.matrix(constant$values)), 1L)
  expect_identical(ncol(as.matrix(cssem:::.predict_basis(rep(1, 50), constant$info))), 1L)
})

test_that("an interaction edge is labelled a product everywhere, and curves are built quietly", {
  # Regression: effect_ledger() reported shape "linear" for an interaction while
  # effect_card() reported "product", so one edge read as two different things.
  # Separately, .effect_rows() built its curve grid over every name in the model
  # including the interaction, which is not a column of the scores, producing
  # "argument is not numeric or logical: returning NA" whenever a model held
  # both an interaction and a selected nonlinear edge.
  set.seed(41)
  n <- 500
  w <- stats::rnorm(n); x <- stats::rnorm(n)
  y <- .5 * x + .9 * pmax(x, 0) + .2 * w + .25 * x * w + stats::rnorm(n, sd = .5)
  fit <- structure(list(locked_scores = data.frame(W = as.numeric(scale(w)),
    X = as.numeric(scale(x)), Y = as.numeric(scale(y))),
    folds = sample(rep(1:3, length.out = n)),
    reliability = c(W = .85, X = .85, Y = .85)), class = "fit_states")

  expect_no_warning(
    association <- associate(fit, specify_structure(Y ~ X + W + X:W,
      order = c("W", "X", "Y")), seed = 41))
  ledger <- effect_ledger(association); card <- effect_card(association, "Y")
  # The setup is the one that used to warn: a curved edge beside an interaction.
  expect_true(ledger$shape[ledger$predictor == "X"] != "linear")
  expect_identical(ledger$shape[ledger$predictor == "X:W"], "product")
  expect_identical(unique(card$effects$shape[card$effects$predictor == "X:W"]), "product")
  # The fitted curve for the nonlinear edge is still produced in full.
  expect_equal(sum(!is.na(card$effects$fitted)), 50L)
})

test_that("structural reports expose interaction errors-in-variables corrections", {
  # Regression: the correction solver handled product terms, but the
  # structural report excluded the product shape and returned no corrected
  # estimate or interval for the same interaction.
  set.seed(81)
  n <- 500
  X <- stats::rnorm(n); W <- stats::rnorm(n)
  Y <- .4 * X + .6 * W + 1.8 * X * W + stats::rnorm(n, sd = .7)
  scores <- data.frame(X = as.numeric(scale(X)), W = as.numeric(scale(W)),
    Y = as.numeric(scale(Y)))
  selected <- c(X = "linear", W = "linear", "X:W" = "product")
  reported <- cssem:::.corrected_effects(scores, "Y", selected,
    c(X = .8, W = .8, Y = .9), replicates = 40L, seed = 81L)
  interaction <- reported[reported$predictor == "X:W", , drop = FALSE]
  direct <- cssem:::.eiv_coefficients(scores, "Y", names(selected),
    c(X = .8, W = .8, Y = .9))
  expect_true(isTRUE(interaction$eiv_applicable))
  expect_true(is.finite(interaction$corrected_estimate))
  expect_true(is.finite(interaction$corrected_ci_low) && is.finite(interaction$corrected_ci_high))
  expect_equal(interaction$corrected_estimate, unname(direct$corrected[["X:W"]]))
  expect_equal(interaction$predictor_reliability, .8 * .8)
})
