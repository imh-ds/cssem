#' Simulate manifestation-construct data for deterministic validation
#'
#' Generates two correlated-free latent constructs with ordinal indicator blocks.
#' Optional cross-loadings and shared item residuals create controlled measurement
#' misspecification. The known states are stored in `attr(data, "truth")`.
#'
#' @param n Number of respondents.
#' @param items Number of ordinal indicators per construct.
#' @param loading Primary item discrimination proxy used in the data generator.
#' @param missing Probability of item-level missingness.
#' @param local_dependence Shared residual magnitude for the first two items in
#'   each construct.
#' @param cross_loading Loading of the other construct on every item.
#' @param seed Integer random seed.
#' @return A data frame with `a*` and `b*` ordinal items and a `truth` attribute.
#' @examples
#' simulated <- simulate_cssem_data(n = 200, seed = 42)
#' attr(simulated, "truth")
#' @export
simulate_cssem_data <- function(n = 400L, items = 4L, loading = .8, missing = .05,
                                local_dependence = 0, cross_loading = 0, seed = 1L) {
  set.seed(seed); z1 <- stats::rnorm(n); z2 <- stats::rnorm(n)
  make_block <- function(z, other, prefix) {
    shared_residual <- stats::rnorm(n)
    ans <- lapply(seq_len(items), function(j) {
      eta <- loading * z + cross_loading * other + stats::rnorm(n, 0, sqrt(max(.05, 1 - loading^2)))
      if (j %in% c(1L, 2L)) eta <- eta + local_dependence * shared_residual
      x <- cut(eta, breaks = c(-Inf, -1, -.2, .5, Inf), labels = FALSE)
      x[stats::runif(n) < missing] <- NA_integer_; x
    }); names(ans) <- paste0(prefix, seq_len(items)); as.data.frame(ans)
  }
  dat <- cbind(make_block(z1, z2, "a"), make_block(z2, z1, "b")); attr(dat, "truth") <- cbind(A = z1, B = z2); dat
}

#' Create a deterministic measurement-validation design
#'
#' @param tier `"screening"` for six representative local scenarios or `"full"`
#'   for the 32-scenario confirmation grid.
#' @return A data frame accepted by [run_measurement_benchmark()].
#' @examples
#' cssem_validation_design("screening")
#' @export
cssem_validation_design <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  if (tier == "screening") return(data.frame(
    n = c(200L, 500L, 200L, 500L, 500L, 200L),
    loading = c(.80, .80, .55, .80, .80, .55),
    missing = c(0, .05, .05, .10, .05, .10),
    local_dependence = c(0, 0, 0, 0, .35, .35),
    cross_loading = c(0, 0, 0, .20, 0, .20)
  ))
  expand.grid(
    n = c(200L, 500L), loading = c(.55, .80), missing = c(0, .10),
    local_dependence = c(0, .35), cross_loading = c(0, .20),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

#' Run a small CS-SEM versus CB-SEM/PLS scoring benchmark
#'
#' The CB-SEM comparator is a deterministic ordinal-CFA score proxy: a first
#' factor of standardized ordinal indicators. Install and use lavaan in a
#' confirmatory study for a full DWLS CFA fit; this dependency-light benchmark
#' is intentionally runnable in a clean R installation.
#'
#' @param reps Number of independent replications per scenario.
#' @param n Respondent count for the default single scenario; ignored when
#'   `design` is supplied.
#' @param seed Integer seed used to derive deterministic replication seeds.
#' @param tolerance Non-inferiority tolerance used for the `success` attribute.
#' @param design Optional scenario data frame with columns `n`, `loading`,
#'   `missing`, `local_dependence`, and `cross_loading`.
#' @param folds Cross-fitting folds used inside each benchmark fit.
#' @param iterations Measurement iterations used inside each benchmark fit.
#' @return A data frame of scenario results. Its `success_criterion` and
#'   `success` attributes describe the predeclared screening gate.
#' @examples
#' benchmark <- run_measurement_benchmark(
#'   design = cssem_validation_design("screening"), reps = 1, seed = 1
#' )
#' attr(benchmark, "success")
#' @export
run_measurement_benchmark <- function(reps = 20L, n = 400L, seed = 1L, tolerance = .02,
                                      design = NULL, folds = 3L, iterations = 4L) {
  if (is.null(design)) design <- data.frame(n = n, loading = .8, missing = .05, local_dependence = 0, cross_loading = 0)
  required <- c("n", "loading", "missing", "local_dependence", "cross_loading")
  if (!is.data.frame(design) || !all(required %in% names(design))) stop("design must contain: n, loading, missing, local_dependence, cross_loading.", call. = FALSE)
  rows <- vector("list", reps * nrow(design)); row <- 0L
  for (scenario in seq_len(nrow(design))) for (r in seq_len(reps)) {
    row <- row + 1L; d <- design[scenario, ]
    dat <- simulate_cssem_data(n = d$n, loading = d$loading, missing = d$missing, local_dependence = d$local_dependence, cross_loading = d$cross_loading, seed = seed + row); truth <- attr(dat, "truth")
    model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal"), B = list(indicators = paste0("b", 1:4), scales = "ordinal")), folds = folds)
    fit <- cssem_fit(model, dat, seed = seed + row, iterations = iterations, diagnostics = FALSE)
    cs <- mean(abs(diag(stats::cor(fit$locked_scores, truth))))
    blocks <- list(1:4, 5:8)
    pls <- mean(vapply(blocks, function(ix) abs(stats::cor(rowMeans(dat[ix], na.rm=TRUE), truth[, if (min(ix)==1) 1 else 2], use="complete.obs")), numeric(1)))
    cb <- mean(vapply(blocks, function(ix) { x <- scale(dat[ix]); x[is.na(x)] <- 0; sc <- x %*% stats::prcomp(x)$rotation[,1]; abs(stats::cor(sc, truth[, if (min(ix)==1) 1 else 2])) }, numeric(1)))
    rows[[row]] <- cbind(data.frame(scenario = scenario, rep = r), d, data.frame(cssem_recovery = cs, cbsem_ordinal_factor_proxy_recovery = cb, pls_composite_recovery = pls, warning_count = nrow(fit$warnings)))
  }
  result <- do.call(rbind, rows)
  attr(result, "success_criterion") <- sprintf("CS-SEM mean recovery must be within %.3f of both comparator means.", tolerance)
  attr(result, "success") <- mean(result$cssem_recovery) >= mean(result$cbsem_ordinal_factor_proxy_recovery) - tolerance &&
    mean(result$cssem_recovery) >= mean(result$pls_composite_recovery) - tolerance
  result
}
