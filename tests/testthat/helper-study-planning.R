make_gaussian_slope_spec <- function(n = 120L) {
  scenarios <- data.frame(scenario = "base", beta = .30)
  generate <- function(scenario, n, seed) {
    x <- seq_len(n) - mean(seq_len(n))
    x <- x * sqrt(n / sum(x^2))
    data.frame(x = x, y = scenario$beta * x + stats::rnorm(n))
  }
  truth <- function(scenario, n) c(beta = scenario$beta)
  analyze <- function(data, scenario, seed) {
    estimate <- sum(data$x * data$y) / sum(data$x^2)
    list(status = "completed", converged = TRUE, failure_reason = "",
      estimates = data.frame(estimand = "beta", estimate = estimate,
        estimate_status = "available", lower = NA_real_, upper = NA_real_,
        interval_status = "unavailable",
        status_reason = "analytic-oracle fixture does not estimate intervals"))
  }
  study_spec(scenarios, as.integer(n), generate, truth, analyze,
    metadata = list(description = "Gaussian no-intercept slope",
      analysis_scope = "known_design no-intercept OLS"))
}

make_design_fixture <- function(n = 1000L, seed = 502L) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) prior_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", prior_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  latent <- stats::rnorm(n)
  manifest_control <- .35 * latent + sqrt(1 - .35^2) * stats::rnorm(n)
  latent_proxy <- latent + stats::rnorm(n, sd = .5)
  continuous_item_1 <- .8 * latent + sqrt(1 - .8^2) * stats::rnorm(n)
  continuous_item_2 <- .7 * latent + sqrt(1 - .7^2) * stats::rnorm(n)
  ordinal_item_1 <- ordered(cut(latent_proxy, c(-Inf, -.7, 0, .7, Inf),
    labels = FALSE), levels = 1:4)
  skewed_state <- exp(latent / 3)
  product_term <- manifest_control * latent_proxy
  mcar_item <- latent_proxy
  mcar_item[stats::runif(n) < .10] <- NA_real_
  mar_item <- latent_proxy
  mar_probability <- stats::plogis(-1.8 + .65 * as.numeric(scale(manifest_control)))
  mar_item[stats::runif(n) < mar_probability] <- NA_real_
  data.frame(continuous_item_1, continuous_item_2, ordinal_item_1,
    manifest_control, latent_proxy, product_term, skewed_state,
    mcar_item, mar_item, stringsAsFactors = FALSE)
}
