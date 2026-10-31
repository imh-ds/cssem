# Known-equation latent generators for the mediation and moderated-mediation
# harnesses. Each generator lists its exogenous constructs (standard normal) and
# its endogenous nodes in causal order, each a structural function of earlier
# constructs plus independent normal noise. Every endogenous node is
# standardized with population constants rather than sample moments, so the
# simulated states and the population targets share one metric.
#
# Population targets are unit-level counterfactuals computed from the true
# equations on a large fixed reference draw: shift the treatment by `delta`,
# keep each unit's own noise, and propagate. They use no fitted model and no
# package propagation code, so they are independent of the estimator, and they
# are population quantities rather than sample oracles.

.population_draws <- 1000000L
.population_seed <- 20261031L
.generator_cache <- new.env(parent = emptyenv())

# Draw exogenous constructs, then one noise vector per endogenous node.
.generator_draw <- function(generator, n) {
  exogenous <- lapply(stats::setNames(generator$exogenous, generator$exogenous), function(name) stats::rnorm(n))
  noise <- lapply(generator$nodes, function(node) stats::rnorm(n, sd = generator$noise_sd))
  list(exogenous = exogenous, noise = noise)
}

# Population centring and scaling constants for each endogenous node, computed
# in causal order because downstream equations take standardized parents.
.generator_constants <- function(generator, draw) {
  frame <- draw$exogenous; centre <- numeric(0); spread <- numeric(0)
  for (node in names(generator$nodes)) {
    raw <- generator$nodes[[node]](frame) + draw$noise[[node]]
    centre[[node]] <- mean(raw); spread[[node]] <- stats::sd(raw)
    frame[[node]] <- (raw - centre[[node]]) / spread[[node]]
  }
  list(centre = centre, scale = spread)
}

# Evaluate the structural equations. `fixed` sets exogenous constructs to a
# constant (a moderator level); `shift` is added to `x`. Nodes in `sees` are
# recomputed from the current frame; the rest keep their `natural` values, which
# is how a direct-effect arm holds mediators at their unshifted values.
.generator_propagate <- function(generator, draw, constants, x = "X", shift = 0,
                                 sees = names(generator$nodes), natural = NULL, fixed = list()) {
  frame <- draw$exogenous
  for (name in names(fixed)) frame[[name]] <- rep(fixed[[name]], length(frame[[name]]))
  frame[[x]] <- frame[[x]] + shift
  for (node in names(generator$nodes)) {
    frame[[node]] <- if (node %in% sees)
      (generator$nodes[[node]](frame) + draw$noise[[node]] - constants$centre[[node]]) / constants$scale[[node]]
    else natural[[node]]
  }
  frame
}

# Average total and natural direct effects of a `delta` shift in x on y; the
# indirect effect is their difference. This matches the engine's decomposition:
# the direct arm moves only the x -> y edge with mediators at baseline.
.generator_effects <- function(generator, draw, constants, x, y, delta = 1, fixed = list()) {
  natural <- .generator_propagate(generator, draw, constants, x, 0, fixed = fixed)
  shifted <- .generator_propagate(generator, draw, constants, x, delta, fixed = fixed)
  direct_arm <- .generator_propagate(generator, draw, constants, x, delta, sees = y, natural = natural, fixed = fixed)
  total <- mean(shifted[[y]] - natural[[y]]) / delta
  direct <- mean(direct_arm[[y]] - natural[[y]]) / delta
  c(total = total, direct = direct, indirect_total = total - direct)
}

# Simulate one sample's latent states on the population metric.
.generator_states <- function(generator, n, constants) {
  draw <- .generator_draw(generator, n)
  frame <- .generator_propagate(generator, draw, constants)
  as.data.frame(frame[c(generator$exogenous, names(generator$nodes))])
}

# Build a population summary once per session (and per PSOCK worker), with a
# fixed seed and without disturbing the caller's random stream.
.generator_cached <- function(key, build) {
  if (is.null(.generator_cache[[key]])) {
    .preserve_seed()
    set.seed(.population_seed)
    assign(key, build(), envir = .generator_cache)
  }
  .generator_cache[[key]]
}
