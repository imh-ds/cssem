.constraint_edge <- function(value) {
  value <- trimws(as.character(value))
  if (length(value) != 1L || is.na(value) || !grepl("^[^~[:space:]]+[~][^~[:space:]]+$", value))
    stop("Constraint edges must use the declared outcome~predictor form.", call. = FALSE)
  parts <- strsplit(value, "~", fixed = TRUE)[[1L]]
  c(outcome = parts[[1L]], predictor = parts[[2L]], edge = paste(parts, collapse = "~"))
}

#' Declare narrow linear structural coefficient constraints
#'
#' @param equal A list of character edge groups constrained to have equal
#'   locked-score slopes.
#' @param fixed A named numeric vector of fixed locked-score coefficients.
#' @return A `cssem_constraint` object.
#' @export
cssem_constraint <- function(equal = list(), fixed = numeric()) {
  if (!is.list(equal)) stop("equal must be a list of edge groups.", call. = FALSE)
  groups <- lapply(equal, function(group) {
    group <- vapply(group, function(edge) .constraint_edge(edge)[["edge"]], character(1))
    if (length(group) < 2L) stop("Each equality group must contain at least two distinct edges.", call. = FALSE)
    if (anyDuplicated(group)) stop("An equality group contains a duplicate edge.", call. = FALSE)
    group
  })
  if (length(fixed)) {
    if (!is.numeric(fixed) || is.null(names(fixed)) || any(!nzchar(names(fixed))) || anyDuplicated(names(fixed)))
      stop("fixed must be a named numeric vector with unique edge names.", call. = FALSE)
    if (any(!is.finite(fixed))) stop("fixed values must be finite numeric scalars.", call. = FALSE)
    fixed_names <- vapply(names(fixed), function(edge) .constraint_edge(edge)[["edge"]], character(1))
    names(fixed) <- fixed_names
  } else fixed <- numeric()
  equal_edges <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(equal_edges)) stop("An edge appears in more than one equality group.", call. = FALSE)
  if (length(intersect(equal_edges, names(fixed)))) stop("An edge cannot be both equal-constrained and fixed.", call. = FALSE)
  structure(list(equal = groups, fixed = fixed, edges = unique(c(equal_edges, names(fixed))), active = length(equal_edges) || length(fixed)),
    class = "cssem_constraint")
}

.validate_constraint_targets <- function(constraints, structure) {
  if (is.null(constraints)) return(NULL)
  if (!inherits(constraints, "cssem_constraint")) stop("constraints must be a cssem_constraint() object.", call. = FALSE)
  declared <- unlist(lapply(names(structure$effects), function(outcome)
    paste(outcome, names(structure$effects[[outcome]]), sep = "~")), use.names = FALSE)
  unknown <- setdiff(constraints$edges, declared)
  if (length(unknown)) stop(sprintf("Constraint references unknown structural edge(s): %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  constraints
}

.constraint_apply <- function(models, scores, constraints) {
  if (is.null(constraints) || !isTRUE(constraints$active)) return(list(models = models,
    diagnostics = list(active = FALSE), declarations = constraints))
  if (any(vapply(models, function(model) any(unname(model$shapes) != "linear"), logical(1))))
    stop("Constraints require every selected structural edge to be linear; nonlinear and product equations are unsupported.", call. = FALSE)
  for (edge in constraints$edges) {
    parts <- .constraint_edge(edge); model <- models[[parts[["outcome"]]]]
    shape <- model$shapes[[parts[["predictor"]]]]
    if (!identical(shape, "linear")) stop(sprintf("Constraint edge %s has selected shape '%s'; only selected linear edges are supported.", edge, shape), call. = FALSE)
    if (.is_interaction(parts[["predictor"]])) stop("Constraints do not support interaction structural edges.", call. = FALSE)
  }
  outcomes <- names(models); widths <- vapply(models, function(model) 1L + length(model$shapes), integer(1)); offsets <- c(0L, cumsum(widths))
  p <- sum(widths); labels <- character(p); design <- matrix(0, nrow(scores) * length(outcomes), p); response <- numeric(nrow(design)); cursor <- 0L
  for (i in seq_along(outcomes)) {
    outcome <- outcomes[[i]]; predictors <- names(models[[outcome]]$shapes); indices <- (offsets[[i]] + 1L):offsets[[i + 1L]]
    labels[indices] <- c(paste0(outcome, "~(Intercept)"), paste0(outcome, "~", predictors))
    rows <- cursor + seq_len(nrow(scores)); design[rows, indices] <- cbind(1, as.matrix(scores[, predictors, drop = FALSE])); response[rows] <- scores[[outcome]]; cursor <- cursor + nrow(scores)
  }
  edge_index <- setNames(seq_along(labels), labels)
  restrictions <- list(); values <- numeric()
  for (group in constraints$equal) for (edge in group[-1L]) {
    row <- numeric(p); row[[edge_index[[edge]]]] <- 1; row[[edge_index[[group[[1L]]]]]] <- -1
    restrictions[[length(restrictions) + 1L]] <- row; values <- c(values, 0)
  }
  for (edge in names(constraints$fixed)) {
    row <- numeric(p); row[[edge_index[[edge]]]] <- 1
    restrictions[[length(restrictions) + 1L]] <- row; values <- c(values, unname(constraints$fixed[[edge]]))
  }
  R <- do.call(rbind, restrictions); XtX <- crossprod(design); Xty <- crossprod(design, response)
  kkt <- rbind(cbind(XtX, t(R)), cbind(R, matrix(0, nrow(R), nrow(R))))
  if (qr(kkt)$rank < nrow(kkt)) stop("Constrained least-squares system is rank deficient.", call. = FALSE)
  solved <- tryCatch(solve(kkt, c(Xty, values)), error = function(e) NULL)
  if (is.null(solved) || any(!is.finite(solved))) stop("Constrained least-squares system could not be solved.", call. = FALSE)
  beta <- solved[seq_len(p)]
  for (i in seq_along(outcomes)) {
    indices <- (offsets[[i]] + 1L):offsets[[i + 1L]]
    models[[outcomes[[i]]]]$coefficient <- stats::setNames(beta[indices], c("(Intercept)", names(models[[outcomes[[i]]]]$shapes)))
  }
  diagnostics <- list(active = TRUE, optimizer = "deterministic KKT pooled least squares", rank = qr(kkt)$rank,
    condition_number = tryCatch(kappa(kkt), error = function(e) NA_real_), restriction_rank = qr(R)$rank,
    restrictions = nrow(R), status = "solved", limitation = "locked-score linear associational constraints; no EIV, weighting, measurement equality, or nonlinear support")
  list(models = models, diagnostics = diagnostics, declarations = constraints)
}

.constraint_effect_rows <- function(rows, model) {
  for (predictor in names(model$shapes)) {
    index <- which(rows$predictor == predictor & is.na(rows$x))
    if (length(index) && identical(model$shapes[[predictor]], "linear")) {
      rows$naive_estimate[index] <- model$coefficient[model$maps[[predictor]]][1L]
      rows$corrected_estimate[index] <- NA_real_; rows$corrected_ci_low[index] <- NA_real_; rows$corrected_ci_high[index] <- NA_real_
      rows$eiv_applicable[index] <- FALSE; rows$eiv_diagnostic[index] <- "EIV correction disabled for constrained locked-score estimates."
    }
  }
  rows
}
