# Shared validated construction for cssem_model() and specify_measurement().
# Both front doors resolve to this identical internal representation, so every
# downstream consumer of a `cssem_model` object is unaffected by which front
# door built it.
.build_measurement <- function(constructs, folds) {
  if (!is.list(constructs) || is.null(names(constructs)) || any(names(constructs) == ""))
    stop("constructs must be a named list.", call. = FALSE)
  folds <- as.integer(folds)
  if (is.na(folds) || folds < 2L) stop("folds must be at least 2.", call. = FALSE)
  parsed <- lapply(constructs, function(x) {
    if (!is.list(x) || is.null(x$indicators) || is.null(x$scales))
      stop("Each construct needs indicators and scales.", call. = FALSE)
    indicators <- as.character(x$indicators)
    scales <- rep(as.character(x$scales), length.out = length(indicators))
    keys <- rep(if (is.null(x$keys)) 1 else as.integer(x$keys), length.out = length(indicators))
    if (!all(keys %in% c(-1L, 1L))) stop("keys must be -1 or 1.", call. = FALSE)
    if ("manifest" %in% scales) {
      if (length(indicators) != 1L || !identical(scales, "manifest"))
        stop("A manifest() construct declares exactly one indicator and one scale ('manifest').", call. = FALSE)
      reliability <- if (is.null(x$reliability)) 1 else as.numeric(x$reliability)
      if (!is.finite(reliability) || reliability <= 0 || reliability > 1)
        stop("manifest() reliability must be in (0, 1].", call. = FALSE)
      standardize <- if (is.null(x$standardize)) TRUE else isTRUE(x$standardize)
      return(list(indicators = indicators, scales = scales, keys = keys, manifest = TRUE,
        reliability = reliability, standardize = standardize))
    }
    if (length(indicators) < 2L || anyDuplicated(indicators))
      stop("Each construct needs at least two unique indicators.", call. = FALSE)
    if (!all(scales %in% c("ordinal", "continuous")))
      stop("scales must be 'ordinal', 'continuous', or (single-item) 'manifest'.", call. = FALSE)
    list(indicators = indicators, scales = scales, keys = keys, manifest = FALSE)
  })
  all_items <- unlist(lapply(parsed, `[[`, "indicators"), use.names = FALSE)
  if (anyDuplicated(all_items)) stop("An indicator may belong to only one v0.1 construct.", call. = FALSE)
  structure(list(constructs = parsed, folds = folds, version = "0.1"), class = "cssem_model")
}

#' Declare a CS-SEM measurement model
#'
#' **Deprecated.** Use [specify_measurement()], which declares the same
#' specification with [ordinal()]/[continuous()] helpers instead of nested
#' lists.
#'
#' Version 0.1 supports one-dimensional manifestation constructs only; an item
#' may belong to one construct only.
#'
#' @param constructs A named list of construct specifications. Each specification
#'   contains `indicators`, a character vector of column names; `scales`, either
#'   one scale or one per item (`"ordinal"` or `"continuous"`); and optional
#'   `keys`, item directions (`1` or `-1`).
#' @param folds Number of cross-fitting folds. Must be at least two.
#' @param preset Runtime preset. Use `"exploratory"` for lighter-weight model
#'   defaults while iterating locally.
#' @return An object of class `cssem_model`.
#' @examples
#' model <- cssem_model(list(
#'   Trust = list(indicators = c("trust_1", "trust_2"), scales = "ordinal")
#' ))
#' @family model specification functions
#' @export
cssem_model <- function(constructs, folds = 5L, preset = c("default", "exploratory")) {
  .Deprecated("specify_measurement", package = "cssem",
    msg = "cssem_model() is deprecated; use specify_measurement() with ordinal()/continuous() instead.")
  preset <- match.arg(preset)
  if (preset == "exploratory" && missing(folds)) folds <- 2L
  .build_measurement(constructs, folds)
}

.indicator_spec <- function(scale, ..., keys = NULL) {
  indicators <- c(...)
  if (!length(indicators) || !is.character(indicators))
    stop("Declare at least one character indicator column name.", call. = FALSE)
  list(indicators = indicators, scales = scale, keys = keys)
}

#' Declare ordinal indicators for a construct
#'
#' Bundles indicator column names, an `"ordinal"` scale, and optional
#' reverse-key directions for one construct, for use inside
#' [specify_measurement()].
#'
#' @param ... Character indicator column names. Accepts multiple names or a
#'   single character vector (e.g. from `paste0()`).
#' @param keys Optional item directions (`1` or `-1`), recycled to the number
#'   of indicators. Defaults to `1` for every item (no reverse-keying).
#' @return An indicator specification list, consumed by [specify_measurement()].
#' @family model specification functions
#' @export
ordinal <- function(..., keys = NULL) .indicator_spec("ordinal", ..., keys = keys)

#' Declare continuous indicators for a construct
#'
#' @inheritParams ordinal
#' @return An indicator specification list, consumed by [specify_measurement()].
#' @family model specification functions
#' @export
continuous <- function(..., keys = NULL) .indicator_spec("continuous", ..., keys = keys)

#' Declare a manifest (single-item, non-construct) covariate
#'
#' Declares one observed column as a direct structural covariate, bypassing
#' the measurement encoder entirely: the locked score is the (optionally
#' standardized) column itself, computed out-of-fold exactly like every
#' other construct's cross-fitted score, just with nothing estimated to
#' cross-fit. Use this for controls that are not themselves multi-item
#' constructs (age, a binary group indicator) or for a deliberately
#' single-item measure (e.g. a validated single-item life-satisfaction
#' scale) where an externally known reliability, not an internally
#' estimated one, is the right correction input.
#'
#' @param indicator A single character column name.
#' @param reliability Assumed reliability in `(0, 1]`, used by
#'   [associate()]'s errors-in-variables correction exactly as a measured
#'   construct's estimated reliability would be. Defaults to `1` (treated as
#'   measurement-error-free); supply an externally validated value (e.g.
#'   test-retest reliability) to disattenuate a single-item measure honestly.
#' @param standardize Standardize the column to mean 0, SD 1, like every
#'   other locked construct state, so structural coefficients stay
#'   comparable across the model. Set to `FALSE` to keep the variable's
#'   natural units.
#' @param keys Optional item direction (`1` or `-1`).
#' @return An indicator specification list, consumed by [specify_measurement()].
#' @family model specification functions
#' @export
manifest <- function(indicator, reliability = 1, standardize = TRUE, keys = NULL) {
  if (length(indicator) != 1L || !is.character(indicator))
    stop("manifest() takes exactly one character column name.", call. = FALSE)
  list(indicators = indicator, scales = "manifest",
    keys = if (is.null(keys)) 1 else keys, reliability = reliability, standardize = standardize)
}

#' Declare a CS-SEM measurement model with construct helpers
#'
#' Friendly front door for [cssem_model()]. Each construct is declared with
#' [ordinal()] or [continuous()], removing the need to spell out
#' `list(indicators = , scales = )` and its defaults by hand. Use
#' [manifest()] for a single-item covariate that should enter the structural
#' model directly rather than through a fitted measurement model.
#'
#' @param ... Named construct declarations, each an [ordinal()],
#'   [continuous()], or [manifest()] call.
#' @param folds Number of cross-fitting folds. Must be at least two.
#' @param preset Runtime preset. Use `"exploratory"` for lighter-weight model
#'   defaults while iterating locally.
#' @return An object of class `cssem_model`.
#' @examples
#' model <- specify_measurement(
#'   Trust = ordinal("trust_1", "trust_2")
#' )
#' @family model specification functions
#' @export
specify_measurement <- function(..., folds = 5L, preset = c("default", "exploratory")) {
  constructs <- list(...)
  if (!length(constructs) || is.null(names(constructs)) || any(names(constructs) == ""))
    stop("Every construct must be named, e.g. specify_measurement(Trust = ordinal(...)).", call. = FALSE)
  bad <- !vapply(constructs, function(x) is.list(x) && !is.null(x$indicators) && !is.null(x$scales), logical(1))
  if (any(bad)) stop("Each construct must be declared with ordinal() or continuous().", call. = FALSE)
  preset <- match.arg(preset)
  if (preset == "exploratory" && missing(folds)) folds <- 2L
  .build_measurement(constructs, folds)
}

#' Print a CS-SEM measurement model
#'
#' @param x A `cssem_model` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_model <- function(x, ...) {
  cat("CS-SEM measurement model (v", x$version, ")\n", sep = "")
  cat(length(x$constructs), "construct(s),", x$folds, "cross-fitting folds\n")
  invisible(x)
}
