#' Declare a CS-SEM measurement model
#'
#' Creates the theory-declared measurement specification used by [cssem_fit()].
#' Version 0.1 supports one-dimensional manifestation constructs only; an item
#' may belong to one construct only.
#'
#' @param constructs A named list of construct specifications. Each specification
#'   contains `indicators`, a character vector of column names; `scales`, either
#'   one scale or one per item (`"ordinal"` or `"continuous"`); and optional
#'   `keys`, item directions (`1` or `-1`).
#' @param folds Number of cross-fitting folds. Must be at least two.
#' @return An object of class `cssem_model`.
#' @examples
#' model <- cssem_model(list(
#'   Trust = list(indicators = c("trust_1", "trust_2"), scales = "ordinal")
#' ))
#' @family model specification functions
#' @export
cssem_model <- function(constructs, folds = 5L) {
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
    if (length(indicators) < 2L || anyDuplicated(indicators))
      stop("Each construct needs at least two unique indicators.", call. = FALSE)
    if (!all(scales %in% c("ordinal", "continuous")))
      stop("scales must be 'ordinal' or 'continuous'.", call. = FALSE)
    if (!all(keys %in% c(-1L, 1L))) stop("keys must be -1 or 1.", call. = FALSE)
    list(indicators = indicators, scales = scales, keys = keys)
  })
  all_items <- unlist(lapply(parsed, `[[`, "indicators"), use.names = FALSE)
  if (anyDuplicated(all_items)) stop("An indicator may belong to only one v0.1 construct.", call. = FALSE)
  structure(list(constructs = parsed, folds = folds, version = "0.1"), class = "cssem_model")
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
