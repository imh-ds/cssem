# Reusable measurement and outer-validation split specifications.

.split_values <- function(data, value, label) {
  if (length(value) == 1L && is.character(value) && value %in% names(data)) {
    out <- data[[value]]
  } else if (length(value) == nrow(data)) {
    out <- value
  } else {
    stop(sprintf("%s must be a column name or a vector with one value per row.", label), call. = FALSE)
  }
  if (anyNA(out)) stop(sprintf("%s contains missing values.", label), call. = FALSE)
  out
}

.split_fold_count <- function(folds, n) {
  if (length(folds) != 1L || !is.numeric(folds) || !is.finite(folds) ||
      folds != as.integer(folds) || folds < 2L)
    stop("folds must be a finite integer of at least 2.", call. = FALSE)
  folds <- as.integer(folds)
  if (folds > n) stop("folds cannot exceed the number of rows.", call. = FALSE)
  folds
}

.split_outer_table <- function(assignment, method) {
  levels <- sort(unique(assignment))
  if (method == "time") {
    outer_levels <- levels[-1L]
    train_ids <- lapply(outer_levels, function(k) which(assignment < k))
    test_ids <- lapply(outer_levels, function(k) which(assignment == k))
  } else {
    outer_levels <- levels
    train_ids <- lapply(outer_levels, function(k) which(assignment != k))
    test_ids <- lapply(outer_levels, function(k) which(assignment == k))
  }
  if (!length(outer_levels) || any(!vapply(train_ids, length, integer(1))) ||
      any(!vapply(test_ids, length, integer(1))))
    stop("Every split must have non-empty training and test partitions.", call. = FALSE)
  data.frame(outer_id = seq_along(outer_levels),
    train_ids = I(train_ids), test_ids = I(test_ids),
    train_n = vapply(train_ids, length, integer(1)),
    test_n = vapply(test_ids, length, integer(1)),
    stringsAsFactors = FALSE)
}

#' Create reusable measurement and outer-validation splits
#'
#' @param data A data frame whose rows receive split IDs.
#' @param method Split design: `"random"`, `"group"`, or `"time"`.
#' @param folds Number of fold intervals. Time designs use the first interval
#'   only for training and return outer partitions for later intervals.
#' @param group A column name or vector identifying repeated entities. Repeated
#'   labels are allowed but one label is never split across folds.
#' @param time A column name or vector defining temporal order. Equal values
#'   remain in the same time block.
#' @param seed Seed for random and group assignments.
#' @return A `cssem_splits` object with an integer `assignment`, reusable outer
#'   train/test row IDs, and provenance.
#' @export
make_splits <- function(data, method = c("random", "group", "time"), folds = 5L,
                        group = NULL, time = NULL, seed = 1L) {
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  n <- nrow(data)
  if (!n) stop("data must contain at least one row.", call. = FALSE)
  method <- match.arg(method)
  folds <- .split_fold_count(folds, n)
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed))
    stop("seed must be a finite numeric scalar.", call. = FALSE)
  .preserve_seed(); set.seed(seed)
  source_group <- NULL; source_time <- NULL
  if (method == "group") {
    if (is.null(group)) stop("group is required for method = \"group\".", call. = FALSE)
    group_values <- .split_values(data, group, "group")
    if (length(unique(group_values)) < folds) stop("group must contain at least one group per fold.", call. = FALSE)
    groups <- unique(group_values)
    group_assignment <- sample(rep(seq_len(folds), length.out = length(groups)))
    assignment <- group_assignment[match(group_values, groups)]
    source_group <- if (length(group) == 1L && is.character(group)) group else "<vector>"
  } else if (method == "time") {
    if (is.null(time)) stop("time is required for method = \"time\".", call. = FALSE)
    time_values <- .split_values(data, time, "time")
    if (!is.numeric(time_values) && !inherits(time_values, c("Date", "POSIXct", "POSIXlt")))
      stop("time must be numeric, Date, or POSIXct values.", call. = FALSE)
    if (any(!is.finite(time_values))) stop("time must contain only finite values.", call. = FALSE)
    ordered_values <- sort(unique(time_values))
    if (length(ordered_values) < folds) stop("time must contain at least one unique value per fold.", call. = FALSE)
    time_blocks <- cut(seq_along(ordered_values), breaks = folds, labels = FALSE,
      include.lowest = TRUE)
    assignment <- as.integer(time_blocks[match(time_values, ordered_values)])
    source_time <- if (length(time) == 1L && is.character(time)) time else "<vector>"
  } else {
    assignment <- sample(rep(seq_len(folds), length.out = n))
  }
  assignment <- as.integer(assignment)
  outer <- .split_outer_table(assignment, method)
  provenance <- data.frame(method = method, folds = folds, seed = seed,
    group = if (is.null(source_group)) NA_character_ else source_group,
    time = if (is.null(source_time)) NA_character_ else source_time,
    stringsAsFactors = FALSE)
  structure(list(method = method, folds = folds, seed = seed,
    assignment = assignment, row_ids = seq_len(n), outer = outer,
    provenance = provenance), class = c("cssem_splits", "list"))
}

.resolve_split_assignment <- function(split, data, default_folds) {
  if (is.null(split)) return(NULL)
  n <- nrow(data)
  if (inherits(split, "cssem_splits")) {
    if (!identical(as.integer(split$row_ids), seq_len(n)))
      stop("split row IDs must match the supplied data.", call. = FALSE)
    assignment <- split$assignment
    metadata <- split
  } else {
    assignment <- split
    metadata <- list(method = "explicit", seed = NA_real_, provenance = data.frame(
      method = "explicit", folds = NA_integer_, seed = NA_real_,
      group = NA_character_, time = NA_character_, stringsAsFactors = FALSE))
  }
  if (!is.numeric(assignment) || length(assignment) != n || any(!is.finite(assignment)) ||
      any(assignment != as.integer(assignment)) || any(assignment < 1L))
    stop("split must provide one positive integer assignment per row.", call. = FALSE)
  assignment <- as.integer(assignment)
  levels <- sort(unique(assignment))
  if (length(levels) < 2L || !identical(levels, seq_len(length(levels))))
    stop("split must contain consecutive assignments for at least two folds.", call. = FALSE)
  if (!is.null(default_folds) && length(levels) != as.integer(default_folds))
    stop("split fold count must match model$folds.", call. = FALSE)
  list(method = if (is.null(metadata$method)) "explicit" else metadata$method,
    assignment = assignment, row_ids = seq_len(n), metadata = metadata)
}

#' @export
print.cssem_splits <- function(x, ...) {
  cat("CS-SEM splits: ", x$method, " (", x$folds, " folds)\n", sep = "")
  if (!is.null(x$outer) && nrow(x$outer)) print(x$outer[, c("outer_id", "train_n", "test_n"), drop = FALSE], row.names = FALSE)
  invisible(x)
}
