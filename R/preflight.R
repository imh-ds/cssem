# Structured model/data checks used both by users and before fitting.

.preflight_columns <- c("severity", "stage", "construct", "item", "row", "fold",
  "code", "message", "action")

.preflight_issue <- function(severity = "error", stage, code, message, action,
                            construct = NA_character_, item = NA_character_,
                            row = NA_integer_, fold = NA_integer_) {
  data.frame(severity = as.character(severity), stage = as.character(stage),
    construct = as.character(construct), item = as.character(item),
    row = as.integer(row), fold = as.integer(fold), code = as.character(code),
    message = as.character(message), action = as.character(action),
    stringsAsFactors = FALSE)
}

.preflight_empty <- function(class = "cssem_preflight") {
  out <- data.frame(severity = character(), stage = character(), construct = character(),
    item = character(), row = integer(), fold = integer(), code = character(),
    message = character(), action = character(), stringsAsFactors = FALSE)
  class(out) <- c(class, "cssem_preflight", "data.frame")
  out
}

.preflight_table <- function(rows, class) {
  if (!length(rows)) return(.preflight_empty(class))
  out <- do.call(rbind, rows)
  out <- out[, .preflight_columns, drop = FALSE]
  class(out) <- c(class, "cssem_preflight", "data.frame")
  out
}

.preflight_add <- function(rows, ...) c(rows, list(.preflight_issue(...)))

.preflight_model_names <- function(model) {
  if (!is.list(model$constructs) || is.null(names(model$constructs))) return(character())
  names(model$constructs)
}

#' Check a measurement model and optional structural graph before fitting
#'
#' Returns every detected issue with stable severity, location, code, and
#' recovery guidance. A valid model produces a zero-row `cssem_model_check`.
#' Structural checks are included when `structure` is supplied.
#'
#' @param model A `cssem_model` object (including a manually constructed object
#'   when auditing malformed specifications).
#' @param structure Optional `cssem_structure` object to validate against the
#'   model's construct names and temporal order.
#' @return A data frame of class `cssem_model_check` and `cssem_preflight`.
#' @export
check_model <- function(model, structure = NULL) {
  rows <- list()
  add <- function(...) rows <<- .preflight_add(rows, ...)
  if (!inherits(model, "cssem_model")) {
    add(stage = "model", code = "invalid_model_class",
      message = "model must be a cssem_model object.",
      action = "Build the model with specify_measurement() or cssem_model().")
    return(.preflight_table(rows, "cssem_model_check"))
  }
  constructs <- model$constructs
  if (!is.list(constructs) || is.null(names(constructs)) || !length(constructs)) {
    add(stage = "model", code = "empty_constructs",
      message = "The measurement model has no named constructs.",
      action = "Declare at least one named ordinal(), continuous(), or manifest() construct.")
    return(.preflight_table(rows, "cssem_model_check"))
  }
  construct_names <- names(constructs)
  if (any(!nzchar(construct_names)) || anyDuplicated(construct_names))
    add(stage = "model", code = "invalid_construct_names",
      message = "Construct names must be non-empty and unique.",
      action = "Rename duplicate or empty construct declarations.")
  folds <- model$folds
  if (length(folds) != 1L || !is.numeric(folds) || !is.finite(folds) ||
      folds != as.integer(folds) || folds < 2L)
    add(stage = "model", code = "invalid_folds",
      message = "folds must be a whole number of at least two.",
      action = "Set folds to an integer >= 2 and leave enough rows for every fold.")
  all_items <- character()
  for (nm in construct_names) {
    spec <- constructs[[nm]]
    if (!is.list(spec)) {
      add(stage = "model", construct = nm, code = "invalid_construct",
        message = "Construct declaration is not a list.",
        action = "Use ordinal(), continuous(), or manifest() to build the declaration.")
      next
    }
    indicators <- spec$indicators
    if (is.null(indicators) || !is.character(indicators) || !length(indicators) ||
        any(!nzchar(indicators))) {
      add(stage = "model", construct = nm, code = "invalid_indicators",
        message = "Indicators must be a non-empty character vector.",
        action = "Supply unique data-frame column names in the construct declaration.")
      next
    }
    if (anyDuplicated(indicators))
      add(stage = "model", construct = nm, code = "duplicate_indicator",
        item = indicators[which(duplicated(indicators))[1L]],
        message = "A construct repeats an indicator column.",
        action = "Declare each indicator once.")
    scales <- as.character(spec$scales)
    if (!length(scales) || any(!scales %in% c("ordinal", "continuous", "manifest")))
      add(stage = "model", construct = nm, code = "invalid_scale",
        message = "Each item scale must be ordinal, continuous, or manifest.",
        action = "Use ordinal()/continuous(), or one single-item manifest() declaration.")
    scales <- rep(scales, length.out = length(indicators))
    keys <- spec$keys
    if (is.null(keys)) keys <- rep(1L, length(indicators)) else keys <- rep(keys, length.out = length(indicators))
    if (any(!is.numeric(keys) & !is.integer(keys)) || any(!keys %in% c(-1, 1)))
      add(stage = "model", construct = nm, code = "invalid_key",
        message = "keys must contain only -1 or 1.",
        action = "Set reverse-key directions explicitly with keys = -1 or keys = 1.")
    is_manifest <- any(!is.na(scales) & scales == "manifest")
    if (is_manifest && (length(indicators) != 1L || !all(scales == "manifest")))
      add(stage = "model", construct = nm, code = "invalid_manifest_block",
        message = "A manifest construct must contain exactly one manifest indicator.",
        action = "Use manifest('item', ...) for one observed covariate or declare a multi-item encoder.")
    if (!is_manifest && length(indicators) < 2L)
      add(stage = "model", construct = nm, code = "insufficient_indicators",
        message = "A fitted construct needs at least two indicators.",
        action = "Add an indicator or declare a single observed variable with manifest().")
    all_items <- c(all_items, indicators)
  }
  if (anyDuplicated(all_items)) {
    duplicate <- unique(all_items[duplicated(all_items)])[1L]
    add(stage = "model", item = duplicate, code = "cross_construct_indicator",
      message = "An indicator is assigned to more than one construct.",
      action = "Assign each observed column to exactly one construct.")
  }
  if (!is.null(structure)) {
    if (!inherits(structure, "cssem_structure")) {
      add(stage = "structure", code = "invalid_structure_class",
        message = "structure must be a cssem_structure object.",
        action = "Build structural declarations with specify_structure().")
    } else {
      effects <- structure$effects
      if (!is.list(effects) || is.null(names(effects))) {
        add(stage = "structure", code = "invalid_structure_effects",
          message = "The structural declaration has no named outcome effects.",
          action = "Declare one or more Outcome ~ predictor formulas.")
      } else {
        predictors <- unlist(lapply(effects, names), use.names = FALSE)
        declared <- unique(c(names(effects), unlist(lapply(predictors, .predictor_constructs), use.names = FALSE)))
        unknown <- setdiff(declared, construct_names)
        for (node in unknown)
          add(stage = "structure", construct = node, code = "unknown_structural_node",
            message = paste0("Structural declaration references unknown construct '", node, "'."),
            action = "Match every outcome and predictor to a declared measurement construct.")
        if (!any(declared %in% construct_names)) {
          # The unknown-node rows above are the useful report; avoid a second
          # graph error when no structural node can be evaluated.
        } else {
          order <- structure$order
          if (!is.null(order) && (anyDuplicated(order) || any(!nzchar(order))))
            add(stage = "structure", code = "invalid_temporal_order",
              message = "The declared structural order must contain unique, non-empty names.",
              action = "Provide each construct once in order from predictors to outcomes.")
          if (!is.null(order) && !setequal(order, construct_names))
            add(stage = "structure", code = "incomplete_temporal_order",
              message = "The declared structural order does not cover every measurement construct.",
              action = "Include every construct in order, or omit order for an acyclic graph.")
          resolved <- tryCatch(.resolve_temporal_order(structure, construct_names), error = function(e) e)
          if (inherits(resolved, "error"))
            add(stage = "structure", code = "invalid_graph_constraints", message = conditionMessage(resolved),
              action = "Remove cycles and place each predictor before its outcome in order.")
        }
      }
    }
  }
  .preflight_table(rows, "cssem_model_check")
}

.preflight_folds <- function(folds, n, default_folds) {
  if (is.null(folds)) {
    if (!is.numeric(default_folds) || length(default_folds) != 1L || !is.finite(default_folds)) return(NULL)
    return(rep(seq_len(as.integer(default_folds)), length.out = n))
  }
  if (length(folds) == 1L && is.numeric(folds) && is.finite(folds) && folds == as.integer(folds) && folds >= 2L)
    return(rep(seq_len(as.integer(folds)), length.out = n))
  if (length(folds) == n && all(is.finite(folds)) && all(folds == as.integer(folds))) return(as.integer(folds))
  NULL
}

#' Check data compatibility and information support before fitting
#'
#' The returned table includes global and fold-level support, missingness,
#' scale failures, and zero-information rows. Warnings are reported as rows and
#' do not force fitting to stop; errors are the conditions that `fit_states()`
#' rejects before allocating encoders.
#'
#' @param data A data frame containing declared indicators.
#' @param model A `cssem_model` object.
#' @param folds Optional fold count or explicit fold assignment. When omitted,
#'   a deterministic diagnostic assignment is used; `fit_states()` still uses
#'   its seeded random assignment.
#' @return A data frame of class `cssem_data_check` and `cssem_preflight`.
#' @export
check_data <- function(data, model, folds = NULL) {
  rows <- list()
  add <- function(...) rows <<- .preflight_add(rows, ...)
  model_check <- check_model(model)
  if (nrow(model_check)) rows <- c(rows,
    lapply(seq_len(nrow(model_check)), function(i) model_check[i, , drop = FALSE]))
  if (!is.data.frame(data)) {
    add(stage = "data", code = "invalid_data_class", message = "data must be a data frame.",
      action = "Convert the input to a data.frame with one column per declared indicator.")
    return(.preflight_table(rows, "cssem_data_check"))
  }
  if (anyDuplicated(names(data)))
    add(stage = "data", code = "duplicate_data_column", message = "data contains duplicate column names.",
      action = "Make column names unique before fitting.")
  if (any(model_check$severity == "error"))
    return(.preflight_table(rows, "cssem_data_check"))
  n <- nrow(data)
  if (n < 1L)
    add(stage = "data", code = "empty_data", message = "data has no rows.",
      action = "Supply observed rows for fitting.")
  construct_names <- .preflight_model_names(model)
  if (!length(construct_names)) return(.preflight_table(rows, "cssem_data_check"))
  needed <- unlist(lapply(model$constructs, `[[`, "indicators"), use.names = FALSE)
  missing <- setdiff(needed, names(data))
  if (length(missing))
    for (item in missing)
      add(stage = "data", item = item, code = "missing_indicator",
        message = paste0("data is missing declared indicators: ", paste(missing, collapse = ", "), "."),
        action = "Add the declared columns or revise the measurement model.")
  fold_id <- .preflight_folds(folds, n, model$folds)
  if (is.null(fold_id)) {
    add(stage = "data", code = "invalid_fold_assignment",
      message = "folds must be a count >= 2 or an integer assignment with one value per row.",
      action = "Use model$folds or provide an explicit complete fold vector.")
  } else {
    fold_levels <- sort(unique(fold_id))
    if (length(fold_levels) < 2L)
      add(stage = "data", code = "insufficient_folds", message = "At least two validation folds are required.",
        action = "Increase the fold count or provide a multi-fold assignment.")
    if (length(fold_levels) < as.integer(model$folds))
      add(stage = "data", code = "folds_exceed_rows", message = "The data cannot populate every requested fold.",
        action = "Reduce folds or provide more rows.")
    if (length(fold_levels) && min(tabulate(fold_id, nbins = max(fold_levels))) < 4L)
      add(severity = "warning", stage = "data", code = "small_fold",
        message = "At least one validation fold has fewer than four rows.",
        action = "Increase the sample or reduce folds; sparse folds can destabilize scores.")
  }
  if (length(missing)) return(.preflight_table(rows, "cssem_data_check"))
  for (nm in construct_names) {
    spec <- model$constructs[[nm]]
    indicators <- spec$indicators; scales <- rep(as.character(spec$scales), length.out = length(indicators))
    keys <- if (is.null(spec$keys)) rep(1L, length(indicators)) else rep(spec$keys, length.out = length(indicators))
    observed_block <- rep(FALSE, n)
    for (j in seq_along(indicators)) {
      item <- indicators[[j]]; scale <- scales[[j]]; raw <- data[[item]]
      observed <- !is.na(raw); observed_block <- observed_block | observed
      values <- tryCatch({
        if (scale == "ordinal") .prepare_item(raw, scale, keys[[j]]) else .as_numeric_values(raw, scale)
      }, error = function(e) e)
      if (inherits(values, "error")) {
        add(stage = "data", construct = nm, item = item, code = "invalid_item_values",
          message = conditionMessage(values), action = "Correct the item values or declare the appropriate scale.")
        next
      }
      if (scale == "ordinal") {
        level_values <- values$y[!is.na(values$y)]
        if (length(unique(level_values)) < 2L)
          add(stage = "data", construct = nm, item = item, code = "zero_information_item",
            message = "An ordinal item has fewer than two observed categories.",
            action = "Provide item variation or remove the item from the fitted construct.")
        if (length(level_values)) {
          proportions <- tabulate(level_values, nbins = length(values$levels)) / length(level_values)
          if (min(proportions) < .05)
            add(severity = "warning", stage = "data", construct = nm, item = item, code = "sparse_category",
              message = "An observed ordinal category has under 5% support.",
              action = "Inspect category collapse, sample size, and fold allocation before interpreting parameters.")
          if (!is.null(fold_id) && length(unique(fold_id)) > 1L) for (fold in sort(unique(fold_id))) {
            train <- level_values[fold_id[!is.na(raw)] != fold]
            if (length(train) && any(tabulate(train, nbins = length(values$levels)) == 0L))
              add(severity = "warning", stage = "data", construct = nm, item = item, fold = fold,
                code = "fold_missing_category", message = "A training fold lacks at least one observed ordinal category.",
                action = "Use more rows per fold or collapse unsupported categories.")
          }
        }
      } else if (sum(is.finite(values)) < 2L) {
        add(stage = "data", construct = nm, item = item, code = "zero_information_item",
          message = "A numeric item has fewer than two finite observations.",
          action = "Provide at least two finite values or remove the item.")
      }
    }
    if (!any(observed_block))
      add(stage = "data", construct = nm, code = "zero_information_construct",
        message = "No declared item is observed for this construct.",
        action = "Supply observed indicators or omit the construct from this fit.")
    no_item <- which(!observed_block)
    if (length(no_item)) for (row in no_item)
      add(severity = "warning", stage = "data", construct = nm, row = row,
        code = "zero_information_row", message = "A row has no observed indicators for this construct.",
        action = "Review item nonresponse; the encoder will rely on its prior for this row.")
  }
  .preflight_table(rows, "cssem_data_check")
}

.preflight_stop <- function(check, prefix = "Preflight checks failed") {
  errors <- check[check$severity == "error", , drop = FALSE]
  if (!nrow(errors)) return(invisible(NULL))
  detail <- paste(sprintf("[%s] %s (action: %s)", errors$code, errors$message, errors$action), collapse = "\n")
  stop(paste0(prefix, ":\n", detail), call. = FALSE)
}

#' @export
print.cssem_model_check <- function(x, ...) {
  cat("CS-SEM model preflight: ", nrow(x), " issue(s)\n", sep = "")
  if (nrow(x)) print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
print.cssem_data_check <- function(x, ...) {
  cat("CS-SEM data preflight: ", nrow(x), " issue(s)\n", sep = "")
  if (nrow(x)) print.data.frame(x, row.names = FALSE)
  invisible(x)
}
