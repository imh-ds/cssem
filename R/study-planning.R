.study_plain_value <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.list(x)) {
    if (is.object(x)) return(FALSE)
    if (!length(x)) return(TRUE)
    value_names <- names(x)
    if (is.null(value_names) || anyNA(value_names) || any(!nzchar(value_names)) ||
        anyDuplicated(value_names)) return(FALSE)
    return(all(vapply(x, .study_plain_value, logical(1))))
  }
  if (!is.atomic(x) || is.object(x)) return(FALSE)
  attributes <- attributes(x)
  if (is.null(attributes)) return(TRUE)
  identical(names(attributes), "names")
}

.study_validate_callback <- function(callback, name, required) {
  if (!is.function(callback)) {
    stop(sprintf("%s must be a function.", name), call. = FALSE)
  }
  callback_formals <- names(formals(callback))
  if (is.null(callback_formals)) callback_formals <- character()
  if (!all(required %in% callback_formals) && !("..." %in% callback_formals)) {
    stop(sprintf("%s must accept %s (or `...`).", name,
      paste(required, collapse = ", ")), call. = FALSE)
  }
  invisible(callback)
}

.study_validate_metadata <- function(metadata) {
  if (!is.list(metadata) || is.object(metadata) || is.null(names(metadata)) || !length(metadata) ||
      anyNA(names(metadata)) || any(!nzchar(names(metadata))) ||
      anyDuplicated(names(metadata))) {
    stop("metadata must be a non-empty named list of plain values.", call. = FALSE)
  }
  if (!all(vapply(metadata, .study_plain_value, logical(1)))) {
    stop("metadata must contain only plain atomic values or named lists of plain values.",
      call. = FALSE)
  }
  for (field in c("description", "analysis_scope")) {
    value <- metadata[[field]]
    if (is.null(value) || !is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(trimws(value))) {
      stop(sprintf("metadata$%s must be a non-empty character scalar.", field),
        call. = FALSE)
    }
  }
  invisible(metadata)
}

#' Define a callback-driven simulation study
#'
#' @param scenarios A non-empty data frame with a unique, non-missing character
#'   `scenario` column. Each row is passed to callbacks as a named list.
#' @param sample_sizes A non-empty vector of distinct positive whole-number
#'   sample sizes.
#' @param generate A function `generate(scenario, n, seed)` returning the
#'   observed analysis data as a data frame.
#' @param truth A deterministic function `truth(scenario, n)` returning a
#'   finite, named numeric vector of independently derived targets. The target
#'   may vary with `n`; it is never passed to `analyze()`.
#' @param analyze A function `analyze(data, scenario, seed)` returning one
#'   estimate/interval status row per declared estimand. See the study
#'   simulation documentation for the complete return contract.
#' @param metadata A named list containing non-empty character scalars
#'   `description` and `analysis_scope`, plus optional assumptions and target
#'   descriptions. Values must be plain atomic vectors or recursively named
#'   lists of plain values.
#'
#' @return An object of class `cssem_study_spec` containing the validated design,
#'   callbacks, sample-size grid, and metadata. Construction does not call any
#'   callbacks.
#' @export
study_spec <- function(scenarios, sample_sizes, generate, truth, analyze, metadata) {
  if (!is.data.frame(scenarios) || nrow(scenarios) < 1L) {
    stop("scenarios must be a non-empty data frame.", call. = FALSE)
  }
  if (!"scenario" %in% names(scenarios)) {
    stop("scenarios must include a `scenario` column.", call. = FALSE)
  }
  scenario_ids <- scenarios$scenario
  if (!is.character(scenario_ids)) {
    stop("scenarios$scenario must be a character column.", call. = FALSE)
  }
  if (anyNA(scenario_ids) || any(!nzchar(trimws(scenario_ids)))) {
    stop("scenarios$scenario cannot contain missing or empty IDs.", call. = FALSE)
  }
  if (anyDuplicated(scenario_ids)) {
    stop("scenarios$scenario IDs must be unique.", call. = FALSE)
  }

  valid_sizes <- is.numeric(sample_sizes) && is.null(dim(sample_sizes)) &&
    length(sample_sizes) > 0L && all(is.finite(sample_sizes)) &&
    all(sample_sizes > 0) && all(sample_sizes == floor(sample_sizes)) &&
    all(sample_sizes <= .Machine$integer.max) && !anyDuplicated(sample_sizes)
  if (!valid_sizes) {
    stop("sample_sizes must be a non-empty vector of distinct positive integers.",
      call. = FALSE)
  }

  .study_validate_callback(generate, "generate", c("scenario", "n", "seed"))
  .study_validate_callback(truth, "truth", c("scenario", "n"))
  .study_validate_callback(analyze, "analyze", c("data", "scenario", "seed"))
  .study_validate_metadata(metadata)

  structure(list(
    scenarios = scenarios,
    sample_sizes = as.integer(sample_sizes),
    generate = generate,
    truth = truth,
    analyze = analyze,
    metadata = metadata
  ), class = "cssem_study_spec")
}
