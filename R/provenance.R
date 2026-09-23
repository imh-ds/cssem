# Shared, serializable analysis provenance. Public records intentionally store
# configuration summaries and row positions, never observation values or names.

.cssem_provenance_classes <- c(
  "fit_states", "cssem_association", "cssem_outer_validation", "cssem_bootstrap",
  "cssem_contrast", "cssem_model_comparison", "cssem_group_comparison",
  "cssem_measurement_invariance", "cssem_measurement_assessment", "cssem_prediction",
  "cssem_prediction_assessment", "cssem_marginal_contrast", "evidence_report",
  "indirect_effect", "causal_effect", "causal_indirect_effect", "conditional_slopes",
  "conditional_indirect_effect"
)

.cssem_provenance_model_specification <- function(model) {
  constructs <- lapply(model$constructs, function(spec) {
    list(indicators = as.character(spec$indicators), scales = as.character(spec$scales),
      keys = as.integer(spec$keys), manifest = isTRUE(spec$manifest),
      reliability = spec$reliability, standardize = spec$standardize)
  })
  list(constructs = constructs, folds = as.integer(model$folds), version = model$version)
}

.cssem_provenance_structure_specification <- function(structure) {
  list(effects = structure$effects, order = structure$order,
    response_families = structure$response_families)
}

.cssem_provenance_association_settings <- function(settings) {
  allowed <- c("folds", "spline_df", "smooth_uncertainty", "shape_stability_min",
    "shape_alpha", "shape_min_gain", "structural_repeats", "seed", "shadow_scope",
    "reliability", "eiv_bootstrap", "respondent_weighting", "preset", "level",
    "missing_policy", "fixed_shapes", "constraints")
  settings[intersect(names(settings), allowed)]
}

.cssem_provenance_call <- function(call, omit = character()) {
  if (!is.call(call)) return(call)
  if (!is.character(omit) || anyNA(omit))
    stop("omit must contain argument names to remove from the provenance call.", call. = FALSE)
  parts <- as.list(call)
  if (length(parts) < 2L) return(call)
  arg_names <- names(parts)
  if (is.null(arg_names)) arg_names <- rep("", length(parts))
  for (i in seq.int(2L, length(parts)))
    if (arg_names[[i]] %in% omit) parts[[i]] <- "<omitted>"
  names(parts) <- arg_names
  as.call(parts)
}

.cssem_provenance_input_summary <- function(data = NULL, retained_rows = NULL,
                                            split_ids = NULL) {
  if (is.null(data)) {
    n_input <- NA_integer_
    columns <- character()
    missing_counts <- integer()
  } else {
    if (!is.data.frame(data)) stop("provenance input must be a data frame or NULL.", call. = FALSE)
    n_input <- as.integer(nrow(data))
    columns <- names(data)
    missing_counts <- vapply(data, function(column) as.integer(sum(is.na(column))), integer(1))
  }
  if (is.null(retained_rows)) {
    row_positions <- if (is.na(n_input)) integer() else seq_len(n_input)
  } else {
    if (!is.numeric(retained_rows) || any(!is.finite(retained_rows)) ||
        any(retained_rows != as.integer(retained_rows)) ||
        (!is.na(n_input) && any(retained_rows < 1L | retained_rows > n_input)))
      stop("retained_rows must contain valid integer input-row positions.", call. = FALSE)
    row_positions <- as.integer(retained_rows)
  }
  if (is.null(split_ids)) split_ids <- list()
  if (is.atomic(split_ids)) split_ids <- list(split = split_ids)
  if (!is.list(split_ids)) stop("split_ids must be an atomic vector, list, or NULL.", call. = FALSE)
  split_ids <- lapply(split_ids, function(ids) {
    if (!is.numeric(ids) || any(!is.finite(ids)) || any(ids != as.integer(ids)))
      stop("split_ids may contain only finite integer row/partition identifiers.", call. = FALSE)
    as.integer(ids)
  })
  list(n_input = n_input,
    n_retained = if (length(row_positions)) as.integer(length(row_positions)) else
      if (is.na(n_input)) NA_integer_ else 0L,
    columns = as.character(columns), missing_counts = missing_counts,
    row_positions = row_positions, split_ids = split_ids)
}

.cssem_package_version <- function(package) {
  version <- tryCatch(as.character(utils::packageVersion(package)), error = function(e) NULL)
  if (!is.null(version) && length(version) == 1L && nzchar(version)) return(version)
  if (identical(package, "cssem")) {
    namespace_version <- tryCatch({
      namespace <- getNamespace("cssem")
      as.character(getNamespaceInfo(namespace, "spec")$version)
    }, error = function(e) NULL)
    if (!is.null(namespace_version) && length(namespace_version) == 1L && nzchar(namespace_version))
      return(namespace_version)
    if (file.exists("DESCRIPTION")) {
      description <- tryCatch(read.dcf("DESCRIPTION", fields = "Version"), error = function(e) NULL)
      if (!is.null(description) && nrow(description)) return(unname(description[1L, 1L]))
    }
  }
  "unavailable"
}

.cssem_provenance_record <- function(operation, call, settings, input = NULL,
                                     parent = list(), packages = character()) {
  if (length(operation) != 1L || !is.character(operation) || is.na(operation) || !nzchar(operation))
    stop("operation must be one non-empty character value.", call. = FALSE)
  if (!is.list(settings)) stop("settings must be a curated list.", call. = FALSE)
  if (!is.list(parent) || (length(parent) && is.null(names(parent))))
    stop("parent must be a named list of provenance records.", call. = FALSE)
  call_text <- if (is.character(call) && length(call) == 1L) call else
    paste(deparse(call, width.cutoff = 500L), collapse = " ")
  if (length(packages) && (!is.character(packages) || anyNA(packages)))
    stop("packages must contain package names.", call. = FALSE)
  packages <- unique(c("cssem", as.character(packages)))
  package_versions <- stats::setNames(lapply(packages, .cssem_package_version), packages)
  if (is.null(input)) input <- .cssem_provenance_input_summary()
  if (!is.list(input)) stop("input must be a provenance input summary or NULL.", call. = FALSE)
  structure(list(schema_version = 1L, operation = operation, call = as.character(call_text),
    settings = settings,
    software = list(R = as.character(getRversion()), cssem = .cssem_package_version("cssem"),
      packages = package_versions),
    input = input, parent = parent), class = "cssem_provenance")
}

#' Retrieve analysis provenance
#'
#' Returns the stored, serializable configuration and input-accounting record
#' for a supported CS-SEM result. Provenance does not include observations,
#' identifying row names, or calling environments. Supported objects created
#' before provenance was added return `NULL`; unsupported classes produce an
#' error listing the classes for which records are defined.
#'
#' @param x A supported CS-SEM analysis result.
#' @return A versioned provenance list, or `NULL` for a supported legacy result.
#' @export
cssem_provenance <- function(x) {
  supported <- intersect(class(x), .cssem_provenance_classes)
  if (!length(supported))
    stop(sprintf("No provenance contract is defined for class(es): %s. The supported result classes include: %s.",
      paste(class(x), collapse = ", "), paste(.cssem_provenance_classes, collapse = ", ")), call. = FALSE)
  record <- x$provenance_record
  if (is.null(record)) return(NULL)
  if (!inherits(record, "cssem_provenance") || !is.list(record) ||
      !identical(record$schema_version, 1L))
    stop("The stored provenance record has an unsupported schema; update cssem before reading it.", call. = FALSE)
  record
}
