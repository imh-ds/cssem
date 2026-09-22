# Unit-aware design metadata and validation helpers.

.resolve_cluster_ids <- function(cluster, input_data, label = "cluster") {
  if (is.null(cluster)) return(NULL)
  if (!is.data.frame(input_data)) stop("input_data must be a data frame.", call. = FALSE)
  if (length(cluster) == 1L && is.character(cluster) && cluster %in% names(input_data)) {
    ids <- input_data[[cluster]]
  } else if (length(cluster) == nrow(input_data)) {
    ids <- cluster
  } else {
    stop(sprintf("%s must be a column name or a vector with one value per input row.", label), call. = FALSE)
  }
  if (is.list(ids)) stop(sprintf("%s labels must be atomic values.", label), call. = FALSE)
  if (anyNA(ids)) stop(sprintf("%s contains missing labels.", label), call. = FALSE)
  if (is.numeric(ids) && any(!is.finite(ids)))
    stop(sprintf("%s contains non-finite labels.", label), call. = FALSE)
  if (!length(ids) && nrow(input_data)) stop(sprintf("%s cannot be empty.", label), call. = FALSE)
  ids
}

.validate_cluster_labels <- function(cluster_ids, n = NULL, label = "cluster") {
  if (is.null(cluster_ids)) return(invisible(NULL))
  if (!is.null(n) && length(cluster_ids) != n)
    stop(sprintf("%s must contain one value per row.", label), call. = FALSE)
  if (is.list(cluster_ids) || anyNA(cluster_ids))
    stop(sprintf("%s contains missing or unsupported labels.", label), call. = FALSE)
  if (is.numeric(cluster_ids) && any(!is.finite(cluster_ids)))
    stop(sprintf("%s contains non-finite labels.", label), call. = FALSE)
  invisible(cluster_ids)
}

.validate_cluster_assignment <- function(cluster_ids, assignment, context = "split") {
  .validate_cluster_labels(cluster_ids, length(assignment))
  if (length(cluster_ids) != length(assignment))
    stop(sprintf("%s must align with the supplied cluster labels.", context), call. = FALSE)
  per_unit <- tapply(as.integer(assignment), as.character(cluster_ids), function(x) length(unique(x)))
  if (any(per_unit != 1L))
    stop(sprintf("Each cluster must occur in exactly one %s fold; cluster leakage was detected.", context), call. = FALSE)
  invisible(TRUE)
}

.cluster_summary <- function(cluster_ids) {
  if (is.null(cluster_ids)) return(data.frame())
  .validate_cluster_labels(cluster_ids)
  units <- unique(cluster_ids)
  counts <- vapply(units, function(unit) sum(cluster_ids == unit), integer(1))
  data.frame(cluster_id = units, n_rows = counts, retained_rows = counts,
    stringsAsFactors = FALSE)
}

.cluster_unit_n <- function(cluster_ids) {
  if (is.null(cluster_ids)) return(NA_integer_)
  as.integer(length(unique(cluster_ids)))
}

.reject_unsupported_design_fields <- function(design = NULL) {
  if (is.null(design)) return(invisible(NULL))
  if (!is.list(design) || is.null(names(design)) || any(!nzchar(names(design))))
    stop("design must be NULL or a named list of design metadata.", call. = FALSE)
  unsupported <- intersect(names(design), c(
    "weights", "weight", "sampling_weights", "survey_weights", "strata", "fpc",
    "finite_population_correction", "replicate_weights", "design_based_se",
    "design_based_standard_errors"
  ))
  if (length(unsupported)) {
    stop(sprintf(
      "Unsupported survey design metadata (%s): survey weights, strata, finite-population corrections, replicate weights, and design-based standard errors are not implemented. respondent_weighting = \"information\" remains posterior-information weighting, not a survey weight.",
      paste(unsupported, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(design)
}
