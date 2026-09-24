# A causal design keeps directed causal arrows separate from observed or
# theoretical associations. The DAG checks can audit declarations against the
# backdoor criterion; they cannot verify substantive assumptions from scores.

.causal_design_assumption_names <- c(
  "consistency", "no_unmeasured_confounding", "positivity",
  "measurement_validity", "temporal_order",
  "no_exposure_induced_mediator_outcome_confounding"
)

.causal_design_names <- function(x, what, allow_empty = FALSE) {
  if (!is.character(x) || (!allow_empty && !length(x)) || anyNA(x) ||
      any(!nzchar(trimws(x))) || anyDuplicated(x))
    stop(what, " must be a ", if (allow_empty) "unique " else "non-empty, unique ",
      "character vector of non-empty names.", call. = FALSE)
  trimws(x)
}

.causal_design_normalize_assumptions <- function(assumptions) {
  if (is.null(assumptions)) assumptions <- list()
  if (!is.list(assumptions) || (length(assumptions) &&
      (is.null(names(assumptions)) || anyNA(names(assumptions)) ||
       any(!nzchar(names(assumptions))) || anyDuplicated(names(assumptions)))))
    stop("assumptions must be a uniquely named list of declared assumption statuses.", call. = FALSE)
  unknown <- setdiff(names(assumptions), .causal_design_assumption_names)
  if (length(unknown)) stop(sprintf("Unknown causal assumption(s): %s.",
    paste(unknown, collapse = ", ")), call. = FALSE)

  status <- stats::setNames(rep("not_assessed", length(.causal_design_assumption_names)),
    .causal_design_assumption_names)
  evidence <- stats::setNames(rep("", length(.causal_design_assumption_names)),
    .causal_design_assumption_names)
  for (name in names(assumptions)) {
    declaration <- assumptions[[name]]
    if (is.list(declaration)) {
      if (is.null(names(declaration)) || !"status" %in% names(declaration) ||
          any(!names(declaration) %in% c("status", "evidence")))
        stop("Each assumption declaration list may contain only status and evidence.", call. = FALSE)
      evidence_value <- declaration$evidence
      if (!is.null(evidence_value) && (!is.character(evidence_value) ||
          length(evidence_value) != 1L || is.na(evidence_value)))
        stop("assumption evidence must be a single character value.", call. = FALSE)
      if (!is.null(evidence_value)) evidence[[name]] <- evidence_value
      declaration <- declaration$status
    }
    if (!is.character(declaration) || length(declaration) != 1L ||
        is.na(declaration) || !declaration %in% c("assumed", "not_assessed", "violated"))
      stop(sprintf("Invalid assumption status for '%s'; use assumed, not_assessed, or violated.", name),
        call. = FALSE)
    status[[name]] <- declaration
  }
  data.frame(assumption = names(status), status = unname(status),
    evidence = unname(evidence), verification = "not_empirically_verified",
    stringsAsFactors = FALSE)
}

.causal_topological_order <- function(nodes, edges) {
  causal <- edges[edges$type == "causal", , drop = FALSE]
  indegree <- stats::setNames(integer(length(nodes)), nodes)
  if (nrow(causal)) for (to in causal$to) indegree[[to]] <- indegree[[to]] + 1L
  available <- nodes[indegree == 0L]
  order <- character(0)
  while (length(available)) {
    node <- available[[1L]]
    available <- available[-1L]
    order <- c(order, node)
    children <- causal$to[causal$from == node]
    for (child in children) {
      indegree[[child]] <- indegree[[child]] - 1L
      if (indegree[[child]] == 0L) available <- c(available, child)
    }
  }
  if (length(order) == length(nodes)) order else NULL
}

.causal_descendants <- function(edges, node) {
  causal <- edges[edges$type == "causal", , drop = FALSE]
  seen <- character(0)
  frontier <- causal$to[causal$from == node]
  while (length(frontier)) {
    current <- frontier[[1L]]
    frontier <- frontier[-1L]
    if (!current %in% seen) {
      seen <- c(seen, current)
      frontier <- c(frontier, causal$to[causal$from == current])
    }
  }
  seen
}

.causal_ancestors <- function(edges, nodes) {
  causal <- edges[edges$type == "causal", , drop = FALSE]
  seen <- unique(nodes)
  frontier <- seen
  while (length(frontier)) {
    current <- frontier[[1L]]
    frontier <- frontier[-1L]
    parents <- causal$from[causal$to == current]
    new <- setdiff(parents, seen)
    seen <- c(seen, new)
    frontier <- c(frontier, new)
  }
  seen
}

.causal_backdoor_separated <- function(design, adjust, treatment = design$treatment,
                                       outcome = design$outcome) {
  # The backdoor graph removes all arrows out of treatment before checking
  # d-separation. Associational links are not interpreted as DAG arrows.
  causal <- design$edges[design$edges$type == "causal" &
    design$edges$from != treatment, , drop = FALSE]
  ancestors <- .causal_ancestors(causal, c(treatment, outcome, adjust))
  ancestral <- causal[causal$from %in% ancestors & causal$to %in% ancestors, , drop = FALSE]

  adjacency <- stats::setNames(vector("list", length(ancestors)), ancestors)
  connect <- function(a, b) {
    if (identical(a, b)) return()
    adjacency[[a]] <<- unique(c(adjacency[[a]], b))
    adjacency[[b]] <<- unique(c(adjacency[[b]], a))
  }
  for (i in seq_len(nrow(ancestral))) connect(ancestral$from[[i]], ancestral$to[[i]])
  if (nrow(ancestral)) for (child in unique(ancestral$to)) {
    parents <- unique(ancestral$from[ancestral$to == child])
    if (length(parents) > 1L) for (i in seq_len(length(parents) - 1L))
      for (j in seq.int(i + 1L, length(parents))) connect(parents[[i]], parents[[j]])
  }

  conditioned <- intersect(adjust, ancestors)
  frontier <- if (treatment %in% conditioned) character(0) else treatment
  visited <- character(0)
  while (length(frontier)) {
    current <- frontier[[1L]]
    frontier <- frontier[-1L]
    if (current %in% visited || current %in% conditioned) next
    visited <- c(visited, current)
    frontier <- c(frontier, setdiff(adjacency[[current]], c(visited, conditioned)))
  }
  !outcome %in% visited
}

# Mediation additionally needs each mediator-outcome relation to be
# unconfounded given the treatment, the adjustment set, and the mediators that
# precede it on a causal path. Mediators are the causal-DAG nodes lying on a
# directed treatment -> outcome path. Returns the mediators with an open
# backdoor path to the outcome.
.causal_mediator_open_backdoors <- function(design, adjust) {
  treatment <- design$treatment; outcome <- design$outcome
  mediators <- setdiff(intersect(.causal_descendants(design$edges, treatment),
    .causal_ancestors(design$edges, outcome)), c(treatment, outcome))
  open <- character(0)
  for (mediator in mediators) {
    upstream <- intersect(setdiff(.causal_ancestors(design$edges, mediator), mediator), mediators)
    conditioned <- unique(c(treatment, adjust, upstream))
    if (!.causal_backdoor_separated(design, conditioned, treatment = mediator, outcome = outcome))
      open <- c(open, mediator)
  }
  open
}

#' Declare a causal graph, adjustment set, and identification assumptions
#'
#' causal_design() stores causal arrows separately from associational links,
#' records the analyst's assumption statuses, and provides the graph against
#' which validate_causal_design() applies the sufficient backdoor criterion.
#' An "assumed" status is a declaration; it is never verified from data.
#'
#' @param edges A data frame with from and to construct names and an optional
#'   type column containing "causal" or "associational". If omitted, type
#'   defaults to "causal". Associational links are displayed in the design but
#'   are not interpreted as directed DAG edges.
#' @param treatment,outcome Single treatment and outcome construct names.
#' @param adjust Character vector of proposed pre-treatment adjustment
#'   constructs. An empty vector is allowed but cannot usually satisfy the
#'   backdoor criterion.
#' @param assumptions Named list of statuses for consistency,
#'   no_unmeasured_confounding, positivity, measurement_validity,
#'   temporal_order, and no_exposure_induced_mediator_outcome_confounding.
#'   Each status is "assumed", "not_assessed", or "violated"; it may also
#'   be a list with status and optional evidence fields. Unspecified
#'   assumptions are recorded as "not_assessed".
#' @param nodes Optional complete vector of construct names. If omitted, names
#'   are inferred from edge endpoints and the treatment, outcome, and
#'   adjustment set.
#' @return An object of class cssem_causal_design containing nodes, typed
#'   edges, assumptions, and a deterministic topological order for causal arrows.
#' @examples
#' edges <- data.frame(from = c("Age", "Age", "Treatment"),
#'   to = c("Treatment", "Outcome", "Outcome"))
#' design <- causal_design(edges, "Treatment", "Outcome", adjust = "Age",
#'   assumptions = list(no_unmeasured_confounding = "assumed"))
#' validate_causal_design(design)
#' @export
causal_design <- function(edges, treatment, outcome, adjust = character(0),
                          assumptions = list(), nodes = NULL) {
  if (!is.data.frame(edges) || !all(c("from", "to") %in% names(edges)))
    stop("edges must be a data frame with from and to columns.", call. = FALSE)
  if (!is.character(treatment) || length(treatment) != 1L || is.na(treatment) || !nzchar(trimws(treatment)))
    stop("treatment must be one non-empty construct name.", call. = FALSE)
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) || !nzchar(trimws(outcome)))
    stop("outcome must be one non-empty construct name.", call. = FALSE)
  treatment <- trimws(treatment)
  outcome <- trimws(outcome)
  if (identical(treatment, outcome)) stop("treatment and outcome must differ.", call. = FALSE)
  if (is.null(adjust)) adjust <- character(0)
  adjust <- .causal_design_names(adjust, "adjust", allow_empty = TRUE)
  if (any(adjust %in% c(treatment, outcome))) stop("adjust must be distinct from treatment and outcome.", call. = FALSE)

  edge_table <- data.frame(from = as.character(edges$from), to = as.character(edges$to),
    stringsAsFactors = FALSE)
  if ("type" %in% names(edges)) edge_table$type <- as.character(edges$type) else edge_table$type <- "causal"
  if (anyNA(edge_table$from) || anyNA(edge_table$to) || any(!nzchar(trimws(edge_table$from))) ||
      any(!nzchar(trimws(edge_table$to))))
    stop("edge endpoints must be non-empty construct names.", call. = FALSE)
  edge_table$from <- trimws(edge_table$from)
  edge_table$to <- trimws(edge_table$to)
  if (any(edge_table$from == edge_table$to)) stop("causal graph edges cannot be self-links.", call. = FALSE)
  if (anyNA(edge_table$type) || any(!edge_table$type %in% c("causal", "associational")))
    stop("edge type must be causal or associational.", call. = FALSE)
  if (anyDuplicated(paste(edge_table$from, edge_table$to, edge_table$type, sep = "\r")))
    stop("edges cannot contain duplicate typed links.", call. = FALSE)

  required_nodes <- unique(c(treatment, outcome, adjust, edge_table$from, edge_table$to))
  if (is.null(nodes)) nodes <- required_nodes else {
    nodes <- .causal_design_names(nodes, "nodes")
    if (any(!required_nodes %in% nodes)) stop("nodes must include the treatment, outcome, adjust set, and every edge endpoint.", call. = FALSE)
  }
  order <- .causal_topological_order(nodes, edge_table)
  if (is.null(order)) stop("causal edges must form an acyclic directed graph (DAG).", call. = FALSE)

  structure(list(nodes = nodes, edges = edge_table, treatment = treatment,
    outcome = outcome, adjust = adjust, assumptions = .causal_design_normalize_assumptions(assumptions),
    topological_order = order), class = "cssem_causal_design")
}

#' Audit a declared causal design against the backdoor criterion
#'
#' Checks that the causal arrows form a DAG, that the proposed adjustment set
#' contains no treatment descendants, and that it d-separates treatment and
#' outcome in the backdoor graph. Declared assumptions remain unverifiable by
#' this function; causal_admissible is true only when those required statuses
#' are all explicitly set to "assumed" as well as the graph checks passing.
#'
#' @param design An object returned by causal_design().
#' @param adjust Optional alternative adjustment set to audit. Defaults to the
#'   set stored in design.
#' @param estimand Either "effect" or "mediation". Mediation also requires
#'   no_exposure_induced_mediator_outcome_confounding to be declared assumed,
#'   and each mediator on a causal treatment -> outcome path to be d-separated
#'   from the outcome given the treatment, the adjustment set, and upstream
#'   mediators.
#' @return An object of class causal_design_audit with graph checks, assumption
#'   states, and separate valid and causal_admissible results.
#' @examples
#' audit <- validate_causal_design(design)
#' audit$checks
#' @export
validate_causal_design <- function(design, adjust = NULL, estimand = c("effect", "mediation")) {
  estimand <- match.arg(estimand)
  if (!inherits(design, "cssem_causal_design")) stop("design must be a cssem_causal_design.", call. = FALSE)
  if (is.null(adjust)) adjust <- design$adjust
  adjust <- .causal_design_names(adjust, "adjust", allow_empty = TRUE)
  if (any(adjust %in% c(design$treatment, design$outcome)) || any(!adjust %in% design$nodes))
    stop("adjust must contain only design nodes other than treatment and outcome.", call. = FALSE)

  descendants <- .causal_descendants(design$edges, design$treatment)
  no_post_treatment <- !any(adjust %in% descendants)
  backdoor_blocked <- .causal_backdoor_separated(design, adjust)
  assumptions <- design$assumptions
  required_assumptions <- c("consistency", "no_unmeasured_confounding", "positivity",
    "measurement_validity", "temporal_order")
  if (identical(estimand, "mediation"))
    required_assumptions <- c(required_assumptions, "no_exposure_induced_mediator_outcome_confounding")
  assumption_rows <- assumptions[match(required_assumptions, assumptions$assumption), , drop = FALSE]
  assumption_rows$required_for <- estimand
  assumption_rows$passed <- assumption_rows$status == "assumed"
  mediation <- identical(estimand, "mediation")
  open_mediators <- if (mediation) .causal_mediator_open_backdoors(design, adjust) else character(0)
  mediator_blocked <- !length(open_mediators)

  checks <- data.frame(
    check = c("acyclic_causal_graph", "no_post_treatment_adjustment", "backdoor_adjustment",
      if (mediation) "mediator_outcome_backdoor",
      paste0(required_assumptions, "_assumption")),
    passed = c(TRUE, no_post_treatment, backdoor_blocked, if (mediation) mediator_blocked,
      assumption_rows$passed),
    details = c("Causal edges are acyclic; associational links are excluded from DAG traversal.",
      if (no_post_treatment) "No adjusted construct is a descendant of treatment." else
        sprintf("Post-treatment adjustment detected: %s.", paste(intersect(adjust, descendants), collapse = ", ")),
      if (backdoor_blocked) "The adjustment set d-separates treatment and outcome in the backdoor graph." else
        "An open backdoor path remains under the declared causal DAG.",
      if (mediation) {
        if (mediator_blocked) "Every mediator-outcome relation is d-separated given treatment, adjustment, and upstream mediators." else
          sprintf("Open mediator-outcome backdoor path under the declared causal DAG for: %s.",
            paste(open_mediators, collapse = ", "))
      },
      vapply(seq_len(nrow(assumption_rows)), function(i) {
        status <- assumption_rows$status[[i]]
        if (status == "assumed") "Declared assumed; not verified by data." else
          paste0("Not admissible: status is ", status, ".")
      }, character(1))),
    stringsAsFactors = FALSE)
  valid <- checks$passed[checks$check == "acyclic_causal_graph"]
  adjustment_valid <- no_post_treatment && backdoor_blocked && mediator_blocked
  structure(list(valid = valid, causal_admissible = valid && adjustment_valid && all(assumption_rows$passed),
    adjustment_valid = adjustment_valid,
    estimand = estimand, treatment = design$treatment, outcome = design$outcome,
    adjust = adjust, graph_semantics = "causal_dag_with_separate_associations",
    checks = checks, assumptions = assumption_rows,
    topological_order = design$topological_order), class = "causal_design_audit")
}

#' @export
print.cssem_causal_design <- function(x, ...) {
  cat(sprintf("CS-SEM causal design: %s -> %s\n", x$treatment, x$outcome))
  cat(sprintf("  nodes: %d; causal edges: %d; associational links: %d\n",
    length(x$nodes), sum(x$edges$type == "causal"), sum(x$edges$type == "associational")))
  cat(sprintf("  adjustment set: %s\n", if (length(x$adjust)) paste(x$adjust, collapse = ", ") else "(none)"))
  cat("  assumptions are declarations and are not verified from data.\n")
  invisible(x)
}

#' @export
print.causal_design_audit <- function(x, ...) {
  cat("CS-SEM causal design audit\n")
  cat(sprintf("  graph valid: %s; causal admissibility under declared assumptions: %s\n",
    x$valid, x$causal_admissible))
  for (i in seq_len(nrow(x$checks)))
    cat(sprintf("  %-48s %s\n", x$checks$check[[i]], if (x$checks$passed[[i]]) "pass" else "fail"))
  cat("  Assumed statuses remain unverifiable without substantive evidence.\n")
  invisible(x)
}
