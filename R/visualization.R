#' Extract data used by CS-SEM plots
#'
#' Returns the data behind a package plot so callers can customize it or use a
#' different graphics system. The returned tables retain effect basis,
#' interval availability, and routing status where those apply. Base graphics
#' methods can be exported with standard R devices such as grDevices::pdf().
#'
#' @param x A CS-SEM result object with a plot_data() method.
#' @param ... Arguments passed to a class-specific method.
#' @return A data frame. Path diagrams also attach a nodes data frame as an
#'   attribute; unavailable intervals remain NA and are never imputed.
#' @examples
#' # path_data <- plot_data(association, type = "paths")
#' # curve_data <- plot_data(association, type = "curve", outcome = "Y", predictor = "X")
#' @export
plot_data <- function(x, ...) UseMethod("plot_data")

#' @export
plot_data.default <- function(x, ...) {
  stop("plot_data() is not defined for objects of class ", paste(class(x), collapse = "/"), ".", call. = FALSE)
}

.plot_topological_ranks <- function(nodes, edges, order = NULL) {
  ranks <- stats::setNames(rep(0, length(nodes)), nodes)
  interaction_nodes <- nodes[grepl(":", nodes, fixed = TRUE)]
  constructs <- setdiff(nodes, interaction_nodes)
  if (!is.null(order) && all(constructs %in% order)) {
    ranks[constructs] <- match(constructs, order)
    for (term in interaction_nodes) {
      components <- strsplit(term, ":", fixed = TRUE)[[1L]]
      if (all(components %in% constructs)) ranks[[term]] <- max(ranks[components]) + .5
    }
    return(ranks)
  }
  expanded <- list()
  for (i in seq_len(nrow(edges))) {
    from <- edges$from[[i]]; to <- edges$to[[i]]
    components <- strsplit(from, ":", fixed = TRUE)[[1L]]
    if (from %in% interaction_nodes && all(components %in% constructs)) {
      expanded[[length(expanded) + 1L]] <- data.frame(from = from, to = to)
      for (component in components)
        expanded[[length(expanded) + 1L]] <- data.frame(from = component, to = from)
    } else expanded[[length(expanded) + 1L]] <- data.frame(from = from, to = to)
  }
  expanded <- do.call(rbind, expanded)
  for (iteration in seq_len(max(1L, length(nodes)))) {
    changed <- FALSE
    for (i in seq_len(nrow(expanded))) {
      from <- expanded$from[[i]]; to <- expanded$to[[i]]
      if (from %in% nodes && to %in% nodes && ranks[[to]] <= ranks[[from]]) {
        ranks[[to]] <- ranks[[from]] + 1
        changed <- TRUE
      }
    }
    if (!changed) break
    if (iteration == length(nodes))
      stop("Cannot lay out a structural path diagram for a cyclic specification.", call. = FALSE)
  }
  ranks
}

.plot_node_layout <- function(nodes, ranks, labels = nodes) {
  unique_ranks <- sort(unique(unname(ranks)))
  x <- if (length(unique_ranks) < 2L) stats::setNames(rep(.5, length(nodes)), nodes) else
    stats::setNames((ranks[nodes] - min(unique_ranks)) / diff(range(unique_ranks)), nodes)
  y <- stats::setNames(numeric(length(nodes)), nodes)
  for (stage in unique_ranks) {
    members <- nodes[ranks[nodes] == stage]
    y[members] <- if (length(members) == 1L) .5 else seq(.85, .15, length.out = length(members))
  }
  data.frame(id = nodes, label = unname(labels[nodes]), x = unname(x[nodes]),
    y = unname(y[nodes]), node_type = ifelse(grepl(":", nodes, fixed = TRUE), "interaction_term", "construct"),
    stringsAsFactors = FALSE)
}

.plot_association_edges <- function(association, routing = NULL) {
  if (!inherits(association, "cssem_association")) stop("x must be a cssem_association.", call. = FALSE)
  if (!is.null(routing) && !inherits(routing, "cssem_routing")) stop("routing must be a cssem_routing.", call. = FALSE)
  if (is.null(association$scores) || is.null(association$structure)) stop("association must carry locked scores and a structural specification.", call. = FALSE)
  declared <- .declared_edges(association$structure)
  nodes <- names(association$scores)
  interaction_terms <- unique(declared$from[grepl(":", declared$from, fixed = TRUE)])
  interaction_terms <- interaction_terms[vapply(interaction_terms, function(term)
    all(strsplit(term, ":", fixed = TRUE)[[1L]] %in% nodes), logical(1))]
  layout_nodes <- c(nodes, interaction_terms)
  ranks <- .plot_topological_ranks(layout_nodes, declared, association$structure$order)
  node_labels <- stats::setNames(nodes, nodes)
  if (length(interaction_terms)) {
    for (term in interaction_terms) {
      components <- strsplit(term, ":", fixed = TRUE)[[1L]]
      if (!all(components %in% nodes)) next
      ranks[[term]] <- max(ranks[components]) + .5
      node_labels[[term]] <- paste(components, collapse = " x ")
    }
  }
  layout <- .plot_node_layout(names(ranks), ranks, node_labels)
  rows <- list()
  param <- parameter_table(association)
  for (i in seq_len(nrow(declared))) {
    from <- declared$from[[i]]; to <- declared$to[[i]]
    pieces <- strsplit(from, ":", fixed = TRUE)[[1L]]
    is_interaction <- length(pieces) > 1L && all(pieces %in% nodes) && from %in% layout$id
    if (is_interaction) {
      for (piece in pieces) {
        origin <- layout[layout$id == piece, , drop = FALSE]
        destination <- layout[layout$id == from, , drop = FALSE]
        rows[[length(rows) + 1L]] <- data.frame(from = piece, to = from,
          x = origin$x, y = origin$y, xend = destination$x, yend = destination$y,
          edge_type = "interaction_input", status = "derived_term", interpretation = "Product-term construction; not a structural or causal effect.",
          estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, basis = "not_applicable",
          units = "", shape = "product", interval_basis = "unavailable", estimand = NA_character_,
          adjustment_set = NA_character_, label = "product term", display_label = "product term",
          stringsAsFactors = FALSE)
      }
    }
    key <- .path_key(from, to)
    p <- param[param$predictor == from & param$outcome == to, , drop = FALSE]
    estimate <- if (nrow(p)) p$estimate[[1L]] else NA_real_
    low <- if (nrow(p)) p$ci_low[[1L]] else NA_real_
    high <- if (nrow(p)) p$ci_high[[1L]] else NA_real_
    basis <- if (nrow(p) && !is.na(p$estimate_basis[[1L]])) p$estimate_basis[[1L]] else "unavailable"
    units <- if (nrow(p) && !is.na(p$units[[1L]])) p$units[[1L]] else ""
    shape <- if (nrow(p) && !is.na(p$shape[[1L]])) p$shape[[1L]] else "unspecified"
    status <- "associational"
    interpretation <- "Declared structural direction; association only, not a causal claim."
    estimand <- adjustment <- NA_character_
    if (!is.null(routing)) {
      route_row <- routing$table[match(key, routing$table$path), , drop = FALSE]
      if (nrow(route_row) && !is.na(route_row$status[[1L]])) {
        status <- route_row$status[[1L]]
        interpretation <- route_row$interpretation[[1L]]
        estimand <- route_row$estimand[[1L]]
        adjustment <- route_row$adjustment_set[[1L]]
        if (status %in% c("causal", "causal_weak")) {
          estimate <- route_row$effect[[1L]]
          low <- route_row$ci_low[[1L]]
          high <- route_row$ci_high[[1L]]
          routed_effect <- routing$causal_effects[[key]]
          basis <- if (is.null(routed_effect)) "routed_basis_unavailable" else if (isTRUE(routed_effect$disattenuated))
            "corrected_eiv" else if (routed_effect$estimand %in% c("adjusted_dml", "adjusted_ame"))
              "flexible_adjusted_no_eiv" else "naive_adjusted"
        }
      }
    }
    start <- layout[layout$id == from, , drop = FALSE]
    finish <- layout[layout$id == to, , drop = FALSE]
    has_ci <- is.finite(low) && is.finite(high)
    effect_label <- if (is.finite(estimate)) {
      value <- if (has_ci) sprintf("%.2f [%.2f, %.2f]", estimate, low, high) else sprintf("%.2f", estimate)
      paste(value, basis, sep = "\n")
    } else paste(shape, basis, sep = "\n")
    interval_basis <- if (!has_ci) "unavailable" else if (status %in% c("causal", "causal_weak")) {
      routed_effect <- routing$causal_effects[[key]]
      if (!is.null(routed_effect) && isTRUE(routed_effect$bootstrap > 0L)) "percentile_bootstrap" else "routed_interval"
    } else if (nrow(p) && "uncertainty_method" %in% names(p) && !is.na(p$uncertainty_method[[1L]])) {
      p$uncertainty_method[[1L]]
    } else "available_interval"
    label_parts <- c(effect_label, units, status)
    if (!is.na(estimand)) label_parts <- c(label_parts, paste("estimand:", estimand))
    if (!is.na(adjustment) && nzchar(adjustment)) label_parts <- c(label_parts, paste("adjusted for:", adjustment))
    if (has_ci) label_parts <- c(label_parts, paste("interval:", interval_basis))
    display_label <- paste(effect_label, units, status, sep = "\n")
    rows[[length(rows) + 1L]] <- data.frame(from = from, to = to,
      x = start$x, y = start$y, xend = finish$x, yend = finish$y,
      edge_type = "structural", status = status, interpretation = interpretation,
      estimate = estimate, ci_low = low, ci_high = high, basis = basis,
      units = units, shape = shape,
      interval_basis = interval_basis,
      estimand = estimand, adjustment_set = adjustment,
      label = paste(label_parts, collapse = "\n"), display_label = display_label,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  attr(out, "nodes") <- layout
  attr(out, "causal_warning") <- "Arrows show declared structural direction. Associational edges are not causal claims; causal readings require routed status and assumptions."
  out
}

#' @export
plot_data.cssem_association <- function(x, type = c("paths", "curve"), routing = NULL,
                                        outcome = NULL, predictor = NULL, n = 101L, at = NULL, ...) {
  type <- match.arg(type)
  if (type == "paths") return(.plot_association_edges(x, routing = routing))
  if (!inherits(x, "cssem_association")) stop("x must be a cssem_association.", call. = FALSE)
  if (length(outcome) != 1L || is.na(outcome) || !outcome %in% names(x$full_models))
    stop("outcome must identify a declared endogenous construct.", call. = FALSE)
  if (length(predictor) != 1L || is.na(predictor) || !predictor %in% names(x$full_models[[outcome]]$shapes) ||
      grepl(":", predictor, fixed = TRUE))
    stop("predictor must identify a selected structural edge with a single construct predictor.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2L || n != as.integer(n))
    stop("n must be an integer of at least 2.", call. = FALSE)
  n <- as.integer(n)
  scores <- x$scores
  observed <- scores[[predictor]]
  support <- range(observed[is.finite(observed)])
  if (!all(is.finite(support)) || support[[1L]] >= support[[2L]])
    stop("predictor needs at least two distinct finite locked scores to draw a response curve.", call. = FALSE)
  model <- x$full_models[[outcome]]
  score_names <- names(scores)
  held <- vapply(scores, function(value) mean(value[is.finite(value)], na.rm = TRUE), numeric(1))
  if (is.null(at)) at <- numeric()
  if (is.list(at)) at <- unlist(at, use.names = TRUE)
  if (!is.numeric(at) || (length(at) && (is.null(names(at)) || any(!nzchar(names(at))))) ||
      anyDuplicated(names(at)) || any(!names(at) %in% score_names) || any(!is.finite(at)))
    stop("at must be a named numeric vector or list of finite locked-score values.", call. = FALSE)
  held[names(at)] <- at
  grid <- seq(support[[1L]], support[[2L]], length.out = n)
  new_data <- as.data.frame(as.list(held), check.names = FALSE)
  new_data <- new_data[rep(1L, n), , drop = FALSE]
  new_data[[predictor]] <- grid
  predicted <- .predict_shape_model(model, new_data)
  family <- .structural_family(x$response_families[[outcome]])
  units <- if (family$family == "gaussian") "expected outcome on the locked-score scale" else
    "expected response level on the declared categorical scale"
  shape <- as.character(model$shapes[[predictor]])
  data.frame(predictor = predictor, outcome = outcome, x = grid, predicted = as.numeric(predicted),
    ci_low = NA_real_, ci_high = NA_real_, support_low = support[[1L]], support_high = support[[2L]],
    in_support = grid >= support[[1L]] & grid <= support[[2L]], shape = shape,
    response_family = family$family, basis = "selected_structural_model_prediction", units = units,
    interval_basis = "unavailable", status = "associational", stringsAsFactors = FALSE)
}

.plot_status_style <- function(status) {
  styles <- list(
    associational = c(col = "#315A7D", lty = "solid"),
    predictive = c(col = "#6A4C93", lty = "dashed"),
    representational = c(col = "#666666", lty = "dotted"),
    causal = c(col = "#2E6B3F", lty = "solid"),
    causal_weak = c(col = "#A65E00", lty = "dashed"),
    derived_term = c(col = "#888888", lty = "dotted")
  )
  lapply(status, function(value) if (is.null(styles[[value]])) styles$associational else styles[[value]])
}

#' Plot a CS-SEM structural diagram or response curve
#'
#' @param x A cssem_association from associate().
#' @param type "paths" for declared structural edges or "curve" for one
#'   selected edge's response curve.
#' @param routing Optional cssem_routing from route(); causal status is
#'   shown only when explicitly routed.
#' @param outcome Endogenous construct for a response curve.
#' @param predictor Single-construct predictor for a response curve.
#' @param n Number of points in a response curve.
#' @param at Optional named locked-score values at which to hold other inputs.
#' @param ... Additional arguments passed to base graphics plotting calls.
#' @return x, invisibly. Use plot_data() to customize the plotted values.
#' @export
plot.cssem_association <- function(x, type = c("paths", "curve"), routing = NULL,
                                   outcome = NULL, predictor = NULL, n = 101L, at = NULL, ...) {
  type <- match.arg(type)
  data <- plot_data(x, type = type, routing = routing, outcome = outcome,
    predictor = predictor, n = n, at = at)
  if (type == "curve") {
    graphics::plot(data$x, data$predicted, type = "l", lwd = 2,
      xlab = predictor, ylab = data$units[[1L]],
      main = sprintf("%s response curve (%s; observed support only)", outcome, data$shape[[1L]]), ...)
    graphics::rug(x$scores[[predictor]], col = "#777777", ticksize = .025)
    graphics::mtext(sprintf("Interval basis: %s; status: associational", data$interval_basis[[1L]]), side = 3, line = .25, cex = .75)
  } else {
    nodes <- attr(data, "nodes")
    graphics::plot.new()
    graphics::plot.window(xlim = c(-.08, 1.08), ylim = c(-.08, 1.08))
    graphics::title(main = "Declared structural direction; association is not causation",
      xlab = "Declared model order", ylab = "Construct")
    styles <- .plot_status_style(data$status)
    for (i in seq_len(nrow(data))) {
      style <- styles[[i]]
      graphics::arrows(data$x[[i]], data$y[[i]], data$xend[[i]], data$yend[[i]],
        length = .07, angle = 20, code = 2, col = style[["col"]], lty = style[["lty"]], ...)
      if (data$edge_type[[i]] == "structural") {
        graphics::text((data$x[[i]] + data$xend[[i]]) / 2,
          (data$y[[i]] + data$yend[[i]]) / 2 + .035, labels = data$display_label[[i]],
          cex = .58, col = style[["col"]])
      }
    }
    width <- .07
    for (i in seq_len(nrow(nodes))) {
      graphics::rect(nodes$x[[i]] - width, nodes$y[[i]] - .045,
        nodes$x[[i]] + width, nodes$y[[i]] + .045, col = "white", border = "#333333")
      graphics::text(nodes$x[[i]], nodes$y[[i]], labels = nodes$label[[i]], cex = .78)
    }
    status_levels <- unique(data$status[data$edge_type == "structural"])
    styles <- .plot_status_style(status_levels)
    graphics::legend("bottom", legend = status_levels, col = vapply(styles, function(style) style[["col"]], character(1)),
      lty = vapply(styles, function(style) style[["lty"]], character(1)), horiz = TRUE, bty = "n", cex = .7, xpd = NA)
  }
  invisible(x)
}

#' @export
plot_data.conditional_slopes <- function(x, type = c("slopes", "johnson_neyman"), ...) {
  type <- match.arg(type)
  basis <- if (isTRUE(x$disattenuated)) "disattenuated_eiv" else "naive"
  if (type == "slopes") {
    out <- x$slopes
    out$moderator_sd <- if (length(x$levels) >= nrow(out)) x$levels[seq_len(nrow(out))] else rep(NA_real_, nrow(out))
    if (!"ci_low" %in% names(out)) out$ci_low <- NA_real_
    if (!"ci_high" %in% names(out)) out$ci_high <- NA_real_
    out$component <- "simple_slope"
    out$path <- paste(x$predictor, x$outcome, sep = "\u2192")
    out$basis <- basis
    out$interval_basis <- ifelse(is.finite(out$ci_low) & is.finite(out$ci_high),
      "percentile_bootstrap", "unavailable")
    out$available <- TRUE
    out$status <- "associational"
    out$units <- "outcome locked-score units per predictor locked-score unit"
    return(out)
  }
  jn <- x$johnson_neyman
  if (is.null(jn) || !length(jn$grid)) {
    out <- data.frame(moderator_sd = NA_real_, moderator_value = NA_real_,
      significant = NA, ci_low = NA_real_, ci_high = NA_real_,
      basis = basis, interval_basis = "unavailable", available = FALSE,
      status = "unavailable", stringsAsFactors = FALSE)
    attr(out, "unavailable_reason") <- "Johnson-Neyman intervals require bootstrap uncertainty and johnson_neyman = TRUE."
    return(out)
  }
  association <- x$association
  values <- if (!is.null(jn$moderator_value) && length(jn$moderator_value) == length(jn$grid))
    jn$moderator_value else if (!is.null(association$scores) && x$moderator %in% names(association$scores))
      .moderator_values(association$scores, x$moderator, jn$grid) else jn$grid
  slope <- if (!is.null(jn$slope) && length(jn$slope) == length(jn$grid)) jn$slope else rep(NA_real_, length(jn$grid))
  low <- if (!is.null(jn$ci_low) && length(jn$ci_low) == length(jn$grid)) jn$ci_low else rep(NA_real_, length(jn$grid))
  high <- if (!is.null(jn$ci_high) && length(jn$ci_high) == length(jn$grid)) jn$ci_high else rep(NA_real_, length(jn$grid))
  data.frame(moderator_sd = jn$grid, moderator_value = values,
    slope = slope, significant = as.logical(jn$significant), ci_low = low, ci_high = high,
    basis = basis,
    interval_basis = ifelse(is.finite(low) & is.finite(high), "percentile_bootstrap", "unavailable"),
    available = TRUE, status = "associational", stringsAsFactors = FALSE)
}

#' Plot conditional slopes and Johnson-Neyman results
#'
#' @param x A conditional_slopes object from conditional_slopes().
#' @param type "slopes" for the fitted simple slopes or "johnson_neyman" for
#'   the bootstrap confidence region.
#' @param ... Additional arguments passed to base graphics.
#' @return x, invisibly. Use plot_data() to customize the plotted values.
#' @export
plot.conditional_slopes <- function(x, type = c("slopes", "johnson_neyman"), ...) {
  type <- match.arg(type)
  data <- plot_data(x, type = type)
  if (type == "slopes") {
    graphics::plot(data$moderator_value, data$slope, type = "b", pch = 19,
      xlab = x$moderator, ylab = paste("Slope of", x$predictor, "on", x$outcome),
      main = "Conditional slopes (associational)", ...)
    available <- is.finite(data$ci_low) & is.finite(data$ci_high)
    if (any(available)) graphics::segments(data$moderator_value[available], data$ci_low[available],
      data$moderator_value[available], data$ci_high[available], lwd = 1.5)
    basis_text <- if (any(available)) "95% percentile-bootstrap intervals" else "intervals unavailable"
    graphics::mtext(sprintf("%s; %s", data$basis[[1L]], basis_text), side = 3, line = .25, cex = .75)
  } else if (!isTRUE(data$available[[1L]])) {
    graphics::plot.new()
    graphics::title(main = sprintf("Johnson-Neyman results for %s -> %s", x$predictor, x$outcome))
    graphics::text(.5, .55, labels = attr(data, "unavailable_reason"), cex = .85)
  } else {
    finite_ci <- is.finite(data$ci_low) & is.finite(data$ci_high)
    y_range <- range(c(data$ci_low[finite_ci], data$ci_high[finite_ci],
      data$slope[is.finite(data$slope)], 0), finite = TRUE)
    if (length(y_range) < 2L || !all(is.finite(y_range))) y_range <- c(-1, 1) else if (diff(y_range) == 0)
      y_range <- y_range + c(-.5, .5)
    graphics::plot(data$moderator_value, rep(0, nrow(data)), type = "n",
      ylim = y_range, xlab = x$moderator, ylab = "Conditional slope",
      main = "Johnson-Neyman region (associational)", ...)
    graphics::abline(h = 0, col = "#666666", lty = "dashed")
    if (all(finite_ci)) {
      graphics::polygon(c(data$moderator_value, rev(data$moderator_value)),
        c(data$ci_low, rev(data$ci_high)), border = NA,
        col = grDevices::adjustcolor("#315A7D", alpha.f = .18))
      graphics::lines(data$moderator_value, data$ci_low, col = "#315A7D")
      graphics::lines(data$moderator_value, data$ci_high, col = "#315A7D")
    }
    finite_slope <- is.finite(data$slope)
    if (any(finite_slope)) graphics::lines(data$moderator_value[finite_slope],
      data$slope[finite_slope], col = "#111111", lwd = 1.5)
    graphics::points(data$moderator_value, ifelse(data$significant, y_range[[2L]] * .92, y_range[[1L]] * .92),
      pch = ifelse(data$significant, 19, 1), col = ifelse(data$significant, "#2E6B3F", "#555555"))
    graphics::mtext(sprintf("%s; intervals: %s; filled points mark slopes distinguishable from zero", data$basis[[1L]],
      if (all(finite_ci)) "95% percentile bootstrap" else "unavailable; no confidence band shown"),
      side = 3, line = .25, cex = .75)
  }
  invisible(x)
}

#' @export
plot_data.indirect_effect <- function(x, ...) {
  out <- indirect_effect_ledger(x)
  out$estimate <- out$effect
  out$units <- "outcome locked-score units per predictor contrast"
  out$interval_basis <- ifelse(is.finite(out$ci_low) & is.finite(out$ci_high),
    "percentile_bootstrap", "unavailable")
  out$available <- is.finite(out$estimate)
  out[, c("component", "path", "estimate", "ci_low", "ci_high", "basis",
    "interval_basis", "units", "status", "available")]
}

#' Plot an associational mediation decomposition
#'
#' @param x An indirect_effect object.
#' @param ... Additional arguments passed to base graphics.
#' @return x, invisibly. Use plot_data() to customize the plotted values.
#' @export
plot.indirect_effect <- function(x, ...) {
  data <- plot_data(x)
  display_path <- gsub(.PATH_ARROW, " -> ", data$path, fixed = TRUE)
  labels <- ifelse(is.na(data$path), data$component, paste(data$component, display_path, sep = ": "))
  labels <- paste0(labels, " (", data$basis, ")")
  y <- rev(seq_len(nrow(data)))
  finite <- is.finite(data$estimate)
  x_range <- range(c(data$estimate[finite], data$ci_low[is.finite(data$ci_low)],
    data$ci_high[is.finite(data$ci_high)], 0), finite = TRUE)
  if (!length(x_range) || diff(x_range) == 0) x_range <- x_range + c(-.5, .5)
  graphics::plot(data$estimate, y, xlim = x_range, ylim = c(.5, nrow(data) + .5),
    yaxt = "n", ylab = "", xlab = "Effect (outcome locked-score units per predictor contrast)",
    main = "Mediation decomposition (associational)", pch = ifelse(data$available, 19, 4), ...)
  graphics::axis(2, at = y, labels = labels, las = 1)
  graphics::abline(v = 0, col = "#666666", lty = "dashed")
  interval <- is.finite(data$ci_low) & is.finite(data$ci_high)
  if (any(interval)) graphics::segments(data$ci_low[interval], y[interval],
    data$ci_high[interval], y[interval], lwd = 1.5)
  graphics::mtext("Naive/disattenuated basis is listed in plot_data(); unavailable intervals are omitted.",
    side = 3, line = .25, cex = .75)
  invisible(x)
}

#' @export
plot_data.evidence_report <- function(x, type = c("effects", "constructs", "causal_claims"), ...) {
  type <- match.arg(type)
  if (type == "effects") {
    out <- x$effects
    out$available <- is.finite(out$estimate)
    out$status <- out$causal_status
    if (!"basis" %in% names(out)) out$basis <- "unavailable"
    if (!"units" %in% names(out)) out$units <- ""
    return(out)
  }
  if (type == "constructs") {
    if (is.null(x$constructs)) {
      out <- data.frame(construct = character(), stability = numeric(), held_out_loss = numeric(),
        distinctiveness = numeric(), warnings = integer(), verdict = character())
      attr(out, "unavailable_reason") <- "Construct diagnostics were not included in this evidence report."
      return(out)
    }
    out <- x$constructs
    out$available <- TRUE
    return(out)
  }
  if (is.null(x$causal_claims)) {
    out <- data.frame(claim = character(), type = character(), estimand = character(),
      effect = numeric(), ci_low = numeric(), ci_high = numeric(), verdict = character())
    attr(out, "unavailable_reason") <- "No causal claims were supplied to this evidence report."
    return(out)
  }
  out <- x$causal_claims
  out$available <- is.finite(out$effect)
  out$interval_basis <- ifelse(is.finite(out$ci_low) & is.finite(out$ci_high),
    "routed_or_bootstrap_interval", "unavailable")
  out
}

#' Plot one section of a CS-SEM evidence report
#'
#' @param x An evidence_report object.
#' @param type "effects", "constructs", or "causal_claims".
#' @param ... Additional arguments passed to base graphics.
#' @return x, invisibly. Use plot_data() to customize the plotted values.
#' @export
plot.evidence_report <- function(x, type = c("effects", "constructs", "causal_claims"), ...) {
  type <- match.arg(type)
  data <- plot_data(x, type = type)
  if (!nrow(data)) {
    graphics::plot.new()
    graphics::title(main = paste("Evidence report:", type))
    graphics::text(.5, .55, labels = attr(data, "unavailable_reason"), cex = .85)
    return(invisible(x))
  }
  if (type == "effects") {
    if (!any(is.finite(data$contribution))) {
      graphics::plot.new()
      graphics::title(main = "Structural evidence profile")
      graphics::text(.5, .55, labels = "Predictive contribution is unavailable for the reported model.")
      return(invisible(x))
    }
    y <- rev(seq_len(nrow(data)))
    graphics::plot(data$contribution, y, yaxt = "n", ylab = "",
      xlab = "Cross-validated edge-drop MSE increase",
      main = "Structural evidence profile", pch = 19, ...)
    edge_labels <- paste0(gsub(.PATH_ARROW, " -> ", data$path, fixed = TRUE), " [", data$causal_status, "]")
    graphics::axis(2, at = y, labels = edge_labels, las = 1)
    graphics::points(data$contribution, y, pch = ifelse(data$causal_status == "causal", 19,
      ifelse(data$causal_status == "causal_weak", 1, 17)),
      col = ifelse(data$causal_status == "causal", "#2E6B3F",
        ifelse(data$causal_status == "causal_weak", "#A65E00", "#315A7D")))
    graphics::mtext("Associational effects remain descriptive; causal status appears only when routed.",
      side = 3, line = .25, cex = .75)
  } else if (type == "constructs") {
    y <- rev(seq_len(nrow(data)))
    graphics::plot(data$stability, y, xlim = c(0, 1), yaxt = "n", ylab = "",
      xlab = "Construct-state stability", main = "Measurement convergence and stability", pch = 19, ...)
    graphics::axis(2, at = y, labels = data$construct, las = 1)
  } else {
    y <- rev(seq_len(nrow(data)))
    claim_status <- ifelse(data$label == "causal_under_assumptions", "causal under assumptions", "adjusted association")
    finite_effect <- is.finite(data$effect)
    x_range <- range(c(data$effect[finite_effect], data$ci_low[is.finite(data$ci_low)],
      data$ci_high[is.finite(data$ci_high)], 0), finite = TRUE)
    if (!length(x_range) || diff(x_range) == 0) x_range <- x_range + c(-.5, .5)
    graphics::plot(data$effect, y, xlim = x_range, ylim = c(.5, nrow(data) + .5),
      yaxt = "n", ylab = "", xlab = "Effect",
      main = "Routed effect claims (causal only under assumptions)", pch = 19, ...)
    claim_labels <- paste0(gsub(.PATH_ARROW, " -> ", data$claim, fixed = TRUE), " [", claim_status, "]")
    graphics::axis(2, at = y, labels = claim_labels, las = 1)
    graphics::points(data$effect[finite_effect], y[finite_effect],
      pch = ifelse(data$label[finite_effect] == "causal_under_assumptions", 19, 1),
      col = ifelse(data$label[finite_effect] == "causal_under_assumptions", "#2E6B3F", "#A65E00"))
    interval <- is.finite(data$ci_low) & is.finite(data$ci_high)
    if (any(interval)) graphics::segments(data$ci_low[interval], y[interval],
      data$ci_high[interval], y[interval], lwd = 1.5)
    graphics::abline(v = 0, col = "#666666", lty = "dashed")
  }
  invisible(x)
}

#' @export
plot_data.fit_states <- function(x, type = c("convergence", "scores", "redundancy", "item_loss"), ...) {
  type <- match.arg(type)
  if (type == "convergence") {
    if (is.null(x$measurement_engine) || !length(x$measurement_engine))
      stop("fit_states must carry measurement_engine convergence records.", call. = FALSE)
    rows <- lapply(names(x$measurement_engine), function(construct) {
      engine <- x$measurement_engine[[construct]]
      converged <- engine$converged
      folds <- engine$folds
      folds_converged <- engine$folds_converged
      status <- if (is.na(converged)) "not_applicable" else if (isTRUE(converged)) "converged" else "not_converged"
      data.frame(construct = construct, estimator = as.character(engine$estimator),
        converged = as.logical(converged), iterations = as.integer(engine$iterations),
        folds_converged = as.integer(folds_converged), folds = as.integer(folds),
        fold_rate = if (is.finite(folds) && folds > 0 && is.finite(folds_converged)) folds_converged / folds else NA_real_,
        status = status, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }
  if (type == "scores") {
    if (is.null(x$locked_scores)) stop("fit_states does not carry locked scores.", call. = FALSE)
    scores <- as.data.frame(x$locked_scores)
    return(data.frame(respondent = rep(seq_len(nrow(scores)), times = ncol(scores)),
      construct = rep(names(scores), each = nrow(scores)), score = as.vector(as.matrix(scores)),
      stringsAsFactors = FALSE))
  }
  if (type == "redundancy") {
    if (is.null(x$redundancy)) stop("fit_states does not carry a redundancy matrix.", call. = FALSE)
    table <- as.data.frame(as.table(x$redundancy), stringsAsFactors = FALSE)
    names(table) <- c("construct_from", "construct_to", "correlation")
    table
  } else {
    if (is.null(x$item_metrics)) stop("fit_states does not carry item-loss metrics.", call. = FALSE)
    x$item_metrics
  }
}
