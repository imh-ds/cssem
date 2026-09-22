.contrast_tokenize <- function(expression) {
  if (length(expression) != 1L || !is.character(expression) || is.na(expression) || !nzchar(trimws(expression)))
    stop("Each contrast expression must be one non-empty character string.", call. = FALSE)
  chars <- strsplit(expression, "", fixed = TRUE)[[1L]]
  tokens <- list(); i <- 1L; n <- length(chars)
  add <- function(type, value) tokens[[length(tokens) + 1L]] <<- list(type = type, value = value)
  while (i <= n) {
    if (grepl("[[:space:]]", chars[[i]])) { i <- i + 1L; next }
    ch <- chars[[i]]
    if (ch %in% c("+", "-", "*", "/", "(", ")")) {
      if (ch == "-" && i < n && identical(chars[[i + 1L]], ">")) {
        j <- i + 2L
        while (j <= n && !grepl("[[:space:]+*/()]", chars[[j]]) &&
               !(chars[[j]] == "-" && j < n && chars[[j + 1L]] != ">")) j <- j + 1L
        add("id", paste(chars[i:(j - 1L)], collapse = "")); i <- j; next
      }
      add("op", ch); i <- i + 1L; next
    }
    if (grepl("[0-9.]", ch)) {
      text <- paste(chars[i:n], collapse = "")
      match <- regexpr("^[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?|^\\.[0-9]+([eE][+-]?[0-9]+)?", text, perl = TRUE)
      if (match[[1L]] < 1L) stop("Invalid numeric literal in contrast expression.", call. = FALSE)
      value <- substr(text, 1L, attr(match, "match.length"))
      add("number", as.numeric(value)); i <- i + attr(match, "match.length"); next
    }
    if (grepl("[A-Za-z_.]", ch)) {
      j <- i
      while (j <= n && !grepl("[[:space:]+*/()]", chars[[j]])) {
        if (chars[[j]] == "-" && (j == n || chars[[j + 1L]] != ">")) break
        j <- j + 1L
      }
      add("id", paste(chars[i:(j - 1L)], collapse = "")); i <- j; next
    }
    stop(sprintf("Unsupported character `%s` in contrast expression.", ch), call. = FALSE)
  }
  tokens
}

.contrast_parse_expression <- function(expression) {
  tokens <- .contrast_tokenize(expression); position <- 1L
  peek <- function() if (position <= length(tokens)) tokens[[position]] else NULL
  take <- function() { token <- peek(); position <<- position + 1L; token }
  parse_primary <- function() {
    token <- peek()
    if (is.null(token)) stop("Contrast expression ends before a value.", call. = FALSE)
    if (token$type == "number") { take(); return(list(type = "literal", value = token$value)) }
    if (token$type == "id") {
      take()
      if (!is.null(peek()) && identical(peek()$value, "("))
        stop("function calls are not allowed in contrast expressions.", call. = FALSE)
      return(list(type = "id", value = token$value))
    }
    if (identical(token$value, "(")) {
      take(); value <- parse_sum()
      if (is.null(peek()) || !identical(peek()$value, ")"))
        stop("Unclosed parenthesis in contrast expression.", call. = FALSE)
      take(); return(value)
    }
    stop("Expected a parameter_id, number, or parenthesized expression.", call. = FALSE)
  }
  parse_unary <- function() {
    token <- peek()
    if (!is.null(token) && token$type == "op" && token$value %in% c("+", "-")) {
      take(); return(list(type = "unary", operator = token$value, value = parse_unary()))
    }
    parse_primary()
  }
  parse_product <- function() {
    left <- parse_unary()
    repeat {
      token <- peek()
      if (is.null(token) || token$type != "op" || !token$value %in% c("*", "/")) break
      take(); left <- list(type = "binary", operator = token$value, left = left, right = parse_unary())
    }
    left
  }
  parse_sum <- function() {
    left <- parse_product()
    repeat {
      token <- peek()
      if (is.null(token) || token$type != "op" || !token$value %in% c("+", "-")) break
      take(); left <- list(type = "binary", operator = token$value, left = left, right = parse_product())
    }
    left
  }
  ast <- parse_sum()
  if (position <= length(tokens)) {
    if (identical(tokens[[position]]$value, "^")) stop("the `^` operator is not allowed in contrast expressions; multiply terms explicitly.", call. = FALSE)
    stop("Unexpected token in contrast expression.", call. = FALSE)
  }
  refs <- character()
  collect <- function(node) {
    if (identical(node$type, "id")) refs <<- unique(c(refs, node$value))
    if (identical(node$type, "unary")) collect(node$value)
    if (identical(node$type, "binary")) { collect(node$left); collect(node$right) }
  }
  collect(ast)
  list(expression = expression, ast = ast, references = refs)
}

.contrast_eval_ast <- function(node, values) {
  if (identical(node$type, "literal")) return(node$value)
  if (identical(node$type, "id")) return(unname(values[[node$value]]))
  if (identical(node$type, "unary")) {
    value <- .contrast_eval_ast(node$value, values)
    return(if (node$operator == "-") -value else value)
  }
  left <- .contrast_eval_ast(node$left, values); right <- .contrast_eval_ast(node$right, values)
  if (node$operator == "/" && isTRUE(right == 0)) stop("division by zero in contrast expression.", call. = FALSE)
  switch(node$operator, `+` = left + right, `-` = left - right,
    `*` = left * right, `/` = left / right)
}

.contrast_evaluate_table <- function(table, spec) {
  lookup <- .contrast_lookup(table)
  if ("naive_parameter_id" %in% names(table)) for (i in seq_len(nrow(table))) {
    naive <- as.character(table$naive_parameter_id[[i]])
    corrected <- as.character(table$corrected_parameter_id[[i]])
    if (!is.na(naive) && nzchar(naive)) {
      base <- sub(":naive$", "", naive)
      selected <- if (spec$basis == "naive") naive else if (spec$basis == "corrected") corrected else as.character(table$parameter_id[[i]])
      if (!is.na(selected) && nzchar(selected) && selected %in% names(lookup)) lookup[[base]] <- lookup[[selected]]
    }
  }
  referenced <- unique(unlist(lapply(spec$definitions, `[[`, "references"), use.names = FALSE))
  unknown <- setdiff(referenced, names(lookup))
  if (length(unknown)) stop(sprintf("Unknown parameter_id(s): %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  values <- list(); availability <- list()
  for (id in referenced) {
    entry <- lookup[[id]]
    values[[id]] <- if (isTRUE(entry$available)) as.numeric(table[[entry$column]][[entry$row]]) else NA_real_
    availability[[id]] <- list(available = isTRUE(entry$available), reason = entry$reason,
      basis = entry$basis, parameter_id = id)
  }
  estimates <- vapply(spec$definitions, function(definition) {
    tryCatch(as.numeric(.contrast_eval_ast(definition$ast, values)), error = function(e) NA_real_)
  }, numeric(1))
  names(estimates) <- names(spec$definitions)
  reasons <- vapply(spec$definitions, function(definition) {
    missing <- definition$references[!vapply(definition$references, function(id) isTRUE(availability[[id]]$available), logical(1))]
    if (length(missing)) return(paste0("Unavailable parameter_id(s): ", paste(missing, collapse = ", ")))
    value <- tryCatch(.contrast_eval_ast(definition$ast, values), error = function(e) e)
    if (inherits(value, "error")) conditionMessage(value) else ""
  }, character(1))
  list(estimates = estimates, availability = availability,
    expression_status = data.frame(contrast = names(estimates), estimate = unname(estimates),
      available = is.finite(estimates), availability_reason = reasons, stringsAsFactors = FALSE))
}

#' Declare safe arithmetic contrasts over CS-SEM parameter IDs
#'
#' @param definitions Named list of arithmetic expression strings.
#' @param basis Default reporting basis for the contrast.
#' @return An object of class `cssem_contrast_spec`.
#' @export
contrast_spec <- function(definitions, basis = c("auto", "naive", "corrected")) {
  basis <- match.arg(basis)
  if (!is.list(definitions) || !length(definitions) || is.null(names(definitions)) ||
      any(!nzchar(names(definitions))) || anyDuplicated(names(definitions)))
    stop("definitions must be a non-empty named list of contrast expressions.", call. = FALSE)
  parsed <- lapply(definitions, .contrast_parse_expression)
  structure(list(definitions = parsed, basis = basis), class = "cssem_contrast_spec")
}

.contrast_lookup <- function(table) {
  aliases <- attr(table, "parameter_aliases")
  if (is.null(aliases)) aliases <- setNames(lapply(seq_len(nrow(table)), function(i)
    list(row = i, column = "estimate", basis = as.character(table$estimate_basis[[i]]),
      available = isTRUE(table$available[[i]]), reason = as.character(table$availability_reason[[i]]))),
    table$parameter_id)
  aliases
}

#' Evaluate a CS-SEM contrast specification
#'
#' The point estimate is evaluated from the stable parameter table. When
#' `reps` is positive, all referenced terms are refit and evaluated together in
#' one resampling run so their covariance is preserved.
#' @param object A supported CS-SEM result object.
#' @param spec A `cssem_contrast_spec` object.
#' @param reps Number of joint resampling replicates.
#' @param level Confidence level for resampling intervals.
#' @param seed Seed for resampling.
#' @param resample Resampling unit.
#' @param cluster Optional cluster labels.
#' @param selection Shape-selection mode.
#' @return An object of class `cssem_contrast`.
#' @export
contrast <- function(object, spec, reps = 0L, level = .95, seed = 1L,
                     resample = c("row", "cluster"), cluster = NULL,
                     selection = c("fixed", "repeat")) {
  if (!inherits(spec, "cssem_contrast_spec")) stop("spec must be a contrast_spec() object.", call. = FALSE)
  reps <- .bootstrap_scalar_integer(reps, "reps", minimum = 0L)
  if (length(level) != 1L || !is.numeric(level) || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be a number strictly between zero and one.", call. = FALSE)
  seed <- .bootstrap_scalar_integer(seed, "seed", minimum = 0L)
  resample <- match.arg(resample); selection <- match.arg(selection)
  table <- parameter_table(object)
  if (!is.data.frame(table) || !nrow(table)) stop("object does not expose a parameter table.", call. = FALSE)
  evaluated <- .contrast_evaluate_table(table, spec)
  result <- list(spec = spec, parameter_table = table, estimates = evaluated$estimates,
    availability = evaluated$availability, expression_status = evaluated$expression_status,
    basis = spec$basis, selection = selection, reps = reps,
    level = level, seed = seed, resample = resample, cluster = cluster,
    status = if (all(is.finite(evaluated$estimates))) "complete" else "partial")
  if (reps > 0L) {
    if (!inherits(object, "cssem_association"))
      stop("Bootstrap contrasts currently require a cssem_association object.", call. = FALSE)
    if (!any(is.finite(evaluated$estimates))) {
      result$draws <- matrix(NA_real_, nrow = reps, ncol = length(evaluated$estimates),
        dimnames = list(as.character(seq_len(reps)), names(evaluated$estimates)))
      result$replicates <- data.frame(replicate = seq_len(reps), status = "failed",
        failure_reason = "The point contrast is unavailable; no valid bootstrap statistic exists.",
        stringsAsFactors = FALSE)
      result$successful_replicates <- 0L; result$failure_count <- reps
      result$intervals <- data.frame(contrast = names(evaluated$estimates), estimate = unname(evaluated$estimates),
        ci_low = NA_real_, ci_high = NA_real_, level = level, stringsAsFactors = FALSE)
    } else {
      bootstrap_names <- names(evaluated$estimates)[is.finite(evaluated$estimates)]
      boot_fit <- object$fit
      if (is.null(boot_fit$data)) boot_fit$data <- as.data.frame(boot_fit$locked_scores)
      statistic <- function(context) {
        refit <- .bootstrap_association(context, object, selection)
        values <- .contrast_evaluate_table(parameter_table(refit), spec)$estimates
        values <- values[bootstrap_names]
        if (any(!is.finite(values))) stop("A bootstrap contrast replicate produced an unavailable estimate.", call. = FALSE)
        attr(values, "bootstrap_metadata") <- list(shape_signature = paste(vapply(refit$full_models,
          function(model) paste(names(model$shapes), unname(model$shapes), sep = "=", collapse = ";"), character(1)), collapse = "|"))
        values
      }
      bootstrap <- bootstrap_model(boot_fit, statistic, reps = reps, level = level, seed = seed,
        refit = "locked_scores", resample = resample, cluster = cluster)
      result$draws <- matrix(NA_real_, nrow = reps, ncol = length(evaluated$estimates),
        dimnames = list(as.character(seq_len(reps)), names(evaluated$estimates)))
      result$draws[, bootstrap_names] <- bootstrap$draws
      result$replicates <- bootstrap$replicates
      result$successful_replicates <- bootstrap$successful_replicates
      result$failure_count <- bootstrap$failure_count; result$bootstrap <- bootstrap
      result$intervals <- do.call(rbind, lapply(names(evaluated$estimates), function(name) {
        row <- bootstrap$summary[bootstrap$summary$parameter == name, , drop = FALSE]
        if (nrow(row)) data.frame(contrast = name, estimate = evaluated$estimates[[name]],
          ci_low = row$ci_low[[1L]], ci_high = row$ci_high[[1L]], level = level,
          stringsAsFactors = FALSE) else data.frame(contrast = name, estimate = evaluated$estimates[[name]],
          ci_low = NA_real_, ci_high = NA_real_, level = level, stringsAsFactors = FALSE)
      }))
      result$selection_signatures <- if ("metadata" %in% names(bootstrap$replicates))
        vapply(bootstrap$replicates$metadata, function(meta) if (is.null(meta) || length(meta) != 1L || is.na(meta[[1L]])) NA_character_ else meta$shape_signature, character(1)) else rep(NA_character_, nrow(bootstrap$replicates))
      result$selection_changes <- if (selection == "fixed") 0L else {
        reference <- paste(vapply(object$full_models, function(model) paste(names(model$shapes), unname(model$shapes), sep = "=", collapse = ";"), character(1)), collapse = "|")
        sum(!is.na(result$selection_signatures) & result$selection_signatures != reference)
      }
    }
  } else {
    result$draws <- matrix(numeric(), nrow = 0L, ncol = length(evaluated$estimates),
      dimnames = list(character(), names(evaluated$estimates)))
    result$intervals <- data.frame(contrast = names(evaluated$estimates), estimate = unname(evaluated$estimates),
      ci_low = NA_real_, ci_high = NA_real_, level = level, stringsAsFactors = FALSE)
  }
  structure(result, class = c("cssem_contrast", "list"))
}

#' @export
print.cssem_contrast <- function(x, ...) {
  cat("CS-SEM contrast: ", length(x$estimates), " estimand(s); status = ", x$status, "\n", sep = "")
  print(x$expression_status, row.names = FALSE)
  invisible(x)
}
