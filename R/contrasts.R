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
#' Point evaluation is available in Task 1. Resampling arguments are accepted
#' for the joint contrast implementation and are validated there.
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
  table <- parameter_table(object)
  if (!is.data.frame(table) || !nrow(table)) stop("object does not expose a parameter table.", call. = FALSE)
  lookup <- .contrast_lookup(table)
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
  structure(list(spec = spec, parameter_table = table, estimates = estimates,
    availability = availability, expression_status = data.frame(
      contrast = names(estimates), estimate = unname(estimates), available = is.finite(estimates),
      availability_reason = reasons, stringsAsFactors = FALSE),
    basis = spec$basis, selection = match.arg(selection), reps = as.integer(reps),
    level = level, seed = seed, resample = match.arg(resample), cluster = cluster,
    status = if (all(is.finite(estimates))) "complete" else "partial"),
    class = c("cssem_contrast", "list"))
}

#' @export
print.cssem_contrast <- function(x, ...) {
  cat("CS-SEM contrast: ", length(x$estimates), " estimand(s); status = ", x$status, "\n", sep = "")
  print(x$expression_status, row.names = FALSE)
  invisible(x)
}
