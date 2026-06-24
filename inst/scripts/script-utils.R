prefer_workspace_library <- function(
  path = "local_r_lib",
  use_env = "CSSEM_USE_LOCAL_R_LIB",
  path_env = "CSSEM_R_LIB_PATH"
) {
  explicit_path <- Sys.getenv(path_env, unset = "")
  if (nzchar(explicit_path)) {
    if (!dir.exists(explicit_path)) {
      stop("Configured library path does not exist: ", explicit_path, call. = FALSE)
    }
    .libPaths(c(normalizePath(explicit_path, winslash = "/", mustWork = TRUE), .libPaths()))
    return(invisible(.libPaths()))
  }

  use_local <- tolower(Sys.getenv(use_env, unset = ""))
  if (!use_local %in% c("1", "true", "yes")) {
    return(invisible(.libPaths()))
  }

  if (!dir.exists(path)) {
    stop("Requested repo-local library does not exist: ", path, call. = FALSE)
  }

  .libPaths(c(normalizePath(path, winslash = "/", mustWork = TRUE), .libPaths()))
  invisible(.libPaths())
}
