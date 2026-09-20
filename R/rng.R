# Deterministic seeding is deliberate in this package: a fit, an association, or
# a bootstrap must reproduce exactly from its `seed` argument. Consuming the
# caller's random stream is not deliberate, and it is surprising -- a user who
# draws random numbers before and after fitting a model should get the sequence
# they would have got without the model.
#
# Each exported entry point that seeds calls .preserve_seed() first, which
# registers an on.exit() in that function's own frame restoring the stream that
# was there on entry (or removing the one we created, when the session had not
# drawn a random number yet).
.preserve_seed <- function(envir = parent.frame()) {
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    do.call(on.exit, list(
      quote(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())),
      add = TRUE, after = FALSE), envir = envir)
    return(invisible(NULL))
  }
  state <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  do.call(on.exit, list(
    bquote(assign(".Random.seed", .(state), envir = globalenv())),
    add = TRUE, after = FALSE), envir = envir)
  invisible(NULL)
}
