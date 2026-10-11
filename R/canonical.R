# Canonical JSON and digests ------------------------------------------------
#
# One serialization for every content digest that must be stable across
# sessions and machines: named lists are sorted by name at every depth, scalars
# are unboxed, and doubles keep full precision. Fingerprints predate this
# helper and keep their own format.

#' @keywords internal
#' @noRd
bg_canonicalize <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      x <- x[order(names(x), method = "radix")]
    }
    return(lapply(x, bg_canonicalize))
  }
  x
}

#' @keywords internal
#' @noRd
bg_canonical_json <- function(x) {
  as.character(jsonlite::toJSON(
    bg_canonicalize(x),
    auto_unbox = TRUE,
    null = "null",
    digits = I(17)
  ))
}

#' @keywords internal
#' @noRd
bg_canonical_digest <- function(x) {
  digest::digest(bg_canonical_json(x), algo = "sha256", serialize = FALSE)
}
