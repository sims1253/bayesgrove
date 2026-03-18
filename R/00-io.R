# --- Timestamps and Storage Paths ---

#' @keywords internal
bg_now_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' @keywords internal
bg_storage_path <- function(project, ...) {
  file.path(project@path, ".bayesgrove", ...)
}

#' @keywords internal
bg_sort_persisted_value <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(x)
  }

  if (is.list(x)) {
    if (!is.null(names(x))) {
      nm <- names(x)
      ord <- order(nm)
      x <- x[ord]
      names(x) <- nm[ord]
    }

    return(lapply(x, bg_sort_persisted_value))
  }

  x
}

# --- JSON IO Primitives ---

#' @keywords internal
bg_write_json_atomic <- function(path, data, sort_keys = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  payload <- if (isTRUE(sort_keys)) bg_sort_persisted_value(data) else data

  jsonlite::write_json(
    payload,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    force = TRUE
  )

  file.rename(tmp, path)
  invisible(TRUE)
}

#' @keywords internal
bg_with_file_lock <- function(lock_path, code, timeout = 10, poll = 0.05) {
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  acquired <- FALSE
  start <- Sys.time()

  while (!acquired) {
    acquired <- dir.create(lock_path, recursive = FALSE, showWarnings = FALSE)
    if (acquired) {
      break
    }

    if (as.numeric(Sys.time() - start, units = "secs") >= timeout) {
      cli::cli_abort(
        "Timed out waiting for project lock at {.path {lock_path}}."
      )
    }

    Sys.sleep(poll)
  }

  on.exit(unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
  eval.parent(substitute(code))
}

#' @keywords internal
bg_append_jsonl <- function(path, record) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock_path <- paste0(path, ".lock")

  bg_with_file_lock(lock_path, {
    json_line <- jsonlite::toJSON(
      bg_sort_persisted_value(record),
      auto_unbox = TRUE,
      null = "null"
    )
    cat(paste0(json_line, "\n"), file = path, append = TRUE)
    invisible(TRUE)
  })
}
